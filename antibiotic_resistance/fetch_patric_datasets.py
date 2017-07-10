"""
An attempt to get an AMR dataset from PATRIC

"""
import numpy as np
import os
import pandas as pd

from numpy import infty as inf
from patric_tools import amr, genomes
from tempfile import gettempdir

metadata_file = "patric_metadata_07.07.2017.tsv"
EPSILON = 1e-6
DATASET_MIN_EXAMPLES = 100


def _load_metadata(metadata):
    if not os.path.exists(metadata_file):
        amr.get_latest_metadata(".")
    metadata = pd.read_table(metadata_file,
                             converters={'genome_id': str,
                                         'genome_name': lambda x: " ".join(x.lower().split()[:2])})
    metadata = metadata.loc[metadata["laboratory_typing_method"] == "MIC"]
    metadata = _remove_duplicates(metadata)
    return metadata


def _remove_duplicates(metadata):
    """
    Remove duplicates and genomes for which we have more that one measurement

    """
    # Remove any duplicate lines
    metadata = metadata.drop_duplicates(subset=['genome_id', 'antibiotic', 'measurement_sign', 'measurement_value'], keep='first')
    # Remove genomes for which we have multiple different measurements
    metadata = metadata.drop_duplicates(subset=['genome_id', 'antibiotic'], keep=False)
    return metadata


def _filter_by_species_and_antibiotic(metadata, species, antbiotic):
    return metadata.loc[(metadata.genome_name == species) & (metadata.antibiotic == antibiotic)]


metadata = _load_metadata(metadata_file)

usable_datasets = []
for species in metadata.genome_name.unique():
    for antibiotic in metadata.antibiotic.unique():

        metadata_filtered = _filter_by_species_and_antibiotic(metadata, species, antibiotic)
        if metadata_filtered.shape[0] == 0:
            continue

        try:
            metadata_filtered["measurement_value"] = metadata_filtered["measurement_value"].astype(np.float)
        except Exception as e:
            # Some antibiotics are drug combinations and we have a measurement
            # of the form MIC_1/MIC_2. Just ignore those for now.
            continue

        dataset_examples = []

        # Runtime check to make sure that we don't have duplicates
        assert np.unique(metadata_filtered["genome_id"], return_counts=True)[1].max() == 1

        # Create target intervals
        for x in metadata_filtered.iterrows():
            x = x[1]

            if x["measurement_sign"] == "<":
                lower = -inf
                upper = x["measurement_value"] - EPSILON
            elif x["measurement_sign"] == "<=":
                lower = -inf
                upper = x["measurement_value"]
            elif x["measurement_sign"] == ">":
                lower = x["measurement_value"] + EPSILON
                upper = inf
            elif x["measurement_sign"] == ">=":
                lower = x["measurement_value"]
                upper = inf
            else:
                continue  # Unsupported measurement sign (== or none). Skip example.
                # Note: as a convention, we could treat measurement with no signs as ==
                # Maybe I could ask Jim Davis if this is a good assumption.

            dataset_examples.append((x["genome_id"], lower, upper))

        if len(dataset_examples) < DATASET_MIN_EXAMPLES:
            continue
        print species, antibiotic
        usable_datasets.append(dict(species=species, antibiotic=antibiotic,
                                    data=dataset_examples))

# Now we need to choose which dataset to focus on
# TODO: we will select datasets with a good mix of interval types
# TODO: ideally with some diversity in the interval limits
# TODO: make sure each genome is there only once
length_by_dataset = [len(ds["data"]) for ds in usable_datasets]
usable_datasets = np.array(usable_datasets)[np.argsort(length_by_dataset)[::-1]]

# Write a file containing the number of examples per data set
with open("datasets/examples_per_dataset.csv", "w") as f_summary:
    f_summary.write("species,antibiotic,examples,examples_left_censored,examples_right_censored\n")

    for ds in usable_datasets:
        print "Dataset: ", ds["species"], ds["antibiotic"]
        print len(ds["data"])
        left_censored = [(lower, upper) for _, lower, upper in ds["data"] if np.isinf(lower)]
        right_censored = [(lower, upper) for _, lower, upper in ds["data"] if np.isinf(upper)]
        print "[-inf, x]:", len(left_censored), np.unique(["{0:.6f}+{1:.6f}".format(lower, upper) for lower, upper in left_censored], return_counts=True)[1].tolist()
        print "[x, inf]:", len(right_censored), np.unique(["{0:.6f}+{1:.6f}".format(lower, upper) for lower, upper in right_censored], return_counts=True)[1].tolist()
        print np.unique(["{0:.6f}+{0:.6f}".format(lower, upper) for lower, upper in right_censored], return_counts=True)[0].tolist()
        print "\n" * 2

        f_summary.write("{0!s},{1!s},{2:d},{3:d},{4:d}\n".format(ds["species"],
                                                               ds["antibiotic"],
                                                               len(ds["data"]),
                                                               len(left_censored),
                                                               len(right_censored)))

        ds_save_path = "./datasets/{0!s}__{1!s}".format(ds["species"].replace(" ", "_"), ds["antibiotic"].replace(" ", "_").replace("/", "_"))
        if not os.path.exists(ds_save_path):
            os.mkdir(ds_save_path)
        with open(os.path.join(ds_save_path, "targets.csv"), "w") as f_targets:
            f_targets.write("min.log.penalty,max.log.penalty\n")
            with open(os.path.join(ds_save_path, "genome_ids.csv"), "w") as f_gids:
                for g_id, lower, upper in ds["data"]:
                    f_targets.write("{0:.6f},{1:.6f}\n".format(lower, upper))
                    f_gids.write("{0!s}\n".format(g_id))
