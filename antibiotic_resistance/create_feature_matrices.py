"""
Read the set of annotations downloaded for each data set, create a feature
matrix and save it to csv.

"""
import numpy as np
import os
import pandas as pd

from collections import defaultdict

DATASETS_PATH = "./datasets"

def generate_feature_matrix(dataset_path):
    genome_identifiers = np.array([l.strip() for l in open(os.path.join(dataset_path, "genome_ids.csv"), "r")])

    features = defaultdict(list)

    annotation_storage_path = os.path.join(dataset_path, "annotations")

    for i, genome in enumerate(genome_identifiers):
        # print "Genome %d/%d" % (i + 1, len(genome_identifiers))
        genome_features = pd.read_table(os.path.join(annotation_storage_path, genome + ".PATRIC.features.tab"))
        genome_features = genome_features.dropna(subset=["figfam_id"])
        for fid in genome_features.figfam_id:
            features[fid].append(i)

    features_ds = pd.DataFrame(index=genome_identifiers, columns=features.keys(), dtype=np.uint8)
    for i, feature_id in enumerate(features.iterkeys()):
        # print "Feature %d/%d (%s)" % (i + 1, len(features), feature_id)
        genomes = genome_identifiers[features[feature_id.replace("#", "")]]
        features_ds.loc[genomes, feature_id] = 1
    features_ds = features_ds.fillna(0)
    return features_ds.astype(np.uint8, copy=False)


for ds in os.listdir(DATASETS_PATH):
    if not os.path.isdir(os.path.join(DATASETS_PATH, ds)):
        continue
    X = generate_feature_matrix(os.path.join(DATASETS_PATH, ds))
    print ds
    print X.shape
    print "\n" * 2

    X.to_csv(os.path.join(DATASETS_PATH, ds, features.csv), index=False)
