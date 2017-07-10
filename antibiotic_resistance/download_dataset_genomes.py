"""
Download annotations

"""
import os

from patric_tools import genomes


datasets_path = "./datasets"


for ds in os.listdir(datasets_path):
#    if not "streptococcus_pneumoniae__erythromycin" in ds:
#        continue
    if not os.path.isdir(os.path.join(datasets_path, ds)):
        continue

    annotation_path = os.path.join(datasets_path, ds, "annotations")
    if not os.path.exists(annotation_path):
        os.mkdir(annotation_path)

    print "Downloading genome annotations:"
    with open(os.path.join(datasets_path, ds, "genome_ids.csv"), "r") as f:
        for g_id in f:
            g_id = g_id.strip()
            print "... isolate", g_id
            genomes.download_genome_patric_annotations(patric_id=g_id, outdir=annotation_path)
