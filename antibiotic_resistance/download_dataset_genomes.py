"""
Download contigs

"""
import os

from patric_tools import genomes


datasets_path = "./datasets"


for ds in os.listdir(datasets_path):
    if not "streptococcus_pneumoniae__erythromycin" in ds:
        continue

    contig_path = os.path.join(datasets_path, ds, "contigs")
    if not os.path.exists(contig_path):
        os.mkdir(contig_path)

    print "Downloading genome contigs:"
    with open(os.path.join(datasets_path, ds, "genome_ids.csv"), "r") as f:
        for g_id in f:
            g_id = g_id.strip()
            print "... isolate", g_id
            genomes.download_genome_contigs(patric_id=g_id, outdir=contig_path)
