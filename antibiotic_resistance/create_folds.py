"""
Create outer cross-validation folds for each data set

"""
import numpy as np
import os

DATASETS_PATH = "./datasets"
N_FOLDS = 5
RANDOM_SEED = 42

for ds in os.listdir(DATASETS_PATH):
    if not os.path.isdir(os.path.join(DATASETS_PATH, ds)):
        continue

    folds_path = os.path.join(DATASETS_PATH, ds, "folds.csv")

    n_examples = len([l for l in open(os.path.join(DATASETS_PATH, ds, "genome_ids.csv"), "r")])
    idx = (np.arange(n_examples, dtype=np.uint) % N_FOLDS) + 1
    np.random.RandomState(RANDOM_SEED).shuffle(idx)
    with open(folds_path, "w") as f:
        f.write("fold\n")
        for i in idx:
            f.write(str(i)+ "\n")
