"""

"""
import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd

from scipy.io.arff import loadarff
from sklearn.preprocessing import OneHotEncoder
# from sklearn.datasets import load_svmlight_file

downloaded_data_path = "./downloaded.datasets/arff-datasets/regression"
data_path = "./data"


def _generate_intervals_random_width(n_intervals, base_y=0., width_std=0.000001, y_shift_std=0.000001,
                                     open_interval_proba=0.3, random_state=None):
    if random_state is None:
        random_state = np.random.RandomState()

    lower = base_y - np.abs(random_state.normal(loc=0., scale=width_std, size=n_intervals))
    upper = base_y + np.abs(random_state.normal(loc=0., scale=width_std, size=n_intervals))

    shift = random_state.normal(loc=0., scale=y_shift_std, size=n_intervals)
    lower += shift
    upper += shift

    # Randomly make some intervals open
    for idx in np.where(random_state.binomial(1, open_interval_proba, n_intervals) == 1)[0]:
        if random_state.binomial(1, 0.5) == 1:
            lower[idx] = -np.infty
        else:
            upper[idx] = np.infty

    return np.array(zip(lower, upper))


def _replace_non_standard_values(data):
    """
    Replace categorical features by their one-hot encoding

    """
    categorical_idx = []
    X = np.zeros(data.shape, dtype=np.float)
    for i in xrange(X.shape[1]):
        if data[data.columns.values[i]].dtype != np.float64:
            X[:, i] = np.unique(data.iloc[:, i], return_inverse=True)[1]
            categorical_idx.append(i)
        else:
            X[:, i] = data.iloc[:, i]

    # Replace all categorical features by their one hot encoding
    return OneHotEncoder(categorical_features=categorical_idx, sparse=False).fit_transform(X)


if __name__ == "__main__":
    min_examples = 50
    max_examples = 20000

    # Find all libsvm format datasets
    datasets = [f.replace(".arff", "") for f in os.listdir(downloaded_data_path) if ".arff" in f]

    n_datasets_generated = 0
    for d_idx, d_name in enumerate(datasets):
        print "{0:d}/{1:d}: {2!s}".format(d_idx + 1, len(datasets), d_name),
        random_state = np.random.RandomState(42)
        n_folds = 5

        #X, y_true = load_svmlight_file(os.path.join(downloaded_data_path, d_name + ".libsvm"))
        #X = X.todense()

        # Load the data
        data, metadata = loadarff(os.path.join(downloaded_data_path, d_name + ".arff"))
        data = pd.DataFrame(data).dropna()
        if data.shape[0] > max_examples:
            print "Skipped. Too big."
            continue
        elif data.shape[0] < min_examples:
            print "Skipped. Too small."
            continue
        else:
            n_datasets_generated += 1
            print "Ok. Examples: {0:d} Features: {1:d}".format(data.shape[0], data.shape[1])

        # Extract label
        y_true = data.iloc[:, -1].values
        y_name = data.columns.values[-1]
        del data[data.columns.values[-1]]

        # Extract the features
        X_names = data.columns
        X = _replace_non_standard_values(data)

        # Generate interval target values
        y = np.vstack((_generate_intervals_random_width(n_intervals=1, base_y=yi,
                                                        width_std=np.abs(float(yi) / 3) if not np.isclose(yi, 0.) else 1e-1,
                                                        y_shift_std=np.abs(float(yi) / 10) if not np.isclose(yi, 0.) else 1e-2,
                                                        open_interval_proba=0.1, random_state=random_state)
                       for yi in y_true))

        for yl, yu in y:
            assert yl < yu

        sorter = y_true.argsort()
        plt.clf()
        plt.scatter(np.arange(len(y)), y_true[sorter], color="red", label="True target")
        plt.scatter(np.arange(len(y)), np.array(zip(*y)[1])[sorter], edgecolor="green", facecolor="green", linewidth=1.0,
                    alpha=0.7, label="Upper bound")
        plt.scatter(np.arange(len(y)), np.array(zip(*y)[0])[sorter], edgecolor="blue", facecolor="none", linewidth=1.0,
                    alpha=0.7, label="Lower bound")
        plt.xlabel("Example number (ordered by target)")
        plt.ylabel("Targets")

        # Generate folds
        folds = np.arange(X.shape[0]) % n_folds + 1
        random_state.shuffle(folds)

        # Save data
        ds_dir = os.path.join(data_path, d_name)
        if not os.path.exists(ds_dir):
            os.mkdir(ds_dir)
        header = ",".join(X_names)
        features = "\n".join(",".join(str(X[i, j]) for j in xrange(X.shape[1])) for i in xrange(X.shape[0]))
        open(os.path.join(ds_dir, "features.csv"), "w").writelines("\n".join([header, features]))
        open(os.path.join(ds_dir, "targets.csv"), "w").writelines(["min.log.penalty,max.log.penalty\n"] +
                                                                  ["{0:.6f},{1:.6f}\n".format(yi[0], yi[1]) for yi in
                                                                   y])
        open(os.path.join(ds_dir, "folds.csv"), "w").writelines(["fold\n"] + ["{0:d}\n".format(f) for f in folds])
        plt.savefig(os.path.join(ds_dir, "signal.pdf"), bbox_inches="tight")

    print "Generated {0!s} datasets".format(n_datasets_generated)