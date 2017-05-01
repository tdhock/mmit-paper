"""

"""
import matplotlib.pyplot as plt
import numpy as np
import os

from sklearn.datasets import load_svmlight_file


downloaded_data_path = "./downloaded.datasets"
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


if __name__ == "__main__":
    # Find all libsvm format datasets
    datasets = [f.replace(".libsvm", "") for f in os.listdir(downloaded_data_path) if ".libsvm" in f]

    for d_idx, d_name in enumerate(datasets):
        print "{0:d}/{1:d}: {2!s}".format(d_idx + 1, len(datasets), d_name)
        random_state = np.random.RandomState(42)
        n_folds = 5

        X, y_true = load_svmlight_file(os.path.join(downloaded_data_path, d_name + ".libsvm"))
        X = X.todense()

        # Generate interval target values
        y = np.vstack((_generate_intervals_random_width(n_intervals=1, base_y=yi,
                                                        width_std=float(yi) / 3 if yi > 0 else 1e-1,
                                                        y_shift_std=float(yi) / 10 if yi > 0 else 1e-2,
                                                        open_interval_proba=0.1, random_state=random_state)
                       for yi in y_true))

        sorter = y_true.argsort()
        plt.scatter(np.arange(len(y)), y_true[sorter], color="red", label="True target")
        plt.scatter(np.arange(len(y)), np.array(zip(*y)[1])[sorter], edgecolor="red", facecolor="red", linewidth=1.0,
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
        header = ",".join("x{0:d}".format(i) for i in xrange(X.shape[1]))
        features = "\n".join(",".join(str(X[i, j]) for j in xrange(X.shape[1])) for i in xrange(X.shape[0]))
        open(os.path.join(ds_dir, "features.csv"), "w").writelines("\n".join([header, features]))
        open(os.path.join(ds_dir, "targets.csv"), "w").writelines(["min.log.penalty,max.log.penalty\n"] +
                                                                  ["{0:.6f},{1:.6f}\n".format(yi[0], yi[1]) for yi in
                                                                   y])
        open(os.path.join(ds_dir, "folds.csv"), "w").writelines(["fold\n"] + ["{0:d}\n".format(f) for f in folds])
        plt.savefig(os.path.join(ds_dir, "signal.pdf"), bbox_inches="tight")
