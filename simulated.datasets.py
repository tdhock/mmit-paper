import matplotlib.pyplot as plt
import numpy as np
import os
import seaborn as sns; sns.set_style("white")

from functools import partial


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


def _generate_random_interval_francois(base_y=0., width_std=0.000001, shift_std=0.000001, open_interval_proba=0.3,
                                       n_draws=100, random_state=None):
    # The standard deviation cannot be zero
    width_std = max(width_std, 0.001)
    shift_std = max(shift_std, 0.001)

    if random_state is None:
        random_state = np.random.RandomState()

    # Random sampling from a gaussian until we find a valid interval
    lower = upper = base_y
    while np.isclose(lower, upper):
        draws = random_state.normal(loc=base_y, scale=width_std, size=n_draws)
        lower = min(draws)
        upper = max(draws)

    # Add a random shift to the interval's position
    shift = random_state.normal(loc=0., scale=shift_std)
    lower += shift
    upper += shift

    # Remove an interval bound with some probability
    if random_state.binomial(1, open_interval_proba) == 1:
        if random_state.binomial(1, 0.5) == 1:
            lower = -np.infty
        else:
            upper = np.infty

    return [lower, upper]


def _generate_data(func, n_examples, n_features, interval_width_std, interval_shift_std,
                   open_interval_proba, x_min=0, x_max=10, random_state=None):
    if random_state is None:
        random_state = np.random.RandomState()

    X = random_state.uniform(low=x_min, high=x_max, size=(n_examples, n_features))
    x_signal = X[:, 0]

    base_y = np.array([func(xi) for xi in x_signal])
    y = np.vstack((_generate_random_interval_francois(base_y=yi,
                                                      width_std=interval_width_std,
                                                      shift_std=interval_shift_std,
                                                      open_interval_proba=open_interval_proba,
                                                      n_draws=10,
                                                      random_state=random_state)
                   for yi in base_y))

    # Rescale interval bounds in the 0-1 range
    min_limit = min(yi[0] for yi in y if not np.isinf(yi[0]))
    max_limit = max(yi[1] for yi in y if not np.isinf(yi[1]))
    y = np.array([[(yi[0] - min_limit) / (max_limit - min_limit), (yi[1] - min_limit) / (max_limit - min_limit)]
                  for yi in y])

    fig = plt.figure()
    plt.scatter(X[:, 0], zip(*y)[1], edgecolor="red", facecolor="red", linewidth=1.0, alpha=0.7, label="Upper bound")
    plt.scatter(X[:, 0], zip(*y)[0], edgecolor="blue", facecolor="none", linewidth=1.0, alpha=0.7, label="Lower bound")
    plt.xlabel("Signal feature")
    plt.ylabel("Targets")
    plt.legend()
    plt.tight_layout()
    plt.show()
    return X, y, fig


def generate_function_datasets(datasets, random_seed=42):
    def save_dataset(X, y, plot, n_folds, name):
        folds = np.arange(X.shape[0]) % n_folds + 1
        random_state.shuffle(folds)

        ds_dir = "data/{0!s}".format(name)
        if not os.path.exists(ds_dir):
            os.mkdir(ds_dir)

        header = ",".join("x{0:d}".format(i) for i in xrange(X.shape[1]))
        features = "\n".join(",".join(str(X[i, j]) for j in xrange(X.shape[1])) for i in xrange(X.shape[0]))
        open(os.path.join(ds_dir, "features.csv"), "w").writelines("\n".join([header, features]))
        open(os.path.join(ds_dir, "targets.csv"), "w").writelines(["min.log.penalty, max.log.penalty\n"] +
                                                                  ["{0:.6f}, {1:.6f}\n".format(yi[0], yi[1]) for yi in
                                                                   y])
        open(os.path.join(ds_dir, "folds.csv"), "w").writelines(["fold\n"] + ["{0:d}\n".format(f) for f in folds])
        plot.savefig(os.path.join(ds_dir, "signal.pdf"), bbox_inches="tight")

    random_state = np.random.RandomState(random_seed)
    generate = partial(_generate_data,
                       n_examples=200,
                       n_features=20,
                       interval_width_std=0.3,
                       interval_shift_std=0.2,
                       open_interval_proba=0.2,
                       random_state=random_state)

    for name, func in datasets.iteritems():
        print "Generating", name
        X, y, plot = generate(func)
        save_dataset(X, y, plot, n_folds=5, name=name)


if __name__ == "__main__":
    datasets = {"simulated.sin": lambda x: np.sin(x),
                "simulated.abs": lambda x: np.abs(x - 5.),
                "simulated.linear": lambda x: x / 5}
    generate_function_datasets(datasets, random_seed=42)
