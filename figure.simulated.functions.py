"""

"""
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns

from os.path import basename, join

class Dataset(object):
    def __init__(self, path):
        self.path = path
        feature_data = pd.read_csv(join(path, "features.csv"))
        self.X = feature_data.values
        self.X.flags.writeable = False
        self.feature_names = feature_data.columns.values
        self.feature_names.flags.writeable = False
        del feature_data
        self.y = pd.read_csv(join(path, "targets.csv")).values
        self.y.flags.writeable = False
        self.folds = pd.read_csv(join(path, "folds.csv")).values.reshape(-1, )
        self.folds.flags.writeable = False
        self.name = basename(path)

    @property
    def n_examples(self):
        return self.X.shape[0]

    @property
    def n_features(self):
        return self.X.shape[1]

    def __hash__(self):
        return hash((self.X.data, self.y.data, self.feature_names.data, self.folds.data))


if __name__ == "__main__":
    include_datasets = {"simulated.linear": "$f(x) = x/5$",
                        "simulated.log": "$f(x) = log(x+1)$",
                        "simulated.sin": "$f(x) = sin(x)$"}
    include_methods = ["mmit.squared.hinge.pruning", "IntervalRegressionCV"]

    datasets = [Dataset("./data/{0!s}".format(d)) for d in include_datasets]

    to_rgba = matplotlib.colors.colorConverter.to_rgba
    sns.set_style("white")
    cmap = sns.color_palette("hls", len(include_methods))
    bound_alpha = 0.3
    linewidth = 1.2

    fig, axes = plt.subplots(ncols=len(datasets))
    fig.set_size_inches(15, 4.5)

    for ax, d in zip(axes, datasets):
        # Plot the interval bounds
        x = d.X[:, 0]
        y = d.y
        bound_color = to_rgba('grey', alpha=bound_alpha)
        ax.scatter(x, zip(*y)[1], edgecolor=bound_color, facecolor=bound_color, linewidth=0.5, label="Upper bound")
        ax.scatter(x, zip(*y)[0], edgecolor=bound_color, facecolor="none", linewidth=0.5, label="Lower bound")

        # Plot the predictions of each method
        x_sorter = np.argsort(x)
        x = x[x_sorter]
        y = y[x_sorter]
        for m_idx, m in enumerate(include_methods):
            p = pd.read_csv("./predictions/{0!s}/{1!s}/predictions.fulltrain.csv".format(m, d.name)).values[x_sorter]

            ax.plot(x, p, label=m, color=cmap[m_idx], linewidth=linewidth)
            #for xi, pi, yi in zip(x, p, y):
            #    if pi < yi[0]:
            #        ax.vlines(xi, pi, yi[0], color=cmap[m_idx], linestyle="-", alpha=0.5, linewidth=linewidth)
            #    elif yi[1] < pi:
            #        ax.vlines(xi, yi[1], pi, color=cmap[m_idx], linestyle="-", alpha=0.5, linewidth=linewidth)

            #ax.get_xaxis().set_visible(False)
            ax.get_yaxis().set_visible(False)
            ax.get_xaxis().set_ticks([])
            ax.set_xlabel("Signal feature ($x$)")
            ax.set_title(include_datasets[d.name])

    plt.subplots_adjust(wspace=0.01, hspace=0.01)

    plt.legend(ncol=len(datasets) + 2, loc='upper center', bbox_to_anchor=(-0.5, 1.15))
    plt.savefig("figure.simulated.functions.pdf", bbox_inches="tight")