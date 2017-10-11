"""

"""
import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns

from matplotlib import rc; rc('text', usetex=True)
from mmit.metrics import mean_squared_error
from os.path import basename, join

class Dataset(object):
    def __init__(self, path):
        self.path = path
        feature_data = pd.read_csv(join(path, "features.csv"))
        self.X = feature_data.values.astype(np.float64)
        self.X.flags.writeable = False
        self.feature_names = feature_data.columns.values
        self.feature_names.flags.writeable = False
        del feature_data
        self.y = pd.read_csv(join(path, "targets.csv")).values.astype(np.float64)
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


def configure_pgf_output():
    pgf_with_latex = {  # setup matplotlib to use latex for output
        "pgf.texsystem": "pdflatex",  # change this if using xetex or lautex
        "text.usetex": True,  # use LaTeX to write all text
        "font.family": "serif",
        "font.serif": [],  # blank entries should cause plots to inherit fonts from the document
        "font.sans-serif": [],
        "font.monospace": [],
        "axes.titlesize": 8,
        "axes.labelsize": 8,  # LaTeX default is 10pt font.
        "font.size": 8,
        "legend.fontsize": 8,  # Make the legend/label fonts a little smaller
        "xtick.labelsize": 8,
        "ytick.labelsize": 8,
        "pgf.preamble": [
            r"\usepackage[utf8x]{inputenc}",  # use utf8 fonts becasue your computer can handle it :)
            r"\usepackage[T1]{fontenc}",  # plots will be generated using this preamble
        ]
    }
    mpl.rcParams.update(pgf_with_latex)


def se_mean_squared_error(y_true, y_pred):
    """
    The standard error of the mean squared distance to the interval bounds

    """
    from scipy.stats import sem
    return sem([(p - t[0])**2 if p < t[0] else ((p - t[1])**2 if t[1] < p else 0.0) for p, t in zip(y_pred, y_true)])


if __name__ == "__main__":
    include_datasets = {"simulated.linear": "$f(x) = x/5$",
                        "simulated.abs": "$f(x) = |x|$",
                        "simulated.sin": r"$f(x) = \sin(x)$"}
    include_methods = ["mmit.linear.hinge.pruning", "IntervalRegressionCV"]
    method_pretty_names = ["MMIT", "L1-Linear"]
    method_pretty_names = dict(zip(include_methods, method_pretty_names))

    datasets = [Dataset("./data/{0!s}".format(d)) for d in include_datasets]

    configure_pgf_output()
    sns.set_style("white")
    to_rgba = mpl.colors.colorConverter.to_rgba
    cmap = ['#e41a1c','#377eb8'] #sns.color_palette("hls", len(include_methods))  # ['#66c2a5','#fc8d62','#8da0cb']
    bound_alpha = 0.5
    linewidth = 1.5
    dash_style = [1, 1, 1, 1]

    fig, axes = plt.subplots(ncols=len(datasets))
    fig.set_size_inches(7, 1.75)

    for ax, d in zip(axes, datasets):
        # Plot the interval bounds
        x = d.X[:, 0]
        y = d.y
        bound_color = to_rgba('grey', alpha=bound_alpha)
        ax.scatter(x, zip(*y)[1], edgecolor=bound_color, facecolor=bound_color, linewidth=1.0, s=10,
                   label=r"Upper limit")
        ax.scatter(x, zip(*y)[0], edgecolor=bound_color, facecolor="none", linewidth=1.0, s=10,
                   label=r"Lower limit")

        # Plot the predictions of each method
        x_sorter = np.argsort(x)
        x = x[x_sorter]
        y = y[x_sorter]
        for m_idx, m in enumerate(include_methods):
            p = pd.read_csv("./predictions/{0!s}/{1!s}/predictions.fulltrain.csv".format(m, d.name)).values[x_sorter]

            ax.plot(x, p, label=m, color=cmap[m_idx], linewidth=linewidth, linestyle="-", zorder=3)

            # Show training set MSE
            # if method_pretty_names[m] == "mmit":
            #     ax.text(ax.get_xlim()[0] + 1.7, ax.get_ylim()[0] + 0.2, r"{0!s}: {1:.2f}; {1:.2f}".format(method_pretty_names[m], mean_squared_error(y_true=y, y_pred=p), se_mean_squared_error(y_true=y, y_pred=p)))
            # elif method_pretty_names[m] == "l1-linear":
            #     ax.text(ax.get_xlim()[1] - 6.7, ax.get_ylim()[0] + 0.2, r"{0!s}: {1:.2f}; {1:.2f}".format(method_pretty_names[m], mean_squared_error(y_true=y, y_pred=p), se_mean_squared_error(y_true=y, y_pred=p)))

            # Plot residuals
            # for xi, pi, yi in zip(x, p, y):
            #     if pi < yi[0]:
            #         ax.vlines(xi, pi, yi[0], color=cmap[m_idx], alpha=0.8, linewidth=linewidth, linestyles=[(0, dash_style)])
            #     elif yi[1] < pi:
            #         ax.vlines(xi, yi[1], pi, color=cmap[m_idx], alpha=0.8, linewidth=linewidth, linestyles=[(0, dash_style)])

        #ax.get_xaxis().set_visible(False)
        ax.get_yaxis().set_visible(False)
        ax.get_xaxis().set_ticks([])
        ax.set_xlabel("Signal feature ($x$)")
        ax.set_title(include_datasets[d.name])
        ax.set_xlim(x.min() - 0.5, x.max() + 0.5)
        ax.set_ylim(y[~np.isinf(y)].min() - 0.5, y[~np.isinf(y)].max() + 0.5)

        # Hack to have the residuals in the legend
        #ax.plot(x, p, color="black", linewidth=linewidth * 0.90, linestyle="--", dashes=dash_style, label="Residuals", zorder=0)

        if d.name == "simulated.sin":
            f_x = np.sin(x)
        elif d.name == "simulated.abs":
            f_x = np.abs(x - 5)
        elif d.name == "simulated.linear":
            f_x = x / 5.
        ax.plot(x, f_x, label=r"Function $f(x)$", color="black", linewidth=linewidth, linestyle="-", zorder=1)


    plt.subplots_adjust(wspace=0.00, hspace=0.00)

    # XXX: This needs to be changed if the methods changed. It is used to fine tune legend ordering for the paper.
    labels, handles = plt.gca().get_legend_handles_labels()
    handle_by_label = dict(zip(handles, labels))
    order = ["mmit.linear.hinge.pruning", "IntervalRegressionCV", r"Function $f(x)$", r"Lower limit",
            r"Upper limit"]
    plt.legend([handle_by_label[l] for l in order], [method_pretty_names.get(l, l) for l in order], ncol=5,
              loc='upper center', bbox_to_anchor=(-0.5, 1.28), labelspacing=10)

    plt.savefig("figure.simulated.functions.pdf", bbox_inches="tight")
    plt.savefig("figure.simulated.functions.pgf", bbox_inches="tight")
