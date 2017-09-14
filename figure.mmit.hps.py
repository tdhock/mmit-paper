"""

"""
import json
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns

from collections import defaultdict
from os import listdir
from os.path import isdir, join


def load_parameters_by_method(predictions_path):
    dataset_params_by_method = defaultdict(lambda: {})
    print "Loading parameters..."
    for method in listdir(predictions_path):
        if isdir(join(predictions_path, method)) and "mmit" in method and "pruning" not in method:
            print "\t", method
            for dataset in listdir(join(predictions_path, method)):
                print "\t\t", dataset
                fold_params = json.load(open(join(predictions_path, method, dataset, "parameters.json"), "r"))
                dataset_params_by_method[dataset][method] = fold_params
    return dataset_params_by_method


if __name__ == "__main__":
    dataset_params_by_method = load_parameters_by_method("./predictions")

    cmap = sns.color_palette("hls", 8)

    clip_to_min = 1e-7

    # ------------------------------------------------------------------------------------------------------------------
    # Selected margin by dataset
    # ------------------------------------------------------------------------------------------------------------------
    plt.clf()
    method_is_plotted = defaultdict(bool)
    dataset_names = sorted(dataset_params_by_method.keys())
    for i, dataset in enumerate(dataset_names):
        for j, (method, fold_parameters) in enumerate(dataset_params_by_method[dataset].iteritems()):
            margins = np.array([f["best"]["margin"] for f in fold_parameters])
            margins[margins == 0] = 1e-7
            offset = 1e-4
            plt.scatter([i + j * offset] * 5, np.log10(margins), edgecolor=cmap[j], facecolor="none",
                        linewidth=1, s=20, alpha=0.7, label=method if not method_is_plotted[method] else None)
            method_is_plotted[method] = True
    plt.axhline(np.log10(clip_to_min), linestyle="--", color="red", label="Zero value")
    plt.xlim([-1, len(dataset_names)])
    plt.xticks(np.arange(len(dataset_names)), dataset_names, rotation=90)
    plt.ylabel(r"$\log_{10}(\epsilon)$")
    plt.title("Selected margin values ({0!s} means 0.)".format(clip_to_min))
    plt.legend(bbox_to_anchor=(1.2, 1.))
    plt.gcf().set_size_inches(12, 4)
    plt.savefig("figure-mmit-hp-margins.pdf", bbox_inches="tight")

    # ------------------------------------------------------------------------------------------------------------------
    # Selected min_samples_split by dataset
    # ------------------------------------------------------------------------------------------------------------------
    plt.clf()
    method_is_plotted = defaultdict(bool)
    dataset_names = sorted(dataset_params_by_method.keys())
    for i, dataset in enumerate(dataset_names):
        for j, (method, fold_parameters) in enumerate(dataset_params_by_method[dataset].iteritems()):
            hp_values = np.array([f["best"]["min_samples_split"] for f in fold_parameters])
            offset = 1e-4
            plt.scatter([i + j * offset] * 5, hp_values, edgecolor=cmap[j], facecolor="none",
                        linewidth=1, s=20, alpha=0.7, label=method if not method_is_plotted[method] else None)
            method_is_plotted[method] = True
    plt.xlim([-1, len(dataset_names)])
    plt.xticks(np.arange(len(dataset_names)), dataset_names, rotation=90)
    plt.ylim(ymin=-5)
    plt.ylabel("Min samples split")
    plt.title("Selected min_samples_split values")
    plt.legend(bbox_to_anchor=(1.2, 1.))
    plt.gcf().set_size_inches(12, 4)
    plt.savefig("figure-mmit-hp-min-samples-split.pdf", bbox_inches="tight")

    # ------------------------------------------------------------------------------------------------------------------
    # Selected max_depth by dataset
    # ------------------------------------------------------------------------------------------------------------------
    plt.clf()
    method_is_plotted = defaultdict(bool)
    dataset_names = sorted(dataset_params_by_method.keys())
    for i, dataset in enumerate(dataset_names):
        for j, (method, fold_parameters) in enumerate(dataset_params_by_method[dataset].iteritems()):
            hp_values = np.array([f["best"]["max_depth"] for f in fold_parameters])
            offset = 1e-4
            plt.scatter([i + j * offset] * 5, hp_values, edgecolor=cmap[j], facecolor="none",
                        linewidth=1, s=20, alpha=0.7, label=method if not method_is_plotted[method] else None)
            method_is_plotted[method] = True
    plt.xlim([-1, len(dataset_names)])
    plt.xticks(np.arange(len(dataset_names)), dataset_names, rotation=90)
    plt.ylim(ymin=-1)
    plt.ylabel("Max depth")
    plt.title("Selected max_depth values")
    plt.legend(bbox_to_anchor=(1.2, 1.))
    plt.gcf().set_size_inches(12, 4)
    plt.savefig("figure-mmit-hp-max-depth.pdf", bbox_inches="tight")
