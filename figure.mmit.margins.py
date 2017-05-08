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
        if isdir(join(predictions_path, method)) and "mmit" in method:
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

    for m in np.logspace(-6, 2, 15):
        plt.axhline(np.log10(m), linestyle="--", color="grey", alpha=0.3)

    plt.xlim([-1, len(dataset_names)])
    plt.xticks(np.arange(len(dataset_names)), dataset_names, rotation=90)
    plt.ylabel(r"$\log_{10}(\epsilon)$")
    plt.title("Selected margin values ({0!s} means 0.)".format(clip_to_min))
    plt.legend(bbox_to_anchor=(1.25, 1.))

    plt.gcf().set_size_inches(12, 4)
    plt.savefig("figure.mmit.margins.pdf", bbox_inches="tight")

    # ------------------------------------------------------------------------------------------------------------------
    # CV score vs margin
    # ------------------------------------------------------------------------------------------------------------------
    for i, dataset in enumerate(dataset_names):
        for j, (method, fold_parameters) in enumerate(dataset_params_by_method[dataset].iteritems()):
            plt.clf()
            for k in xrange(len(fold_parameters)):
                # For each margin value, find the min HPs
                best_score_by_margin = defaultdict(lambda: -np.infty)
                for cv_result in fold_parameters[k]["all"]:
                    hps, scores = cv_result
                    margin = hps["margin"]
                    if scores["cv"] > best_score_by_margin[margin]:
                        best_score_by_margin[margin] = scores["cv"]
                margins = np.sort(best_score_by_margin.keys())
                margins[margins == 0] = clip_to_min

                plt.plot(np.log10(margins), np.log10([best_score_by_margin[m] * -1 for m in margins]))
            plt.title("Method: {0!s}   Dataset: {1!s}".format(method, dataset))
            plt.show()
