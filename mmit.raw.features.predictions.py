from __future__ import absolute_import, division, print_function, unicode_literals
from builtins import range

import json
import numpy as np
import pandas as pd

from collections import defaultdict
from functools import partial
from joblib import delayed, Parallel
from mmit import MaxMarginIntervalTree
from mmit.core.solver import compute_optimal_costs
from mmit.metrics import mean_squared_error, zero_one_loss
from mmit.model import TreeExporter
from mmit.model_selection import GridSearchCV
from os import listdir, mkdir, system
from os.path import abspath, basename, exists, join
from shutil import rmtree as rmdir
from time import time


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


def keep_only_raw_features(dataset):
    is_raw = [not("log." in name or "log+" in name) for name in dataset.feature_names]
    dataset.feature_names = dataset.feature_names[is_raw]
    dataset.X = dataset.X[:, is_raw]
    return dataset


def find_datasets(path):
    for d in listdir(path):
        if exists(join(path, d, "features.csv")) and \
                exists(join(path, d, "targets.csv")) and \
                exists(join(path, d, "folds.csv")):
            yield Dataset(abspath(join(path, d)))


def evaluate_on_dataset(d, parameters, metric, result_dir, pruning=True, n_margin_values=10, n_cpu=-1):
    ds_result_dir = join(result_dir, d.name)
    if not exists(ds_result_dir):
        mkdir(ds_result_dir)

    ds_uid_file = join(ds_result_dir, "dataset.uid")
    # if exists(ds_uid_file) and open(ds_uid_file, "r").next().strip() == str(hash(d)):
    if not exists(join(ds_result_dir, "predictions.csv")):
        start_time = time()
        fold_predictions = np.zeros(d.n_examples)
        fold_train_mse = []
        fold_cv_results = []
        for i, fold in enumerate(np.unique(d.folds)):
            fold_start = time()

            fold_train = d.folds != fold
            X_train = d.X[fold_train]
            y_train = d.y[fold_train]
            X_test = d.X[~fold_train]
            y_test = d.y[~fold_train]

            # Determine the margin grid
            sorted_limits = y_train.flatten()
            sorted_limits = sorted_limits[~np.isinf(sorted_limits)]
            sorted_limits.sort()
            range_max = sorted_limits.max() - sorted_limits.min()
            range_min = np.diff(sorted_limits)
            range_min = range_min[range_min > 0].min()
            parameters = dict(parameters)  # Make a copy
            parameters["margin"] = np.logspace(np.log10(range_min), np.log10(range_max), n_margin_values)

            cv = GridSearchCV(estimator=MaxMarginIntervalTree(), param_grid=parameters, cv=10, n_jobs=n_cpu,
                              scoring=metric, pruning=pruning)
            cv.fit(X_train, y_train, d.feature_names)
            fold_predictions[~fold_train] = cv.predict(X_test)
            fold_cv_results.append({"best": cv.best_params_, "all": cv.cv_results_})
            fold_train_mse.append(mean_squared_error(y_train, cv.predict(X_train)))
            print("........fold {0:d} took {1:.2} seconds".format(i + 1, time() - fold_start))

            # Save the tree
            latex_exporter = TreeExporter("latex")
            open(join(ds_result_dir, "model_fold_{0:d}.tex".format(i + 1)), "w").write(
                latex_exporter(cv.best_estimator_))

        # Save the predictions
        open(join(ds_result_dir, "predictions.csv"), "w")\
            .write("pred.log.penalty\n" + "\n".join(str(x) for x in fold_predictions))

        # Save the cross-validation results for each fold
        json.dump(fold_cv_results, open(join(ds_result_dir, "parameters.json"), "w"))

        # Generate the PDF file for each tree
        # build_cmd = "cd {0!s}; for i in ./model_fold_*.tex; do lualatex $i > /dev/null; rm ./*.aux ./*.log;done".format(ds_result_dir)
        # !$build_cmd

        # Save a hash of the data to avoid re-running
        open(join(ds_uid_file), "w").write(str(hash(d)))


if __name__ == "__main__":
    n_cpu = 40

    run_algos = [
        "mmit.linear.hinge.raw.features",
        "mmit.linear.hinge.pruning.raw.features",
        "mmit.squared.hinge.raw.features",
        "mmit.squared.hinge.pruning.raw.features"
    ]

    def prep_result_dir(result_dir):
        if not exists(result_dir):
            mkdir(result_dir)

    def mse_metric(estimator, X, y):
        """
        Negative mean squared error, since GridSearchCV maximizes a metric
        """
        return -mean_squared_error(y_pred=estimator.predict(X), y_true=y)

    datasets = list(find_datasets("./data"))
    datasets = [keep_only_raw_features(d) for d in datasets]  # Keep only raw features
    datasets = [d for d in datasets if "H3K36me3" in d.name or
                                       "H3K27ac" in d.name or
                                       "H3K4me3" in d.name or
                                       "blastoma" in d.name]  # Keep only penalty learning data sets

    failed = []
    for method in run_algos:
        print(method)

        # Determine the values of the HPs based on the learning algorithm
        params = {"loss": ["linear_hinge" if method.split(".")[1] == "linear" else "squared_hinge"]}
        if "pruning" in method:
            params.update({"max_depth": [10000000], "min_samples_split": [2]})
            pruning = True
        else:
            params.update({"max_depth": [1, 2, 3, 5, 7, 10, 20, 50, 100, 200, 500, 1000],
                           "min_samples_split": [2, 5, 10, 30, 50, 100, 300, 500]})
            pruning = False

        # Prepare the results directory
        result_dir = "./predictions/{0!s}".format(method)
        prep_result_dir(result_dir)

        # Run on all datasets
        for i, d in enumerate(datasets):
            print("....{0:d}/{1:d}: {2!s}".format(i, len(datasets), d.name))
            try:
                evaluate_on_dataset(d, params, mse_metric, result_dir, pruning, n_margin_values=15, n_cpu=n_cpu)
            except:
                failed.append((method, d.name))

    print("The following datasets failed to run:")
    for method, d_name in failed:
        print(method, d_name)