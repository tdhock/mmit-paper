from __future__ import absolute_import, division, print_function, unicode_literals
from builtins import range

import json
import numpy as np
import pandas as pd

from mmit import MaxMarginIntervalTree
from mmit.metrics import mean_squared_error, zero_one_loss
from mmit.model import TreeExporter
#from mmit.model_selection import GridSearchCV
from os import listdir, mkdir, system
from os.path import abspath, basename, exists, join
from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn.tree import DecisionTreeRegressor
from time import time


class IntervalDecisionTreeRegressor(DecisionTreeRegressor):
    def __init__(self, margin=0., max_depth=10, min_samples_split=2):
        self.margin = margin
        self.max_depth = max_depth
        self.min_samples_split = min_samples_split
        super(IntervalDecisionTreeRegressor, self).__init__(max_depth=max_depth, min_samples_split=min_samples_split)

    def fit(self, X, y):
        # Explode the intervals into real-valued labels
        X = np.vstack((X, X))
        y = np.hstack(zip(*y))

        # Include the margin
        y[:len(y) / 2] += self.margin  # Lower bounds
        y[len(y) / 2:] -= self.margin  # Upper bounds

        # Remove all infinity bounds
        is_infinite = np.isinf(y)
        X = X[~is_infinite]
        y = y[~is_infinite]

        return super(IntervalDecisionTreeRegressor, self).fit(X, y)


class Dataset(object):
    def __init__(self, path):
        self.path = path
        feature_data = pd.read_csv(join(path, "features.csv"))
        self.X = feature_data.values.astype(np.double)
        self.X.flags.writeable = False
        self.feature_names = feature_data.columns.values
        self.feature_names.flags.writeable = False
        del feature_data
        self.y = pd.read_csv(join(path, "targets.csv")).values.astype(np.double)
        self.y.flags.writeable = False
        self.folds = pd.read_csv(join(path, "folds.csv")).values.reshape(-1, ).astype(np.int)
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


def find_datasets(path):
    for d in listdir(path):
        if exists(join(path, d, "features.csv")) and \
                exists(join(path, d, "targets.csv")) and \
                exists(join(path, d, "folds.csv")):
            yield Dataset(abspath(join(path, d)))


def evaluate_on_dataset(d, parameters, metric, result_dir, n_margin_values=10, n_cpu=-1):
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

            # Fit a regression tree on the transformed data
            cv = GridSearchCV(estimator=IntervalDecisionTreeRegressor(), param_grid=parameters, cv=10, n_jobs=n_cpu,
                              scoring=metric)
            cv.fit(X_train, y_train)

            # Evaluate the model
            fold_predictions[~fold_train] = cv.predict(X_test)
            fold_cv_results.append({"best": cv.best_params_, "all": cv.cv_results_})
            fold_train_mse.append(mean_squared_error(y_train, cv.predict(X_train)))
            print("........fold {0:d} took {1:.2} seconds".format(i + 1, time() - fold_start))

        # Save the predictions
        open(join(ds_result_dir, "predictions.csv"), "w").write("\n".join(str(x) for x in fold_predictions))

        # Save the cross-validation results for each fold
        import cPickle as c
        c.dump(fold_cv_results, open(join(ds_result_dir, "parameters.pkl"), "w"))

        # Save a hash of the data to avoid re-running
        open(join(ds_uid_file), "w").write(str(hash(d)))


if __name__ == "__main__":
    n_cpu = 4

    def prep_result_dir(result_dir):
        if not exists(result_dir):
            mkdir(result_dir)

    def mse_metric(estimator, X, y):
        """
        Negative mean squared error, since GridSearchCV maximizes a metric
        """
        return -mean_squared_error(y_pred=estimator.predict(X), y_true=y)

    datasets = list(find_datasets("./data"))

    params = {"max_depth": [1, 2, 3, 5, 7, 10, 20, 50, 100, 200, 500, 1000],
              "min_samples_split": [2, 5, 10, 30, 50, 100, 300, 500]}

    # Prepare the results directory
    result_dir = "./predictions/cart"
    prep_result_dir(result_dir)

    # Run on all datasets
    for i, d in enumerate(datasets):
        print("....{0:d}/{1:d}: {2!s}".format(i + 1, len(datasets), d.name))
        evaluate_on_dataset(d, params, mse_metric, result_dir, n_margin_values=15, n_cpu=4)
