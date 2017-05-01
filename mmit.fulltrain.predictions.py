"""

"""
import json
import numpy as np
import pandas as pd

from mmit import MaxMarginIntervalTree
from mmit.metrics import mean_squared_error
from mmit.model_selection import GridSearchCV
from os import listdir, mkdir, system
from os.path import abspath, basename, exists, join

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


def find_datasets(path):
    for d in listdir(path):
        if exists(join(path, d, "features.csv")) and \
                exists(join(path, d, "targets.csv")) and \
                exists(join(path, d, "folds.csv")):
            yield Dataset(abspath(join(path, d)))


def mse_scorer(estimator, X, y):
    """
    Negative mean squared error, since GridSearchCV maximizes a metric
    """
    return -mean_squared_error(y_pred=estimator.predict(X), y_true=y)


def save_predictions(estimator, method, dataset, predictions_path):
    open(join(predictions_path, method, dataset.name, "predictions.fulltrain.csv"), "w")\
        .write("pred.log.penalty\n" + "\n".join(str(p) for p in estimator.predict(dataset.X)))
    json.dump(estimator.get_params(), open(join(predictions_path, method, dataset.name, "parameters.fulltrain.json"), "w"))


if __name__ == "__main__":
    predictions_path = "./predictions"
    n_cpu = 4
    param_template = {"max_depth": [1000], "min_samples_split": [0], "margin": [0.] + np.logspace(-4, 1, 5).tolist()}

    datasets = list(find_datasets("./data"))
    datasets = [d for d in datasets if "simulated." in d.name]

    for i, d in enumerate(datasets):
        print "{0:d}/{1:d}: {2!s}".format(i + 1, len(datasets), d.name)

        print ".... linear hinge"
        method = "mmit.linear.hinge.pruning"
        params = dict(param_template)
        params["loss"] = "hinge"
        if not exists(join(predictions_path, method, d.name)) and \
                not exists(join(predictions_path, method, d.name, "predictions.fulltrain.csv")):
            mkdir(join(predictions_path, method, d.name))
            cv = GridSearchCV(estimator=MaxMarginIntervalTree(), param_grid=params, cv=10, n_jobs=n_cpu, scoring=mse_scorer,
                              pruning=True).fit(d.X, d.y)
            save_predictions(cv.best_estimator_, method, d, predictions_path)

        print ".... squared hinge"
        method = "mmit.squared.hinge.pruning"
        params = dict(param_template)
        params["loss"] = "squared_hinge"
        if not exists(join(predictions_path, method, d.name)) and \
                not exists(join(predictions_path, method, d.name, "predictions.fulltrain.csv")):
            mkdir(join(predictions_path, method, d.name))
            cv = GridSearchCV(estimator=MaxMarginIntervalTree(), param_grid=params, cv=10, n_jobs=n_cpu, scoring=mse_scorer,
                              pruning=True).fit(d.X, d.y)
            save_predictions(cv.best_estimator_, method, d, predictions_path)

