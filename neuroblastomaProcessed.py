import pandas as pd
features=pd.read_csv("neuroblastomaProcessed_features.csv")
targets=pd.read_csv("neuroblastomaProcessed_targets.csv")
from mmit.tree import MaxMarginIntervalTree
model = MaxMarginIntervalTree(
    margin=0.1,
    loss="hinge")
model.fit(features.values, targets.values)
