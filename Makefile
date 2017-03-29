penaltyLearning.predictions.RData: penaltyLearning.predictions.R
	R --no-save < $<
figure-data-set-sizes.png: figure-data-set-sizes.R data.sets.RData
	R --no-save < $<
data.sets.RData: data.sets.R
	R --no-save < $<
figure-penaltyLearning.png: figure-penaltyLearning.R
	R --no-save < $<
