HOCKING-notes.pdf: HOCKING-notes.tex figure-algorithm-steps.tex
	pdflatex HOCKING-notes
figure-algorithm-steps.tex: figure-algorithm-steps.R
	R --no-save < $<
figure-evaluate-predictions.png: figure-evaluate-predictions.R evaluate.predictions.RData
	R --no-save < $<
evaluate.predictions.RData: evaluate.predictions.R
	R --no-save < $<
constant.predictions.RData: constant.predictions.R
	R --no-save < $<
penaltyLearning.predictions.RData: penaltyLearning.predictions.R
	R --no-save < $<
figure-data-set-sizes.png: figure-data-set-sizes.R data.sets.RData
	R --no-save < $<
data.sets.RData: data.sets.R
	R --no-save < $<
figure-penaltyLearning.png: figure-penaltyLearning.R
	R --no-save < $<
