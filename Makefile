figure-margin-complexity-linear.pdf: figure-margin-complexity-linear.R figure.margin.complexity.linear.RData
	R --no-save < $<
margin.complexity.linear.RData: margin.complexity.linear.R
	R --no-save < $<
figure-moves.png: figure-moves.R moves.RData
	R --no-save < $<
moves.RData: moves.R
	R --no-save < $<
HOCKING-notes.pdf: HOCKING-notes.tex figure-algorithm-steps.tex
	pdflatex HOCKING-notes
figure-algorithm-steps.tex: figure-algorithm-steps.R
	R --no-save < $<
figure-evaluate-predictions.png: figure-evaluate-predictions.R evaluate.predictions.RData data.sets.RData
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
trafotree.predictions.RData: trafotree.predictions.R
	R --no-save < $<
# Alex
simulated.datasets: simulated.datasets.py
	python $<
penaltyLearning.fulltrain.predictions: penaltyLearning.fulltrain.predictions.R ./data/*/targets.csv
	R --no-save < $<
mmit.predictions: mmit.predictions.py
	python $<
mmit.fulltrain.predictions: mmit.fulltrain.predictions.py
	python $<
figure.simulated.functions.pdf: figure.simulated.functions.py
	python $<
downloaded.datasets: downloaded.datasets.py 
	python $<
figure.mmit.hps.pgf: figure.mmit.hps.py ./predictions/mmit*/*/parameters.json
	python $<
cart.predictions: cart.predictions.py ./data/*/targets.csv
	python $<
figure.cart.vs.mmit.pgf: figure.cart.vs.mmit.py ./predictions/mmit*/*/parameters.json
	python $<
figure.leaf.split: figure.leaf.split.py
	python $<
figure.cart.hps: figure.cart.hps.py ./predictions/cart/*/parameters.pkl
	python $<
penaltyLearning.raw.features.predictions.RData: penaltyLearning.raw.features.predictions.R
	R --no-save < $<
mmit.raw.features.predictions: mmit.raw.features.predictions.py
	python $<