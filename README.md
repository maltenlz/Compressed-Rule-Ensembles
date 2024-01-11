# Compressed-Rule-Ensembles
R Implementation of Compressed Rule Ensemble Models introduced in "Compressed Rule Ensemble Learning) (2022) by Malte Nalenz and Thomas Augustin, accepted for publication at the AIStats 2022 conference.

# Install

library(devtools) <br />
devtools::install_git("https://github.com/maltenlz/Compressed-Rule-Ensembles")<br />
library(cre)

# Example on simulated data

Draw data from mixture of normals:<br /><br />
x1 = c(rnorm(100,-1, 1), rnorm(100, 1, 1))<br />
x2 = c(rnorm(100,-1, 1), rnorm(100, 1, 1))<br />
x = cbind(x1, x2)<br />
y = c(rep(1, times = 100), rep(0, times = 100))<br />

Run the CRE model with default settings:<br />
<br />
cre_mod = cre(x, y, task = "class")

Predict in-sample:<br />
<br />
predict(cre_mod, x)

Look at the most important rules:<br /><br />
important_rules(cre_mod)

Also look the distribution of split points:<br /><br />
visualise_clusters(cre_mod)

# This package is actively developed with more extensions coming soon... 
