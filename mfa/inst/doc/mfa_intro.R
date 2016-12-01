## ----install,echo=FALSE--------------------------------------------------
#devtools::install_git("https://github.com/cmcneil/project-243","mfa")

## ------------------------------------------------------------------------
# Download the dataset
data(wines)
# The first column is just wine IDs, and the last few columns are supplementary physical data.
# Columns 2-54 are the actual sets of ratings.
analysis_data = wines[,2:54]

## ------------------------------------------------------------------------
variable_tables = list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)

## ------------------------------------------------------------------------
library(mfa)
# We pass the data, the table specification, the number of components, and arguments to the `scale`
# function in order to z-score the data.
analysis_result = mfa(analysis_data, variable_tables, ncomps = 2, center = TRUE, scale = TRUE)
# Now we can examine the results of the analysis, for example, the dataset in terms
# of the compromise factor scores:
analysis_result@cfs

