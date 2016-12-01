## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
data = read.csv("https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv")
# The first column is just wine IDs, and the last few columns are supplementary physical data.
# Columns 2-54 are the actual sets of ratings.
analysis_data = data[,2:54]
variable_tables = list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)
library(mfa)
# We pass the data, the table specification, the number of components, and arguments to the `scale`
# function in order to z-score the data.
analysis_result = mfa(analysis_data, variable_tables, ncomps = 2, center = TRUE, scale = TRUE)

## ------------------------------------------------------------------------
sumeigen(analysis_result)

## ------------------------------------------------------------------------
print(analysis_result, tablenumber=3)

