Multiple Factor Analysis, R package
===================================
For Stat 243, Fall 2016, UC Berkeley

Team Members: Anna Liu, Carson McNeil, Rui Wang, Jingtian Yao

Installation
------------
```{r}
devtools::install_git("https://github.com/cmcneil/project-243","mfa")
```

Quickstart
-----------
Please see the vignette [MFA Introduction](https://github.com/cmcneil/project-243/blob/master/mfa/vignettes/mfa_intro.Rmd) for Introduction to Multiple Factor Analysis and Quickstart

Shiny App
---------
Two ways to run the shiny app for the wine tasting experiment example:

You can download the package and run the following lines
```{r}
library(mfa)
mfa::runExample()
```
Or you can click the webpage [link](https://liuanna.shinyapps.io/wines_app/) to interact with the shiny app without downloading anything to local.

More
----
Please see the vignettes [contribution](https://github.com/cmcneil/project-243/blob/master/mfa/vignettes/exploring_contributions.Rmd) and [display](https://github.com/cmcneil/project-243/blob/master/mfa/vignettes/display.Rmd)

