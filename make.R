library(knitr)

source('./scripts/datagen.R')

source('./scripts/genWindSummaries.R')

source('./scripts/posterBackground/do.R')
source('./scripts/posterBackground/load.R')

setwd('results')
knit2html(input='../scripts/poster.Rmd', output='poster.html')