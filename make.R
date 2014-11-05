library(knitr)

source('./scripts/datagen.R')

setwd('results')
knit2html(input='../scripts/poster.Rmd', output='poster.html')