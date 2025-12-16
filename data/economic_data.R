# Generate the macro and micro economic data
library(tidyverse)
library(tsfeatures)
# devtools::install_github("robjhyndman/compenginets")
# library(compenginets)

# cate_path <- category_scraping()

f1 <- read_csv("data/timeseries/comp-engine-export-metadata-1b6cc.csv")
f2 <- read_csv("data/timeseries/comp-engine-export-metadata-664b7.csv")
f3 <- read_csv("data/timeseries/comp-engine-export-metadata-c02e5.csv")

#birdsongs <- filter(f1, category=="Birdsong")
