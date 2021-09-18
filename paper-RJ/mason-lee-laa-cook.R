## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE, 
  warning = FALSE, 
  message = FALSE)


## ----load-libraries-----------------------------------------------------------
library(cassowaryr)
library(GGally)
library(plotly)
library(tidyverse)


## ----building-blocks, out.height = "30%", out.width = "100%", fig.cap = "The building blocks for graph-based scagnostics"----
knitr::include_graphics("figures/draw1.png")


## ----getdata, eval=FALSE------------------------------------------------------
#> # This is the code that pulls the data together
#> # remotes::install_github("robjhyndman/compenginets")
#> library(compenginets)
#> library(tsfeatures)
#> library(tidyverse)
#> 
#> # get 3 different types of time series
#> cets_birdsong <- get_cets("Birdsong")
#> cets_finance <- get_cets("Finance")
#> cets_music <- get_cets("Music")
#> save(cets_birdsong, file="data/cets_birdsongs.rda")
#> save(cets_finance, file="data/cets_finance.rda")
#> save(cets_music, file="data/cets_music.rda")
#> 
#> # compute time series features
#> feat_birdsongs <- tsfeatures(cets_birdsong) %>%
#>   mutate(type="birdsongs")
#> feat_finance <- tsfeatures(cets_finance) %>%
#>   mutate(type="finance")
#> feat_music <- tsfeatures(cets_music) %>%
#>   mutate(type="music")
#> save(feat_birdsongs, file="data/feat_birdsongs.rda")
#> save(feat_finance, file="data/feat_finance.rda")
#> save(feat_music, file="data/feat_music.rda")
#> 
#> # make big dataset
#> features_bfm <- bind_rows(feat_birdsongs,
#>                           feat_finance,
#>                           feat_music)


## ----tscollections------------------------------------------------------------
load("data/feat_birdsongs.rda")

# calculate scagnostics
scag_birdsongs <- calc_scags_wide(feat_birdsongs[,4:16])


## ----scatmat, fig.height=12, fig.width=16, out.width="100%", include=knitr::is_html_output(), eval=knitr::is_html_output()----
#> # interactive scatterplot NOT WORKING PROPERLY YET
#> scag_birdsongs <- scag_birdsongs %>%
#>   mutate(id = paste(Var1, Var2))
#> highlight_key(scag_birdsongs, key = "id") %>%
#>   ggpairs(columns = 3:10) %>%
#>   ggplotly() %>%
#>   highlight("plotly_selected")


## ----scatmatstatic, fig.height=10, fig.width=10, out.width="100%", include=knitr::is_latex_output(), eval=knitr::is_latex_output()----
scag_birdsongs %>% 
  ggpairs(columns = 3:10)

```{.r .distill-force-highlighting-css}
```
