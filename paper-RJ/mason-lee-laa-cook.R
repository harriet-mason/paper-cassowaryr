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
#> 
#> # get 3 different types of time series
#> set.seed(300)
#> cets_birdsongs <- get_cets("Birdsong")
#> cets_finance <- get_cets("Finance")
#> cets_music <- get_cets("Music")
#> save(cets_birdsongs, file="data/cets_birdsongs.rda")
#> save(cets_finance, file="data/cets_finance.rda")
#> save(cets_music, file="data/cets_music.rda")
#> 
#> # compute time series features
#> feat_birdsongs <- tsfeatures(cets_birdsongs) %>%
#>   mutate(name = names(cets_birdsongs),
#>          type="birdsongs")
#> feat_finance <- tsfeatures(cets_finance) %>%
#>   mutate(name = names(cets_finance),
#>          type="finance")
#> feat_music <- tsfeatures(cets_music) %>%
#>   mutate(name = names(cets_music),
#>          type="music")
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


## ----scatmat, fig.height=12, fig.width=12, out.width="100%", include=knitr::is_html_output(), eval=knitr::is_html_output()----
#> # interactive scatterplot
#> scag_birdsongs <- scag_birdsongs %>%
#>   mutate(id = paste(Var1, Var2))
#> 
#> ggplotly(ggpairs(scag_birdsongs, columns = 3:10, aes(label=id)), width = 600, height = 600)


## ----scatmatstatic, fig.height=10, fig.width=10, out.width="100%", include=knitr::is_latex_output(), eval=knitr::is_latex_output()----
scag_birdsongs %>% 
  ggpairs(columns = 3:10)


## ----interesting-pair---------------------------------------------------------
p <- ggplot(feat_birdsongs, aes(x=linearity, y=curvature, label = name)) +
  geom_point()


## ----interesting-pair-interactive, out.width="50%", include=knitr::is_html_output(), eval=knitr::is_html_output()----
#> ggplotly(p, width=300, height=300)


## ----interesting-pair-static, out.width="50%", include=knitr::is_latex_output(), eval=knitr::is_latex_output()----
p


## ----interesting-ts-----------------------------------------------------------
# plot the time series 
load("data/cets_birdsongs.rda")
# Find interesting series: high curvature, low linearity
s <- feat_birdsongs %>% filter(curvature > 0.9) %>% select(name)
s_ts <- cets_birdsongs[[s$name]] %>%
  as_tibble() %>% mutate(t = 1:n())
ggplot(s_ts, aes(x=t, y=x)) + geom_line()
# Find interesting series: low curvature, high linearity
s <- feat_birdsongs %>% filter(linearity > 1.5) %>% select(name)
s_ts <- cets_birdsongs[[s$name]] %>%
  as_tibble() %>% mutate(t = 1:n())
ggplot(s_ts, aes(x=t, y=x)) + geom_line()


## ----interesting-pair2--------------------------------------------------------
p <- ggplot(feat_birdsongs, aes(x=diff1_acf10, y=trend, label = name)) +
  geom_point()


## ----interesting-pair-interactive2, out.width="50%", include=knitr::is_html_output(), eval=knitr::is_html_output()----
#> ggplotly(p, width=300, height=300)


## ----interesting-pair-static2, out.width="50%", include=knitr::is_latex_output(), eval=knitr::is_latex_output()----
p


## ----interesting-ts2----------------------------------------------------------
# plot the time series 
# Find interesting series: high trend, low diff1
s <- feat_birdsongs %>% filter(trend > 0.15) %>% select(name)
s_ts <- cets_birdsongs[[s$name]] %>%
  as_tibble() %>% mutate(t = 1:n())
ggplot(s_ts, aes(x=t, y=x)) + geom_line()


## ----tscompare, eval=FALSE----------------------------------------------------
#> load("data/feat_finance.rda")
#> load("data/feat_music.rda")
#> feat_all <- bind_rows(feat_birdsongs,
#>                       feat_finance,
#>                       feat_music)
#> 
#> # calculate scagnostics
#> # Would like to do this calculation using a group_by type statement
#> scag_birdsongs <- calc_scags_wide(feat_birdsongs[,4:16]) %>%
#>   mutate(type = "birdsongs")
#> scag_finance <- calc_scags_wide(feat_finance[,4:16]) %>%
#>   mutate(type = "finance")
#> scag_music <- calc_scags_wide(feat_music[,4:16]) %>%
#>   mutate(type = "music")
#> scag_all <- bind_rows(scag_birdsongs,
#>                       scag_finance,
#>                       scag_music)
#> 
#> scag_all <- scag_all %>%
#>   mutate(id = paste(Var1, Var2, type))
#> 
#> ggplotly(ggpairs(scag_all, columns = 3:10, aes(colour = type, label = id), alpha = 0.5), width = 800, height = 800)
#> 
#> p <- ggplot(feat_all,
#>             aes(x=entropy,
#>                 y=curvature,
#>                 colour=type,
#>                 label=name)) +
#>   geom_point()
#> ggplotly(p, width=500, height=300)
#> 
#> s <- feat_all %>% filter(curvature > 45) %>% select(name)
#> s_ts <- cets_finance[[s$name]] %>%
#>   as_tibble() %>% mutate(t = 1:n())
#> ggplot(s_ts, aes(x=t, y=x)) + geom_line()
#> 

```{.r .distill-force-highlighting-css}
```
