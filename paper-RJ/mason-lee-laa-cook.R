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
library(plotly)
library(patchwork)
library(knitr)
library(ggthemes)


## ----building-blocks, out.height = "30%", out.width = "100%", fig.cap = "The building blocks for graph-based scagnostics", eval=FALSE----
#> knitr::include_graphics("figures/draw1.png")


## ----building-blocks2, width = 150, height = 50, out.width = "100%", fig.cap = "The building blocks for graph-based scagnostics"----
library(alphahull)
data("features")
nl <- features %>% filter(feature == "nonlinear2")
d1 <- draw_convexhull(nl$x, nl$y) +
  ggtitle("a. Convex hull") +
  xlab("") + ylab("") +
  theme_void() +
  theme(aspect.ratio=1, axis.text = element_blank())
d2 <- draw_alphahull(nl$x, nl$y) +
  ggtitle("b. Alpha hull") +
  xlab("") + ylab("") +
  theme_void() +
  theme(aspect.ratio=1, axis.text = element_blank())
d3 <- draw_mst(nl$x, nl$y) +
  ggtitle("c. Min. span. tree") +
  xlab("") + ylab("") +
  theme_void() +
  theme(aspect.ratio=1, axis.text = element_blank())
d1 + d2 + d3


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


## ----tscollections, eval=FALSE------------------------------------------------
#> load("data/feat_birdsongs.rda")
#> 
#> # calculate scagnostics
#> scag_birdsongs <- calc_scags_wide(feat_birdsongs[,4:16])


## ----scatmat, fig.height=12, fig.width=12, out.width="100%", include=knitr::is_html_output(), eval=knitr::is_html_output(), eval=FALSE----
#> # interactive scatterplot
#> scag_birdsongs <- scag_birdsongs %>%
#>   mutate(id = paste(Var1, Var2))
#> 
#> ggplotly(ggpairs(scag_birdsongs, columns = 3:10, aes(label=id)), width = 600, height = 600)


## ----scatmatstatic, fig.height=10, fig.width=10, out.width="100%", include=knitr::is_latex_output(), eval=knitr::is_latex_output(), eval=FALSE----
#> scag_birdsongs %>%
#>   ggpairs(columns = 3:10)


## ----interesting-pair, eval=FALSE---------------------------------------------
#> p <- ggplot(feat_birdsongs, aes(x=linearity, y=curvature, label = name)) +
#>   geom_point()


## ----interesting-pair-interactive, out.width="50%", include=knitr::is_html_output(), eval=knitr::is_html_output(), eval=FALSE----
#> ggplotly(p, width=300, height=300)


## ----interesting-pair-static, out.width="50%", include=knitr::is_latex_output(), eval=knitr::is_latex_output(), eval=FALSE----
#> p


## ----interesting-ts, eval=FALSE-----------------------------------------------
#> # plot the time series
#> load("data/cets_birdsongs.rda")
#> load("data/feat_birdsongs.rda")
#> 
#> # Find interesting series: high curvature, low linearity
#> s <- feat_birdsongs %>% filter(curvature > 0.9) %>% select(name)
#> s_ts <- cets_birdsongs[[s$name]] %>%
#>   as_tibble() %>% mutate(t = 1:n())
#> ggplot(s_ts, aes(x=t, y=x)) + geom_line()
#> # Find interesting series: low curvature, high linearity
#> s <- feat_birdsongs %>% filter(linearity > 1.5) %>% select(name)
#> s_ts <- cets_birdsongs[[s$name]] %>%
#>   as_tibble() %>% mutate(t = 1:n())
#> ggplot(s_ts, aes(x=t, y=x)) + geom_line()


## ----interesting-pair2, eval=FALSE--------------------------------------------
#> p <- ggplot(feat_birdsongs, aes(x=diff1_acf10, y=trend, label = name)) +
#>   geom_point()


## ----interesting-pair-interactive2, out.width="50%", include=knitr::is_html_output(), eval=knitr::is_html_output(), eval=FALSE----
#> ggplotly(p, width=300, height=300)


## ----interesting-pair-static2, out.width="50%", include=knitr::is_latex_output(), eval=knitr::is_latex_output(), eval=FALSE----
#> p


## ----interesting-ts2, eval=FALSE----------------------------------------------
#> # plot the time series
#> # Find interesting series: high trend, low diff1
#> s <- feat_birdsongs %>% filter(trend > 0.15) %>% select(name)
#> s_ts <- cets_birdsongs[[s$name]] %>%
#>   as_tibble() %>% mutate(t = 1:n())
#> ggplot(s_ts, aes(x=t, y=x)) + geom_line()


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
#>                       scag_finance) #,
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
#> load("data/feat_finance.rda")
#> load("data/cets_finance.rda")
#> 
#> s <- feat_finance %>% filter(curvature > 45) %>% select(name)
#> s_ts <- cets_finance[[s$name]] %>%
#>   as_tibble() %>% mutate(t = 1:n())
#> ggplot(s_ts, aes(x=t, y=x)) + geom_line()
#> 


## ----eval=FALSE---------------------------------------------------------------
#> # Testing for Ursula's code
#> library(feasts)
#> get_features <- function(ts_in){
#> features(as_tsibble(ts_in), value, feature_set("feasts"))
#> }
#> feats_birdsong <- purrr::map_dfr(cets_birdsongs, get_features)
#> cassowaryr::calc_scags_wide(feats_birdsong)
#> 
#> library(cassowaryr)
#> load("data/feasts_birdsong.rda")
#> sc_skinny(feats_birdsong$zero_run_mean, feats_birdsong$spikiness)
#> debug(cassowaryr:::sc_convex.list)
#> 
#> 


## ----eval=FALSE---------------------------------------------------------------
#> library(fitzRoy)
#> aflw <- fetch_player_stats(2020, comp = "AFLW")
#> save(aflw, file = "data/aflw.rda")
#> 
#> aflw_num <- aflw %>%
#>   select_if(is.numeric)
#> save(aflw_num, file = "data/aflw_num.rda")
#> 
#> scag_aflw <- calc_scags_wide(aflw_num[,5:37])
#> save(scag_aflw, file = "data/scagnostics_aflw.rda")
#> 


## ----splines------------------------------------------------------------------
# Saved scagnostics because calc takes about 30 mins
load("data/scagnostics_aflw.rda")
load("data/aflw_num.rda")
load("data/aflw.rda")

# Highest splines table
scag_aflw %>% 
  select(Var1, Var2, splines) %>%
  arrange(desc(splines)) %>%
  head(10) %>% 
  kable()

# High on splines
aflw <- aflw %>%
  mutate(name = paste(player.givenName, player.surname))
s1 <- ggplot(aflw, 
             aes(x=totalPossessions, 
                 y=disposals, 
                 label = name)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() 
s2 <- ggplot(aflw, 
             aes(x=goals, 
                 y=goalAccuracy, 
                 label = name)) +
  geom_jitter(width = 0.1, height = 2) 
s3 <- ggplot(aflw, 
             aes(x=kicks, 
                 y=disposals, 
                 label = name)) +
  geom_point() 


## ----aflw_interactive, fig.cap="Scatterplots with high values on the splines scagnostic. Mouseover to examine the players relative the the statistics.", include=knitr::is_html_output(), eval=knitr::is_html_output()----
#> gs1 <- ggplotly(s1)
#> gs2 <- ggplotly(s2)
#> gs3 <- ggplotly(s3)
#> subplot(gs1, gs2, gs3, nrows=1, widths = c(0.33, 0.33, 0.33), heights = 0.6)


## ----aflw_static, fig.cap="Scatterplots with high values on the splines scagnostic.", include=knitr::is_latex_output(), eval=knitr::is_latex_output()----
s1 + s2 + s3


## ----some_are_kickers, fig.cap = "Some players tend to kick the ball, even when challenged, whereas others more often use handball for disposals. ", include=knitr::is_html_output(), eval=knitr::is_html_output()----
#> # High on clumpy, low on striated
#> kickers <- ggplot(aflw, aes(x=contestedPossessions, y=handballs,
#>                       label = name)) +
#>   geom_abline(intercept = 0, slope = 1) +
#>   geom_jitter()
#> ggplotly(kickers, width=400, height=400)


## ----eval=FALSE---------------------------------------------------------------
#> # Parallel coordinate plot
#> library(GGally)
#> ggparcoord(scag_aflw, columns = c(3:5, 7:11), scale = "globalminmax")
#> 
#> # Or look at pairs
#> ggplot(scag_aflw, aes(x=splines, y=striated,
#>                       label = paste(Var1, Var2))) +
#>   geom_point()
#> ggplot(scag_aflw, aes(x=clumpy, y=striated,
#>                       label = paste(Var1, Var2))) +
#>   geom_point()
#> ggplotly()
#> 


## ----eval=FALSE---------------------------------------------------------------
#> # Extra AFLW analysis, not to include
#> # High on outlying: actually not so interesting
#> out <- ggplot(aflw, aes(x=metresGained, y=hitouts)) +
#>   geom_point()
#> 
#> # High on clumpy
#> ggplot(aflw, aes(x=disposalEfficiency, y=bounces, label = name)) +
#>   #geom_abline(intercept = 0, slope = 1) +
#>   geom_point()
#> ggplotly()


## ----eval=FALSE---------------------------------------------------------------
#> # Was thinking this might be a good example, but too many missings
#> library(NHANES)
#> NHANES_numeric <- NHANES %>%
#>   select_if(is.numeric)
#> 
#> # Lots of missing values, need to check
#> library(naniar)
#> vis_miss(NHANES_numeric)
#> vm <- miss_var_summary(NHANES_numeric[,-1])
#> 
#> # Keep only variables with less than 50% missing
#> keep <- vm %>% filter(pct_miss < 50)
#> 
#> # Now compute scagnostics
#> scag_nhanes <- calc_scags_wide(NHANES_numeric[,keep$variable])

```{.r .distill-force-highlighting-css}
```
