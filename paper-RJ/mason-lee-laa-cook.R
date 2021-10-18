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
library(ggimg) #for the visual table
library(ggstance) #for vertical dodge on plot
library(fitzRoy) #for AFLW data
library(gridExtra) #groups of static scatter plots


## ----building-blocks, out.height = "30%", out.width = "100%", fig.cap = "The building blocks for graph-based scagnostics", eval=FALSE----
#> knitr::include_graphics("figures/draw1.png")


## ----building-blocks2, width = 150, height = 50, out.width = "100%", fig.cap = "The building blocks for graph-based scagnostics", layout = "l-body"----
library(alphahull)
data("features")
nl <- features %>% filter(feature == "nonlinear2")
d1 <- draw_convexhull(nl$x, nl$y, clr="#FFD700", fill=TRUE) +
  ggtitle("a. Convex hull") +
  xlab("") + ylab("") +
  theme_void() +
  theme(aspect.ratio=1, axis.text = element_blank())
d2 <- draw_alphahull(nl$x, nl$y, clr="#00a800") +
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


## ----convexscag, out.height = "20%", out.width = "100%", fig.cap = "Convex Scagnostic Visual Explanation"----
knitr::include_graphics("figures/drawconvex.png")


## ----skinnyscag, out.height = "20%", out.width = "100%", fig.cap = "Skinny Scagnostic Visual Explanation"----
knitr::include_graphics("figures/drawskinny.png")


## ----outlyingscag, out.height = "20%", out.width = "100%", fig.cap = "Outlying Scagnostic Visual Explanation"----
knitr::include_graphics("figures/drawoutlying.png")


## ----stringyscag, out.height = "20%", out.width = "100%", fig.cap = "Stringy Scagnostic Visual Explanation"----
knitr::include_graphics("figures/drawstringy.png")


## ----skewedscag, out.height = "20%", out.width = "100%", fig.cap = "Skewed Scagnostic Visual Explanation"----
knitr::include_graphics("figures/drawskewed.png")


## ----sparsescag, out.height = "20%", out.width = "100%", fig.cap = "Sparse Scagnostic Visual Explanation"----
knitr::include_graphics("figures/drawsparse.png")


## ----clumpyscag, out.height = "20%", out.width = "100%", fig.cap = "Clumpy Scagnostic Visual Explanation"----
knitr::include_graphics("figures/drawclumpy.png")


## ----striatedscag, out.height = "20%", out.width = "100%", fig.cap = "Striated Scagnostic Visual Explanation"----
knitr::include_graphics("figures/drawstriated.png")


## ----monotonicscag, out.height = "20%", out.width = "100%", fig.cap = "Monotonic Scagnostic Visual Explanation"----
knitr::include_graphics("figures/drawmonotonic.png")


## ----splinescag, out.height = "20%", out.width = "100%", fig.cap = "Splines Scagnostic Visual Explanation"----
knitr::include_graphics("figures/drawsplines.png")


## ----dcorscag, out.height = "20%", out.width = "100%", fig.cap = "Dcor Scagnostic Visual Explanation"----
knitr::include_graphics("figures/drawdcor.png")


## ---- Features plot, width = 150, height = 150, out.width = "100%", fig.cap = "The Scatter Plots of the Features Dataset"----
mypal <- c("#b2182b", "#d53e4f","#FF4E50", "#FC913A", "#fdae61",
           "#F9D423", "#fee08b" , "#abdda4" , "#a6d96a" , "#66c2a5" ,
           "#66bd63","#3B8183", "#3288bd", "#74add1",  "#abd9e9")

#plot them
ggplot(features, aes(x,y,colour=feature))+
  geom_point() +
  theme_minimal() + 
  facet_wrap(~feature, ncol=5, scales="free") +
  xlab("") + ylab("") +
  theme(legend.position = "none", 
        aspect.ratio= 1, 
        axis.text = element_blank()) +
  scale_colour_manual(values = mypal)



## ---- Scatter Plots as images,  include=FALSE, eval=FALSE---------------------
#> #set theme so all scatter plots in table match
#> plot_theme <-  theme_classic() + #theme_minimal() +
#>   theme(aspect.ratio=1, axis.title=element_blank(), axis.text = element_blank(),
#>         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#>         panel.border = element_rect(colour = "black", fill=NA, size=4),
#>         legend.position = "none"
#>         )
#> 
#> #save scatter plots as images
#> plots <- sort(unique(features$feature))
#> 
#> for (i in seq(length(plots))){
#>   holdplot <- features %>%
#>     filter(feature==plots[i]) %>%
#>     ggplot(aes(x,y, size=2))+ geom_point(colour=mypal[i]) + plot_theme
#>   ggsave(paste0("paper-RJ/figures/", plots[i], ".png"),holdplot) #files already in /figure/
#> }
#> 


## ---- Visual Table, width = 150, height = 150, out.width = "100%", fig.cap = "The Features Scatterplots in a Visual Table"----

# Calculate Scagnostics
features_scagnostics_long <- features %>%
  group_by(feature) %>%
  summarise(calc_scags(x,y)) %>%
  pivot_longer(cols=outlying:dcor, names_to = "scagnostic")

# edit data frame
plot_path_data <- features_scagnostics_long %>%
  mutate(plotad = paste0("figures/", feature, ".png"))

# which plots to include in visual table
whichplots <- function(scag, feature){
  pad = FALSE
  # Alphahull measures
  if(all(scag=="convex", feature %in% c("discrete", "ring", "l-shape"))){
    pad = TRUE
  }
  if(all(scag=="skinny", feature %in% c("line", "positive", "disk"))){
    pad = TRUE
  }
  
  # MST measures
  if(all(scag=="outlying", feature %in% c("outliers2","l-shape", "outliers"))){
    pad = TRUE
  }
  if(all(scag=="stringy", feature %in% c("nonlinear1", "gaps"))){
    pad = TRUE
  }
  if(all(scag=="striated", feature %in% c("vlines", "discrete", "weak"))){
    pad = TRUE
  }
  if(all(scag=="clumpy", feature %in% c("vlines", "clusters", "nonlinear"))){
    pad = TRUE
  }
  if(all(scag=="sparse", feature %in% c("weak", "line"))){
    pad = TRUE
  }
  if(all(scag=="skewed", feature %in% c("l-shape", "barrier"))){
    pad = TRUE
  }
  
  # Association Measures
  if(all(scag=="monotonic", feature %in% c("line", "positive", "weak"))){
    pad = TRUE
  }
  if(all(scag=="splines", feature %in% c("nonlinear2", "clusters", "vlines"))){
    pad = TRUE
  }
  if(all(scag=="dcor", feature %in% c("positive", "barrier", "gaps"))){
    pad = TRUE
  }
  pad
}

# Make Visual Table
# Data
plot_data <- plot_path_data %>%
  group_by(scagnostic, feature) %>%
  mutate(doplot = whichplots(scagnostic, feature)) %>%
  ungroup() %>%
  filter(doplot==TRUE)

# so i dont have to keep adjusting the image size
s <- length(unique(plot_data$feature))

# plot
visual_table <- ggplot(plot_data, aes(x=value , y=scagnostic))+
  geom_point_img(aes(img = plotad), size = 1.5) + 
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="none") +
  xlim(-0.1,1.1) +
  scale_size_identity()+
  xlab("Value") +
  ylab("Scagnostic") +
  ggtitle("Visual Table of Scagnostic Values")+
  theme(
    axis.line = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.grid.major.y = element_line()
)
#ggsave("paper-RJ/figures/visual_table.png", visual_table, width=10, height=10)
visual_table



## ---- Striated Vtable, width = 150, height = 50, out.width = "100%", fig.cap = "A Visual Table Comparison of Striated and Striated 2"----

# Make Visual Table
# Data
plot_data_striated <- plot_path_data %>%
  group_by(scagnostic, feature) %>%
  mutate(doplot = ifelse(all(scagnostic %in% c("striated","striated2"), 
                             feature %in% c("vlines", "discrete","line", "disk", "outliers2")),
                         TRUE,
                         FALSE)) %>% 
  ungroup() %>%
  filter(doplot==TRUE)


# plot
striated_visual_table <- ggplot(plot_data_striated, aes(x=value , y=scagnostic))+
  geom_point_img(aes(img = plotad), size = 2,
                 position=ggstance::position_dodgev(height=0.9)) +
  xlim(-0.05,1.05) +
  scale_size_identity()+
  xlab("Value") +
  ylab("Scagnostic") +
  ggtitle("Striated Comparison") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    legend.position="none",
    axis.line = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.grid.major.y = element_line()
)
#ggsave("paper-RJ/figures/striated_visual_table.png", striated_visual_table, width=10, height=10)

striated_visual_table



## ---- Clumpy Vtable, width = 150, height = 50, out.width = "100%", fig.cap = "A Visual Table Comparison of Clumpy and Clumpy 2"----

plot_data_clumpy <- plot_path_data %>%
  group_by(scagnostic, feature) %>%
  mutate(doplot = ifelse(all(scagnostic %in% c("clumpy","clumpy2"), 
                             feature %in% c("vlines", "clusters","gaps", "outliers", "barrier")),
                         TRUE,
                         FALSE)) %>% 
  ungroup() %>%
  filter(doplot==TRUE)


# plot
clumpy_visual_table <- ggplot(plot_data_clumpy, aes(x=value , y=scagnostic))+
  geom_point_img(aes(img = plotad), size = 2, 
                 position=ggstance::position_dodgev(height=0.9)) +
  xlim(-0.05,1.05) +
  scale_size_identity()+
  xlab("Value") +
  ylab("Scagnostic") +
  ggtitle("Clumpy Comparison") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    legend.position="none",
    axis.line = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.grid.major.y = element_line()
)
#ggsave("paper-RJ/figures/clumpy_visual_table.png", clumpy_visual_table, width=10, height=10)

clumpy_visual_table


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
#> get_features <- function(ts_in) {
#>   features(as_tsibble(ts_in),
#>            value, feature_set("feasts"))
#> }
#> feats_birdsong <- purrr::map_dfr(cets_birdsongs, get_features)
#> 
#> 
#> 
#> cassowaryr::calc_scags_wide(feats_birdsong)
#> 
#> library(cassowaryr)
#> load("data/feasts_birdsong.rda")
#> sc_skinny(feats_birdsong$zero_run_mean, feats_birdsong$spikiness)
#> debug(cassowaryr:::sc_convex.list)
#> 
#> 


## ----eval=FALSE---------------------------------------------------------------
#> library(compenginets)
#> 
#> # Playing with other ideas
#> cets_macro <- get_cets("macroeconomics")
#> cets_micro <- get_cets("micoeconomics")
#> save(cets_macro, file="data/cets_macro.rda")
#> save(cets_micro, file="data/cets_micro.rda")
#> 
#> # Comparing two types of time series
#> library(feasts)
#> get_features <- function(ts_in) {
#>   features(as_tsibble(ts_in),
#>            value, feature_set("feasts"))
#> }
#> 
#> load(here::here("data/cets_macro.rda"))
#> feats_macro <- purrr::map_dfr(cets_macro, get_features)
#> save(feats_macro, file=here::here("data/feats_macro.rda"))
#> 
#> load("data/cets_micro.rda")
#> feats_micro <- purrr::map_dfr(cets_micro, get_features)
#> save(feats_micro, file = "data/feats_micro.rda")


## ----timeseries, fig.cap="Interesting differences between two groups of time series detected by scagnostics. The time series are described by time series features, in order to handle different length series. Scagnostics are computed on these features separately for each set to explore for shape differences.", fig.width = 12, fig.height = 4, out.width="100%", layout = "l-body"----
load(here::here("data/feats_macro.rda"))
load(here::here("data/feats_micro.rda"))

# Check scale of each variable
#feats_macro %>% 
#  summarise_all(sd, na.rm=TRUE) %>% 
#  glimpse()

scags_macro <- calc_scags_wide(feats_macro[,1:4], 
  scags = c("convex", "splines", "skinny",
            "outlying", "stringy", "striated",
            "clumpy", "sparse", "skewed"))

#feats_micro %>% 
#  summarise_all(sd, na.rm=TRUE) %>% 
#  glimpse()

scags_micro <- calc_scags_wide(feats_micro[,1:4], 
  scags = c("convex", "splines", "skinny",
            "outlying", "stringy", "striated",
            "clumpy","sparse", "skewed"))

scags_macro <- scags_macro %>%
  pivot_longer(outlying:splines, 
               names_to = "scags",
               values_to = "macro_value") 

scags_micro <- scags_micro %>%
  pivot_longer(outlying:splines, 
               names_to = "scags",
               values_to = "micro_value") 

scags_mac_mic <- full_join(scags_macro, scags_micro, by = c("Var1", "Var2", "scags"))
scags_mac_mic <- scags_mac_mic %>%
  mutate(scag_dif = abs(macro_value-micro_value)) %>%
           # *(macro_value+micro_value)) %>% # weight lower if both values are small
  arrange(desc(scag_dif)) %>%
  head(5)

feats_mac_mic <- bind_rows(
  mutate(feats_macro, type = "macro"),
  mutate(feats_micro, type = "micro")
)

p1 <- ggplot(feats_mac_mic, 
       aes(x=curvature, y=spikiness, colour = type)) + 
  geom_point(alpha = 0.5) +
  scale_colour_brewer("", palette="Dark2") +
  theme(aspect.ratio=1, axis.text = element_blank()) + 
  ggtitle("Boring") # So boring

p2 <- ggplot(feats_mac_mic, 
       aes(x=curvature, y=trend_strength, colour = type)) + 
  geom_point(alpha = 0.5) +
  scale_colour_brewer("", palette="Dark2") +
  theme(aspect.ratio=1, axis.text = element_blank()) + 
  ggtitle("Interesting") # Interesting

p3 <- ggplot(feats_mac_mic, 
       aes(x=curvature, y=linearity, colour = type)) + 
  geom_point(alpha = 0.5) +
  scale_colour_brewer("", palette="Dark2") +
  theme(aspect.ratio=1, axis.text = element_blank()) + 
  ggtitle("Interesting?") # Sort of interesting

p1 + p2 + p3 + plot_layout(guides = "collect") & theme(legend.position = "bottom")


## ----tsplots, fig.cap="Selection of series from the two groups, macroeconomics and microeconomics. The difference is in the jagginess of the two series.", fig.width = 12, fig.height = 8, out.width="100%", layout = "l-body"----
load("data/cets_macro.rda")
load("data/cets_micro.rda")

macro_s_ts1 <- cets_macro[[1]] %>%
  as_tibble() %>% 
  mutate(x = as.numeric(x), 
         t = 1:n(), 
         name = names(cets_macro)[[1]])
macro_s_ts2 <- cets_macro[[2]] %>%
  as_tibble() %>% 
  mutate(x = as.numeric(x), 
         t = 1:n(), 
         name = names(cets_macro)[[2]])
macro_s_ts3 <- cets_macro[[3]] %>%
  as_tibble() %>% 
  mutate(x = as.numeric(x), 
         t = 1:n(), 
         name = names(cets_macro)[[3]])
macro_s_ts <- bind_rows(macro_s_ts1,
                        macro_s_ts2,
                        macro_s_ts3)
mac <- ggplot(macro_s_ts, aes(x=t, y=x)) +
  geom_line() + 
  facet_wrap(~name, ncol=1, scales = "free") +
  ggtitle("Macroeconomics") +
  theme(axis.text = element_blank())

micro_s_ts1 <- cets_micro[[1]] %>%
  as_tibble() %>% 
  mutate(x = as.numeric(x), 
         t = 1:n(), 
         name = names(cets_micro)[[1]])
micro_s_ts2 <- cets_micro[[2]] %>%
  as_tibble() %>% 
  mutate(x = as.numeric(x), 
         t = 1:n(), 
         name = names(cets_micro)[[2]])
micro_s_ts3 <- cets_micro[[3]] %>%
  as_tibble() %>% 
  mutate(x = as.numeric(x), 
         t = 1:n(), 
         name = names(cets_micro)[[3]])
micro_s_ts <- bind_rows(micro_s_ts1,
                        micro_s_ts2,
                        micro_s_ts3)
mic <- ggplot(micro_s_ts, aes(x=t, y=x)) + 
  geom_line() + 
  facet_wrap(~name, ncol=1, scales = "free") +
  ggtitle("Microeconomics") +
  theme(axis.text = element_blank())

mac + mic



## ----eval=FALSE---------------------------------------------------------------
#> set.seed(26)
#> bbh <- read_csv("data/bbh_posterior_samples.csv") %>%
#>   sample_n(200)
#> bbh_scags <- calc_scags_wide(bbh)
#> save(bbh_scags, file="paper-RJ/data/bbh_scags.rda")
#> save(bbh, file="paper-RJ/data/bbh_samples.rda")


## -----------------------------------------------------------------------------
load("data/bbh_scags.rda")
bbh1 <- ggplot(bbh_scags, aes(x=convex, y=skinny, 
                      label = paste(Var1, Var2))) +
  geom_point()
bbh2 <- ggplot(bbh_scags, aes(x=dcor, y=splines, 
                      label = paste(Var1, Var2))) +
  geom_point()
bbh3 <- ggplot(bbh_scags, aes(x=clumpy, y=skewed, 
                      label = paste(Var1, Var2))) +
  geom_point()
gs1 <- ggplotly(bbh1)
gs2 <- ggplotly(bbh2)
gs3 <- ggplotly(bbh3)
subplot(gs1, gs2, gs3, nrows=1, widths = c(0.33, 0.33, 0.33), heights = 0.6)


## -----------------------------------------------------------------------------
load("data/bbh_samples.rda")
bbh1 <- ggplot(bbh, aes(x=time, y=ra)) +
  geom_point()
bbh2 <- ggplot(bbh, aes(x=dec, y=ra)) +
  geom_point()
bbh3 <- ggplot(bbh, aes(x=dec, y=time)) +
  geom_point()
bbh4 <- ggplot(bbh, aes(x=m1, y=m2)) +
  geom_point()
bbh5 <- ggplot(bbh, aes(x=chi_p, y=chi_tot)) +
  geom_point()
bbh6 <- ggplot(bbh, aes(x=time, y=alpha)) +
  geom_point()

subplot(bbh1, bbh2, bbh3, bbh4, bbh5, bbh6,
        nrows=2, widths = c(0.33, 0.33, 0.33), heights = c(0.5, 0.5))


## ---- AFL DATA, include=FALSE, eval=FALSE-------------------------------------
#> 
#> aflw <- fetch_player_stats(2020, comp = "AFLW")
#> 
#> aflw_num <- aflw %>%
#>   select_if(is.numeric)
#> 
#> aflw_num <- aggregate(aflw_num[,5:37],
#>   list(aflw$player.player.player.surname),
#>   mean)
#> 
#> aflw_scags <- calc_scags_wide(aflw_num[,2:34])
#> 
#> #AFLW DATA SAVE
#> save(aflw, file="paper-RJ/data/aflw.rda")
#> save(aflw_num, file="paper-RJ/data/aflw_num.rda")
#> save(aflw_scags, file="paper-RJ/data/aflw_scags.rda")


## ---- AFLW Scatter Plots , echo=FALSE-----------------------------------------

load("data/aflw.rda")
load("data/aflw_num.rda")
load("data/aflw_scags.rda")

mypal <- c("#FF4E50", "#fdae61","#fee08b", "#66c2a5", "#3288bd", "#abd9e9")

#standounts from splom
#outlying and Skewed + highest skinny not 1
p1 <- ggplot(aflw_num, aes(x=disposalEfficiency, y=hitouts, label=Group.1)) + 
  theme_classic()+
  ggtitle("Plot 1") + 
  geom_point(colour=mypal[1]) 

#high on the 3 associations measures
p2 <- ggplot(aflw_num, aes(x=totalPossessions, y=disposals, label=Group.1)) + 
  theme_classic()+
  ggtitle("Plot 2") + 
  geom_point(colour=mypal[2]) 

#low on sparse and high on convex
p3 <- ggplot(aflw_num, aes(x=marksInside50, y=goals)) + 
  theme_classic()+
  ggtitle("Plot 3") + 
  geom_point(colour=mypal[3]) 

#high on clumpy adjusted, low on monotonic
p4 <- ggplot(aflw_num, aes(x=onePercenters, y=handballs)) +
  theme_classic()+
  ggtitle("Plot 4") + 
  geom_point(colour=mypal[4]) 

#interesting HIGH on striated, moderate on outlying
p5 <- ggplot(aflw_num, aes(x=bounces, y=hitouts, label=Group.1)) + 
  theme_classic()+
  ggtitle("Plot 5") + 
  geom_point(colour=mypal[5]) 

# me randomly picking two variables
p6 <- ggplot(aflw_num, aes(x=kicks, y=handballs)) + 
  theme_classic()+
  ggtitle("Plot 6") + 
  geom_point(colour=mypal[6])


## ---- AFLW-scatters-interactive, out.width="80%", include=knitr::is_html_output(), eval=knitr::is_html_output()----
#> 
#> subplot(ggplotly(p1), ggplotly(p2), ggplotly(p3), ggplotly(p4), ggplotly(p5), ggplotly(p6),
#>         nrows=2, widths = c(0.33, 0.33, 0.33), heights = c(0.5,0.5))


## ----AFLW-scatters-static, out.width="80%", include=knitr::is_latex_output(), eval=knitr::is_latex_output(), fig.align='center'----
grid.arrange(p1, p2, p3, p4, p5, p6,  nrow=2)


## ---- 3 Random Plots, echo=FALSE, out.width = "100%", , fig.align='center'----
p7 <- ggplot(aflw_num, aes(x=goals, y=clangers)) + 
  geom_point(colour=mypal[6]) +
  theme_classic()+ 
  ggtitle("Another Plot") 

p8 <- ggplot(aflw_num, aes(x=totalPossessions, y=metresGained)) + 
  geom_point(colour=mypal[6]) +
  theme_classic()+
  ggtitle("and Another Plot") 

grid.arrange(p6, p7, p8, nrow=1)



## ---- AFL relevent SPLOMS, include= FALSE, echo=FALSE-------------------------
load("data/aflw_scags.rda")
test <- aflw_scags %>%
  mutate(lab = paste0(Var1, ", ", Var2)) %>%
  mutate(plot1=ifelse(lab=="disposalEfficiency, hitouts", TRUE,FALSE),
         plot2=ifelse(lab=="totalPossessions, disposals", TRUE,FALSE),
         plot3=ifelse(lab=="marksInside50, goals", TRUE,FALSE),
         plot4=ifelse(lab=="onePercenters, handballs", TRUE,FALSE),
         plot5=ifelse(lab=="hitouts, bounces", TRUE, FALSE)
         ) %>%
  mutate(plotted = any(plot1,plot2,plot3,plot4, plot5))

s1 <- ggplot(test, aes(x=outlying, skewed, colour=plot1, label=lab)) + 
  geom_point() +
  theme_classic() +
  theme(legend.position ="none")+
  scale_colour_manual(values=c("grey", mypal[1]))+
  ggtitle("Relevant Scagnostics Plot")

s2 <- ggplot(test, aes(x=splines, dcor, colour=plot2, label=lab)) + 
  geom_point() +
  theme_classic() +
  theme(legend.position ="none")+
  scale_colour_manual(values=c("grey", mypal[1]))+
  ggtitle("Relevant Scagnostics Plot")

s3 <- ggplot(test, aes(x=sparse, convex, colour=plot3)) + 
  geom_point() +
  theme_classic() +
  theme(legend.position ="none") +
  scale_colour_manual(values=c("grey", mypal[3])) +
  ggtitle("Relevant Scagnostics Plot")

s4 <- ggplot(test, aes(x=clumpy2, monotonic, colour=plot4)) +
  geom_point() +
  theme_classic() +
  theme(legend.position ="none") +
  scale_colour_manual(values=c("grey", mypal[4])) +
  ggtitle("Relevant Scagnostics Plot")

s5 <- ggplot(test, aes(x=outlying, y = striated2, colour=plot5, label=lab)) +
  geom_point() +
  theme_classic() +
  theme(legend.position ="none") +
  scale_colour_manual(values=c("grey", mypal[5])) +
  ggtitle("Relevant Scagnostics Plot")



## ---- plot1-interactive, out.width="70%", include=knitr::is_html_output(), eval=knitr::is_html_output()----
#> 
#> subplot(ggplotly(p1), ggplotly(s1),
#>         nrows=1, widths = c(0.5, 0.5), heights = 0.5)%>%
#>   config(displayModeBar = FALSE)


## ----plot1-static, out.width="70%", include=knitr::is_latex_output(), eval=knitr::is_latex_output(), , fig.align='center'----
grid.arrange(p1, s1, nrow=1)


## ---- plot2-interactive, out.width="70%", include=knitr::is_html_output(), eval=knitr::is_html_output()----
#> 
#> subplot(ggplotly(p2), ggplotly(s2),
#>         nrows=1, widths = c(0.5, 0.5), heights = 0.5)%>%
#>   config(displayModeBar = FALSE)


## ----plot2-static, out.width="70%", include=knitr::is_latex_output(), eval=knitr::is_latex_output(), , fig.align='center'----
grid.arrange(p2, s2, nrow=1)


## ---- plot5-interactive, out.width="70%", include=knitr::is_html_output(), eval=knitr::is_html_output()----
#> 
#> subplot(ggplotly(p5), ggplotly(s5),
#>         nrows=1, widths = c(0.5, 0.5), heights = 0.5)%>%
#>   config(displayModeBar = FALSE)


## ----plot5-static, out.width="70%", include=knitr::is_latex_output(), eval=knitr::is_latex_output(), fig.align='center'----
grid.arrange(p5, s5, nrow=1)


## ----splines------------------------------------------------------------------
# Saved scagnostics because calc takes about 30 mins
load("data/aflw.rda")
load("data/scagnostics_aflw.rda")

# Highest splines table
scag_aflw %>% 
  select(Var1, Var2, splines) %>%
  arrange(desc(splines)) %>%
  head(10) %>% 
  kable(digits=2)

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


## ----aflwinteractive, fig.cap="Scatterplots with high values on the splines scagnostic. Mouseover to examine the players relative the the statistics.", include=knitr::is_html_output(), eval=knitr::is_html_output(), layout = "l-body"----
#> gs1 <- ggplotly(s1)
#> gs2 <- ggplotly(s2)
#> gs3 <- ggplotly(s3)
#> subplot(gs1, gs2, gs3, nrows=1, widths = c(0.33, 0.33, 0.33), heights = 0.6)


## ----aflwstatic, fig.cap="Scatterplots with high values on the splines scagnostic.", include=knitr::is_latex_output(), eval=knitr::is_latex_output(), fig.width=10, fig.height=3.5, out.width="100%"----
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
#> ggparcoord(scag_aflw, columns = 3:11, scale = "globalminmax")
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


## ----eval=FALSE---------------------------------------------------------------
#> library(readxl)
#> wbi <- read_xlsx("data/World_Development_Indicators.xlsx", n_max = 2436) # Cut off extra stuff
#> 
#> wbi_wide <- wbi %>%
#>   mutate(`2018 [YR2018]` = str_replace(`2018 [YR2018]`, "..","")) %>%
#>   mutate(`2018 [YR2018]` = as.numeric(`2018 [YR2018]`)) %>%
#>   select(`Country Code`, `Series Code`, `2018 [YR2018]`) %>%
#>   pivot_wider(names_from = `Series Code`, values_from = `2018 [YR2018]`, id_cols = `Country Code`, values_fn = mean) %>%
#>   filter(!is.na(`Country Code`))
#> 
#> # Drop variables with missings, or too similar
#> library(naniar)
#> s_miss <- miss_summary(wbi_wide)
#> gg_miss_var(wbi_wide)
#> 
#> # Remove vars with XX missing
#> drop <- s_miss$miss_var_summary[[1]] %>%
#>   filter(pct_miss > 15)
#> wbi_wide <- wbi_wide %>%
#>   select(-drop$variable)
#> 
#> # Now check cases
#> drop_cnt <- s_miss$miss_case_summary[[1]] %>%
#>   filter(pct_miss > 10)
#> wbi_wide <- wbi_wide[-drop_cnt$case,]
#> 
#> # Check again - still a few sporadic missings
#> s_miss <- miss_summary(wbi_wide)
#> 
#> vis_miss(wbi_wide)
#> 
#> # Remove any variables that are the same
#> #scag_cor <- calc_scags_wide(wbi_wide_sub[,-1],
#> #    scags = "monotonic")
#> #wbi_wide_sub <- wbi_wide_sub %>%
#> #  select(-NY.GDP.MKTP.CD)
#> wbi_wide <- wbi_wide[,-8] # TX.VAL.TECH.MF.ZS too few values
#> save(wbi_wide, file="data/wbi_wide.rda")
#> 
#> # Check individual scag calculations, for testing
#> calc_scags(wbi_wide$EN.ATM.CO2E.PC, wbi_wide$NV.AGR.TOTL.ZS, scags = "striped")
#> 
#> wbi_scags <- calc_scags_wide(wbi_wide[,-1],
#>     scags = c("outlying", "stringy",
#>               "striated2", "clumpy2", "skewed",
#>               "convex", "skinny",
#>               "splines", "striped"))
#> save(wbi_scags, file="data/wbi_scags.rda")


## ----wbi----------------------------------------------------------------------
load("data/wbi_wide.rda")
load("data/wbi_scags.rda")
wbi_scags_long <- wbi_scags %>% 
  pivot_longer(!c(Var1, Var2), names_to = "scag", values_to = "value") %>%
  arrange(desc(value))

w1 <- ggplot(wbi_wide, 
       aes_string(x=as.character(wbi_scags_long$Var1[1]),
                  y=as.character(wbi_scags_long$Var2[1]))) +
  geom_point()

w2 <- ggplot(wbi_wide, 
       aes_string(x=as.character(wbi_scags_long$Var1[2]),
                  y=as.character(wbi_scags_long$Var2[2]))) +
  geom_point()

# Summarise highest on each index
wbi_scags_top <- wbi_scags_long %>% 
  group_by(scag) %>%
  slice_head(n=1)

w3a <- ggplot(wbi_wide, 
       aes_string(x=as.character(wbi_scags_top$Var1[1]),
                  y=as.character(wbi_scags_top$Var2[1]))) +
  geom_point(aes(label = `Country Code`)) +
  ggtitle(wbi_scags_top$scag[1])

w3b <- ggplot(wbi_wide, 
       aes_string(x=as.character(wbi_scags_top$Var1[2]),
                  y=as.character(wbi_scags_top$Var2[2]))) +
  geom_point(aes(label = `Country Code`)) +
  ggtitle(wbi_scags_top$scag[2])
# Most tend to be vars with too many 0's

# Summarise highest on each variable pair
wbi_scags_topvars <- wbi_scags_long %>% 
  group_by(Var1, Var2) %>%
  slice_head(n=1) %>%
  arrange(scag)

pair1 <- wbi_scags_topvars %>% filter(scag == "clumpy2")

w4 <- ggplot(wbi_wide, 
         aes_string(x=as.character(pair1$Var1),
                    y=as.character(pair1$Var2))) +
  geom_point(aes(label = `Country Code`)) +
  theme(aspect.ratio=1) +
  ggtitle(pair1$scag)

pair2 <- wbi_scags_topvars %>% filter(scag == "striated2")

w5 <- wbi_wide  %>%
  ggplot(aes_string(x=as.character(pair2$Var1),
                    y=as.character(pair2$Var2))) +
  geom_point(aes(label = `Country Code`)) +
  theme(aspect.ratio=1) +
  ggtitle(pair2$scag)

w6 <- ggplot(wbi_scags_topvars, 
             aes(x=fct_reorder(scag, value, median),
                 y=value)) +
  xlab("") + ylab("Scagnostic value") +
  geom_point() + coord_flip()


## ----wbiinteractive, fig.cap="Most of the pairs of indicators exhibit outliersor are stringy. There is one pair that has clumpy as the highest value. There are numerous pairs that have a highest value on convex.",  include=knitr::is_html_output(), eval=knitr::is_html_output(), layout = "l-body"----
#> ws1 <- ggplotly(w6)
#> ws2 <- ggplotly(w3a)
#> ws3 <- ggplotly(w3b)
#> subplot(ws1, ws2, ws3, nrows=1, widths = c(0.33, 0.33, 0.33), heights = 0.6)


## ----wbistatic, fig.cap="Most of the pairs of indicators exhibit outliersor are stringy. There is one pair that has clumpy as the highest value. There are numerous pairs that have a highest value on convex.", fig.height=12, fig.width=12, out.width="100%", include=knitr::is_latex_output(), eval=knitr::is_latex_output()----
w6 + w3a + w3b

