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


## ---- Data Cleaning and Scag Calculation, include=FALSE-----------------------
# edit data
set.seed(29171741)
# variables to use in making the plots

# generate circle data
theta <- runif(150, 0, 2*pi)
r1 <- rbeta(150, 3, 2)
r2 <- rbeta(150, 10, 1)

# generate striated x
vlx <- sample(c(1,2,3), 150, replace=TRUE)

# generate outlying data
outx <- c(rnorm(147, 0,1), 0, 10, 10)
outy <- c(rnorm(147, 0,1), 10, 10, 0)

# formula data
liney <- 2*theta + 2
nonline <- 2*theta^3 - 10*theta^2 - 5*theta +  8


extrafeatures <- tibble(feature = c(rep("disk", 150), rep("ring", 150), rep("vlines", 150), 
                                    rep("outliers2", 150), rep("line", 150), rep("nonlinear1", 150)),
                        x = c(r1*cos(theta), r2*cos(theta), vlx, outx, theta, theta),
                        y = c(r1*sin(theta), r2*sin(theta), theta, outy, liney, nonline)
                        )

#combine with current features
bigfeatures <- bind_rows(features, extrafeatures)

# run scagnostics
features_scagnostics_wide <- bigfeatures %>%
  group_by(feature) %>%
  summarise(calc_scags(x,y)) #%>%
  #select(-clumpy_adjusted)

#long version of
features_scagnostics_long <- features_scagnostics_wide %>%
  pivot_longer(cols=outlying:dcor, names_to = "scagnostic")

#transpose of wide feature scagnostics table
t_features_scagnostics_wide <- features_scagnostics_long %>%
  pivot_wider(names_from = "feature")




## ---- Features plot-----------------------------------------------------------
#plot them
featplot <- ggplot(bigfeatures, aes(x,y,colour=feature))+
  geom_point() +
  theme_minimal() + 
  facet_wrap(~feature, ncol=5, scales="free") +
  xlab("") + ylab("") +
  theme(legend.position = "none", 
        aspect.ratio= 1, 
        axis.text = element_blank())
featplot


## ---- Scatter Plots as images,  include=FALSE, eval=FALSE---------------------
#> 
#> #set theme so all scatter plots in table match
#> plot_theme <-  theme_classic() + #theme_minimal() +
#>   theme(aspect.ratio=1, axis.title=element_blank(), axis.text = element_blank(),
#>         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#>         panel.border = element_rect(colour = "black", fill=NA, size=4),
#>         legend.position = "none"
#>         )
#> 
#> #save scatter plots as images
#> plots <- unique(bigfeatures$feature)
#> 
#> for (i in seq(length(plots))){
#>   holdplot <- bigfeatures %>%
#>     filter(feature==plots[i]) %>%
#>     ggplot(aes(x,y, size=2))+ geom_point() + plot_theme
#>   # ggsave(paste0("figures/", plots[i], ".png"),holdplot) files already in /figure/
#> }
#> 


## ---- Table of Plots, fig.height=10, fig.width=10-----------------------------

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
#ggsave("figures/visual_table.png", visual_table, width=10, height=10)
visual_table



## ----  Striated Comparison, fig.height=5, fig.width=10------------------------
# Make Visual Table
# Data
plot_data_striated <- plot_path_data %>%
  group_by(scagnostic, feature) %>%
  mutate(doplot = ifelse(all(scagnostic %in% c("striated","striated_adjusted"), 
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
#ggsave("figures/striated_visual_table.png", striated_visual_table, width=10, height=10)

striated_visual_table



## ----  Clumpy Comparison, fig.height=5, fig.width=10--------------------------
plot_data_clumpy <- plot_path_data %>%
  group_by(scagnostic, feature) %>%
  mutate(doplot = ifelse(all(scagnostic %in% c("clumpy","clumpy_adjusted"), 
                             feature %in% c("vlines", "clusters","barrier", "outliers", "nonlinear1", 	
"nonlinear2")),
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
#ggsave("figures/clumpy_visual_table.png", clumpy_visual_table, width=10, height=10)

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
#> load("data/cets_macro.rda")
#> feats_macro <- purrr::map_dfr(cets_macro, get_features)
#> save(feats_macro, file="data/feats_macro.rda")
#> 
#> load("data/cets_micro.rda")
#> feats_micro <- purrr::map_dfr(cets_micro, get_features)
#> save(feats_micro, file = "data/feats_micro.rda")


## -----------------------------------------------------------------------------
load("data/feats_macro.rda")
load("data/feats_micro.rda")

# Check scale of each variable
#feats_macro %>% 
#  summarise_all(sd, na.rm=TRUE) %>% 
#  glimpse()

scags_macro <- calc_scags_wide(feats_macro[,1:4], 
  scags = c("convex", "splines", "skinny",
            "outlying", "stringy", "striated",
            "clumpy","sparse", "skewed"))

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
  theme(aspect.ratio=1) + 
  ggtitle("Boring") # So boring

p2 <- ggplot(feats_mac_mic, 
       aes(x=curvature, y=trend_strength, colour = type)) + 
  geom_point(alpha = 0.5) +
  scale_colour_brewer("", palette="Dark2") +
  theme(aspect.ratio=1) + 
  ggtitle("Interesting") # Interesting

p3 <- ggplot(feats_mac_mic, 
       aes(x=curvature, y=linearity, colour = type)) + 
  geom_point(alpha = 0.5) +
  scale_colour_brewer("", palette="Dark2") +
  theme(aspect.ratio=1) + 
  ggtitle("Interesting?") # Sort of interesting

p1 + p2 + p3 + plot_layout(guides = "collect") & theme(legend.position = "bottom")


## -----------------------------------------------------------------------------
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
#> scag_bbh <- calc_scags_wide(bbh)
#> 
#> bbh1 <- ggplot(scag_bbh, aes(x=convex, y=skinny,
#>                       label = paste(Var1, Var2))) +
#>   geom_point()
#> bbh2 <- ggplot(scag_bbh, aes(x=dcor, y=splines,
#>                       label = paste(Var1, Var2))) +
#>   geom_point()
#> bbh3 <- ggplot(scag_bbh, aes(x=clumpy, y=skewed,
#>                       label = paste(Var1, Var2))) +
#>   geom_point()
#> gs1 <- ggplotly(bbh1)
#> gs2 <- ggplotly(bbh2)
#> gs3 <- ggplotly(bbh3)
#> subplot(gs1, gs2, gs3, nrows=1, widths = c(0.33, 0.33, 0.33), heights = 0.6)


## ----eval=FALSE---------------------------------------------------------------
#> 
#> bbh1 <- ggplot(bbh, aes(x=time, y=ra)) +
#>   geom_point()
#> bbh2 <- ggplot(bbh, aes(x=dec, y=ra)) +
#>   geom_point()
#> bbh3 <- ggplot(bbh, aes(x=dec, y=time)) +
#>   geom_point()
#> bbh4 <- ggplot(bbh, aes(x=m1, y=m2)) +
#>   geom_point()
#> bbh5 <- ggplot(bbh, aes(x=chi_p, y=chi_tot)) +
#>   geom_point()
#> bbh6 <- ggplot(bbh, aes(x=time, y=alpha)) +
#>   geom_point()
#> 
#> subplot(bbh1, bbh2, bbh3, bbh4, bbh5, bbh6,
#>         nrows=2, widths = c(0.33, 0.33, 0.33), heights = c(0.5, 0.5))


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


## ---- eval=FALSE--------------------------------------------------------------
#> # highest on each
#> 
#> # Outlying (high)
#> ggplot(aflw, aes(x=	metresGained, y= bounces)) + geom_point()
#> #metresGained has a large number of very close variables that take up most of the MST, and then a handful of higher up ones (it has outliers on this one variable). Anything with meters gained will be high on outlying
#> ggplot(aflw, aes(x=	dreamTeamPoints, y= contestedMarks)) + geom_point()
#> 
#> # Stringy
#> ggplot(aflw, aes(x=clearances.centreClearances, y=contestedMarks)) + geom_point() #lowest
#> ggplot(aflw, aes(x=metresGained, y=goalAssists)) + geom_point() #highest
#> 
#> # Striated
#> ggplot(aflw, aes(x=clearances.stoppageClearances, y=shotsAtGoal)) + geom_point() #lowest
#> #highest is same as stringy
#> 
#> #Striated adjusted
#> #discussed below
#> 
#> #Clumpy
#> ggplot(aflw, aes(x=behinds, y=goals)) + geom_point() #lowest
#> ggplot(aflw, aes(x=	metresGained, y= shotsAtGoal)) + geom_point() #highest
#> 
#> #clumpy_adjusted
#> cl_a <- calc_scags_wide(aflw_num[,5:37], scags="clumpy_adjusted")
#> ggplot(aflw, aes(x=metresGained, y=goalAssists)) + geom_point()  #highest
#> ggplot(aflw, aes(x=	clearances.centreClearances, y= behinds)) + geom_point() #lowest
#> 
#> #Sparse
#> ggplot(aflw, aes(x=	goalAssists, y= clangers)) + geom_point() #highest
#> 
#> #Skewed
#> ggplot(aflw, aes(x=	tacklesInside50, y= marksInside50)) + geom_point() #highest
#> 
#> 
#> 


## ---- eval=FALSE--------------------------------------------------------------
#> # im just adding in striated_adjusted here to compare without recalculating the whole thing
#> new_scag_aflw <- scag_aflw
#> new_scag_aflw$striated_adjusted <- AFL_stri_adj$striated_adjusted #calc_scags_wide(aflw_num[,5:37], scags = "striated_adjusted")
#> 
#> scag_aflw %>%
#>   select(Var1, Var2, striated) %>%
#>   arrange(desc(striated)) %>%
#>   head(10)
#> 
#> # look at lowest values for adjusted striated value
#> lowest_stri <- new_scag_aflw[order(new_scag_aflw$striated),] %>%
#>   select(Var1, Var2, striated, striated_adjusted) %>%
#>   head(10)
#> 
#> lowest_stri_adjusted <- new_scag_aflw[order(new_scag_aflw$striated_adjusted),] %>%
#>   select(Var1, Var2, striated, striated_adjusted) %>%
#>   head(10)
#> 
#> ggplot(aflw, aes(x=metresGained, y=goals)) + geom_point()
#> 
#> #look at top 3 for striated
#> ggplot(aflw, aes(x=clearances.stoppageClearances, y=shotsAtGoal)) + geom_point()
#> ggplot(aflw, aes(x=contestedPossessions, y=	tackles)) + geom_point()
#> ggplot(aflw, aes(x=tacklesInside50, y=marksInside50)) + geom_point()
#> #not interesting
#> 
#> # look at top 3 for striated_adjusted
#> # two continuous variables
#> ggplot(aflw, aes(x=metresGained, y=dreamTeamPoints)) + geom_point()
#> # fractional plots, not interesting technically but they are interesting visually
#> ggplot(aflw, aes(x=goalAccuracy, y=shotsAtGoal)) + geom_point()
#> ggplot(aflw, aes(x=disposalEfficiency, y=disposals)) + geom_point()
#> 
#> #THE OTHER 6 PLOTS
#> ggplot(aflw, aes(x=metresGained, y=disposalEfficiency)) + geom_point() #fraction thing issue again
#> ggplot(aflw, aes(x=dreamTeamPoints, y=disposalEfficiency)) + geom_point()
#> # this is a plot that will have a higher striated value with more data
#> ggplot(aflw, aes(x=	goals, y= goalAccuracy)) + geom_point()
#> ggplot(aflw, aes(x=dreamTeamPoints, y=hitouts)) + geom_point() # 1 with more data
#> ggplot(aflw, aes(x=disposalEfficiency, y=hitouts)) + geom_point() #fraction and large discrete
#> ggplot(aflw, aes(x=totalPossessions, y=hitouts)) + geom_point() #two large discretes
#> ggplot(aflw, aes(x=hitouts, y=disposals)) + geom_point() #two large discretes


## ----eval=FALSE---------------------------------------------------------------
#> library(readxl)
#> wbi <- read_xlsx("data/World_Development_Indicators.xlsx")
#> wbi_wide <- wbi %>%
#>   mutate(`2018 [YR2018]` = as.numeric(`2018 [YR2018]`)) %>%
#>   select(`Country Code`, `Series Code`, `2018 [YR2018]`) %>%
#>   pivot_wider(names_from = `Series Code`, values_from = `2018 [YR2018]`, id_cols = `Country Code`, values_fn = mean) %>%
#>   filter(!is.na(`Country Code`))
#> 
#> wbi_wide_sub <- wbi_wide[, c(1, 2, 3, 6, 11, 13:22, 24, 25, 27:30, 32:34, 37:40)]
#> summary(wbi_wide_sub)
#> 
#> # Remove any variables that are the same
#> scag_cor <- calc_scags_wide(wbi_wide_sub[,-1],
#>     scags = "monotonic")
#> wbi_wide_sub <- wbi_wide_sub %>%
#>   select(-NY.GDP.MKTP.CD)
#> 
#> scag_wbi <- calc_scags_wide(wbi_wide_sub[,-1],
#>     scags = c("outlying", "stringy",
#>               "striated", "skewed",
#>               "convex", "skinny",
#>               "splines"))
#> 
#> scag_wbi_long <- scag_wbi %>%
#>   pivot_longer(!c(Var1, Var2), names_to = "scag", values_to = "value") %>%
#>   arrange(desc(value))
#> 
#> ggplot(wbi_wide_sub,
#>        aes_string(x=as.character(scag_wbi_long$Var1[1]),
#>                   y=as.character(scag_wbi_long$Var2[1]))) +
#>   geom_point()
#> 
#> ggplot(wbi_wide_sub,
#>        aes_string(x=as.character(scag_wbi_long$Var1[1]),
#>                   y=as.character(scag_wbi_long$Var2[1]))) +
#>   geom_point()
#> 

```{.r .distill-force-highlighting-css}
```
