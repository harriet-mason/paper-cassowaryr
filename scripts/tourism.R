library(cassowaryr)
library(tsibble)
library(fabletools)
library(feasts)
library(dplyr)
library(ggplot2)
library(GGally)
library(tidyr)
library(plotly)

data(tourism)

f_std <- function(x) (x-mean(x, na.rm=TRUE)/sd(x, na.rm=TRUE))

tourism_bus_vic_qld <- tourism |>
  filter(State %in% c("Victoria", "Queensland")) |>
  filter(Purpose == "Business")

tourism_bus_hol_qld <- tourism |>
  filter(State == "Queensland") |>
  filter(Purpose %in% c("Business", "Holiday"))

tourism_bus_hol_qld <- tourism_bus_hol_qld |>
  group_by_key() |>
  mutate(Trips_std = (Trips - mean(Trips, na.rm=TRUE))/sd(Trips, na.rm=TRUE))
  
feat_slope <- function(x) {
  n <- length(x)
  t <- seq_len(n)
  slope <- coef(lm(x ~ t))[2]
  as.numeric(slope)
}

tourism_bus_hol_qld_feats1 <- tourism_bus_hol_qld |>
  features(Trips_std, features = list(mean = mean, 
                                  #sd = sd,
                                  slope = feat_slope)) 

tourism_bus_hol_qld_feats2 <- tourism_bus_hol_qld |>
  features(Trips_std, feat_stl)

tourism_bus_hol_qld_feats <- left_join(tourism_bus_hol_qld_feats1,
                                      tourism_bus_hol_qld_feats2)


tourism_bus_hol_qld_feats |>
  group_by(Purpose) |>
  summarise(calc_scags(slope, linearity, scags = "dcor"))

ggplot(tourism_bus_hol_qld_feats, aes(x=slope, y=linearity, colour=Purpose)) +
  geom_point() +
  theme(aspect.ratio=1)

tourism_bus_hol_qld_feats |>
  group_by(Purpose) |>
  summarise(calc_scags(slope, curvature, scags = "dcor"))

ggplot(tourism_bus_hol_qld_feats, aes(x=slope, y=curvature, colour=Purpose)) +
  geom_point() +
  theme(aspect.ratio=1)

ggscatmat(tourism_bus_hol_qld_feats, 
          columns = 4:7, #4:14, 
          color = "Purpose") +
  theme(axis.text = element_blank())

scags_hol <- tourism_bus_hol_qld_feats |>
  filter(Purpose == "Holiday") |>
  select(slope:stl_e_acf10) |>
  calc_scags_wide(scags = c("outlying", "stringy", "striated", 
                            "striated2", "clumpy", "clumpy2",
                            "sparse", "skewed", "convex", 
                            "skinny", "monotonic", "splines", 
                            "dcor")) #|>
  #select(-stripes) # why does stripes get calculated?

scags_bus <- tourism_bus_hol_qld_feats |>
  filter(Purpose == "Business") |>
  select(slope:stl_e_acf10) |>
  calc_scags_wide(scags = c("outlying", "stringy", "striated", 
                            "striated2", "clumpy", "clumpy2",
                            "sparse", "skewed", "convex", 
                            "skinny", "monotonic", "splines", 
                            "dcor")) |>
  select(-stripes) # why does stripes get calculated?

scags_hol <- scags_hol |>
  pivot_longer(outlying:dcor, 
               names_to = "scags",
               values_to = "value") |>
  mutate(Purpose = "Holiday")

scags_bus <- scags_bus |>
  pivot_longer(outlying:dcor, 
               names_to = "scags",
               values_to = "value") |>
  mutate(Purpose = "Business")

scags_tourism <- bind_rows(scags_hol, scags_bus)
scags_tourism <- scags_tourism |>
  pivot_wider(names_from = Purpose, values_from = value)

scags_tourism <- scags_tourism |>
  mutate(scag_dif = abs(Holiday-Business)) |>
  group_by(scags) |>
  arrange(desc(scag_dif))

ggplot(scags_tourism, aes(x=Holiday, 
                          y=Business,
                          label = paste(Var1, Var2))) +
  geom_point() + 
  xlim(c(0, 1)) + ylim(c(0, 1)) +
  theme(aspect.ratio=1)

ggplotly()

ggplot(scags_tourism, aes(x=Holiday, 
                          y=Business)) +
  geom_point() + 
  facet_grid(Var1~Var2) +
  xlim(c(0, 1)) + ylim(c(0, 1)) +
  theme(aspect.ratio=1,
        axis.text = element_blank())

ggplot(tourism_bus_hol_qld, aes(x=Quarter, 
                                y=Trips_std,
                                colour=Purpose)) + 
  geom_line(aes(group=Region), alpha=0.1) + 
  geom_smooth(se=F, span=0.05) +
  facet_wrap(~Purpose, ncol=1)

ggplot(tourism_bus_hol_qld, aes(x=Quarter, 
                                y=Trips_std,
                                colour=Purpose)) + 
  geom_line(aes(group=Region)) + 
  facet_grid(Region~Purpose) +
  theme(axis.text.y = element_blank())
