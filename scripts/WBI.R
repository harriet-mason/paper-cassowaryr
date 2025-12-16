# Pre-processing code
library(cassowaryr)
library(readxl)
library(tidyr)
library(dplyr)
wbi <- read_xlsx("data/World_Development_Indicators.xlsx", n_max = 2436) # Cut off extra stuff

wbi_wide <- wbi |> 
  mutate(`2018 [YR2018]` = str_replace(`2018 [YR2018]`, "..","")) |>
  mutate(`2018 [YR2018]` = as.numeric(`2018 [YR2018]`)) |>
  select(`Country Code`, `Series Code`, `2018 [YR2018]`) |>
  pivot_wider(names_from = `Series Code`, values_from = `2018 [YR2018]`, id_cols = `Country Code`, values_fn = mean) |>
  filter(!is.na(`Country Code`))

# Drop variables with missings, or too similar 
library(naniar)
s_miss <- miss_summary(wbi_wide)
gg_miss_var(wbi_wide)

# Remove vars with more than 15 missing values
drop <- s_miss$miss_var_summary[[1]] |>
  filter(pct_miss > 15)
wbi_wide <- wbi_wide |>
  select(-drop$variable)

# Remove countries with more than 10 missing values
drop_cnt <- s_miss$miss_case_summary[[1]] |>
  filter(pct_miss > 10)
wbi_wide <- wbi_wide[-drop_cnt$case,]

# Check again - still a few sporadic missings
# The missing value plot shows no missing pattern
s_miss <- miss_summary(wbi_wide)

vis_miss(wbi_wide)

# Remove any variables that are the same
#scag_cor <- calc_scags_wide(wbi_wide_sub[,-1], 
#    scags = "monotonic")
#wbi_wide_sub <- wbi_wide_sub |>
#  select(-NY.GDP.MKTP.CD)
wbi_wide <- wbi_wide[,-8] # TX.VAL.TECH.MF.ZS too few values, almost all zero
save(wbi_wide, file="data/wbi_wide.rda")

# Check individual scag calculations, for testing
calc_scags(wbi_wide$EN.ATM.CO2E.PC, wbi_wide$NV.AGR.TOTL.ZS, scags = "striped")

wbi_scags <- calc_scags_wide(wbi_wide[,-1],
                             scags = c("outlying", "stringy",
                                       "striated2", "clumpy2", "skewed",
                                       "convex", "skinny",
                                       "splines", "striped"), out.rm=FALSE)
save(wbi_scags, file="data/wbi_scags.rda")