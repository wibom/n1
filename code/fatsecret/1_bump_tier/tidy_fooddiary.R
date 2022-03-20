here::i_am("code/fatsecret/1_bump_tier/tidy_fooddiary.R")
library(here)
library(tidyverse)

# List files to tidy ----
dir_tier_1 <- "data/tier_1/fatsecret"
dir_tier_2 <- "data/tier_2/fatsecret"
files_tier_1 <- list.files(here(dir_tier_1), recursive = TRUE)

# Parse sums
source("code/fatsecret/1_bump_tier/parse_sums.R")
dir.create(here(dir_tier_2, "sums"), recursive = TRUE)
files_tier_1[1:2] %>% 
  here(dir_tier_1, .) %>% 
  walk(., parse_sums, dir_out = here(dir_tier_2, "sums"))

dir.create(here(dir_tier_2, "items"), recursive = TRUE)
files_tier_1[1:2] %>% 
  here(dir_tier_1, .) %>% 
  walk(., parse_items, dir_out = here(dir_tier_2, "items"))

# add force_reparse functionallity... :)
# add impute functionality... :)