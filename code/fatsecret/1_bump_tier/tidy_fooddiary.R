here::i_am("code/fatsecret/1_bump_tier/tidy_fooddiary.R")
library(here)
library(tidyverse)
source("code/fatsecret/1_bump_tier/tidy_fooddiary_utilities.R")

force_reparse <- FALSE

# List files to tidy ----
get_files_diff <- function(t1, t2) {
  # tier 1 dates
  t1_formatted <- 
    enframe(t1, name = NULL, value = "filename") %>% 
    mutate(
      year = str_extract(filename, "^\\d{4}"), 
      mth_text = str_extract(filename, "(?<=FoodDiary_)\\w{3}"), 
      mth_number = case_when(
        mth_text == "Jan" ~ "01",
        mth_text == "Feb" ~ "02",
        mth_text == "Mar" ~ "03",
        mth_text == "Apr" ~ "04",
        mth_text == "May" ~ "05", 
        mth_text == "Jun" ~ "06",
        mth_text == "Jul" ~ "07",
        mth_text == "Aug" ~ "08", 
        mth_text == "Sep" ~ "09",
        mth_text == "Oct" ~ "10",
        mth_text == "Nov" ~ "11",
        mth_text == "Dec" ~ "12"
      )
    )
  
  # tier 2 dates
  t2_formatted <- 
    enframe(t2, name = NULL, value = "filename") %>% 
    mutate(
      year = str_extract(filename, "\\d{4}"),
      mth_number = basename(filename) %>% str_extract("(?<=\\d{4}_)\\d{2}")
    )
  
  filediff <- 
    anti_join(t1_formatted, t2_formatted, by = c("year", "mth_number")) %>% 
    pull(filename)
  
}
dir_tier_1 <- "data/tier_1/fatsecret"
dir_tier_2 <- "data/tier_2/fatsecret"
files_tier_1 <- list.files(here(dir_tier_1), recursive = TRUE)
files_tier_2 <- list.files(here(dir_tier_2), recursive = TRUE)

# Parse 
files_to_bump <- 
  if (force_reparse  | length(files_tier_2) == 0) {
    files_tier_1
  } else {
    get_files_diff(files_tier_1, files_tier_2)
  }

dir.create(here(dir_tier_2, "sums"), recursive = TRUE)
files_to_bump %>% 
  here(dir_tier_1, .) %>% 
  walk(., parse_sums, dir_out = here(dir_tier_2, "sums"))

dir.create(here(dir_tier_2, "items"), recursive = TRUE)
files_to_bump %>% 
  here(dir_tier_1, .) %>% 
  walk(., parse_items, dir_out = here(dir_tier_2, "items"))
