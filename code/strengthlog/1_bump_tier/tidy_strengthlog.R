here::i_am("code/strengthlog/1_bump_tier/tidy_strengthlog.R")
library(here)
library(tidyverse)

dir_tier_1 <- "data/tier_1/strengthlog"
dir_tier_2 <- "data/tier_2/strengthlog"
file_tier_1 <- list.files(here(dir_tier_1), recursive = TRUE)
stopifnot(length(file_tier_1) == 1)

data_raw_all <- 
  here(dir_tier_1, file_tier_1) %>% 
  read_lines()
ln_header_end <- which(data_raw_all == "Workouts")

# Names of summary columns
summary_names <- data_raw_all[ln_header_end + 1] %>% str_split(",") %>% unlist()

# Remove header
data_raw <- data_raw_all[-c(1:(ln_header_end +1))]

# Identify line-id defining start and stop for each individual workout
data_raw_tbl <- data_raw %>% enframe(name = "line", value = "text")
ln_blank <- data_raw_tbl %>% filter(text == "") %>% pull(line)
ln_workouts <- tibble(
  ln_start = c(1, ln_blank + 1),
  ln_end   = c(ln_blank - 1, data_raw_tbl$line[nrow(data_raw_tbl)])
)

get_workout <- function(ln_start, ln_end, data_raw_tbl) {
  # print(ln_start)
  # ln_start <- 527 #36
  # ln_end <- 537 #42
  
  # Line 2: comment (sometimes)
  has_comment <- 
    data_raw_tbl$text[ln_start + 1] %>% 
    str_detect("Övning", negate = TRUE)
  wo_comment <- 
    if(has_comment) {
      data_raw_tbl$text[ln_start + 1]
    } else {
      NA_character_
    }
  
  # Line 1: summary
  # First part of summary line = workout name
  # The name may contain commas, in which case the full workout name is quoted,
  # e.g. "Workout, with, commas", If the workout name does not contain commas 
  # it is not quoted.
  is_quoted <- data_raw_tbl$text[ln_start] %>% str_detect('^\\".+\\",')
  if(is_quoted) {
    wo_name <- 
      data_raw_tbl$text[ln_start] %>% 
      str_extract('\\".+\\"') %>% 
      str_remove_all('\\"')
    wo_summary_raw <- 
      data_raw_tbl$text[ln_start] %>% 
      str_remove('\\".+\\",') %>% 
      str_split(",") %>% 
      unlist() %>% 
      c(wo_name, .)
  } else {
    wo_summary_raw <- 
      data_raw_tbl$text[ln_start] %>%
      str_split(",") %>% 
      unlist() 
  }
  wo_summary <-
    wo_summary_raw %>% 
    na_if("-1") %>% 
    setNames(., summary_names) %>% 
    as.list() %>% 
    as_tibble() %>% 
    mutate(
      Datum = as.Date(Datum),
      Kroppsvikt = as.numeric(Kroppsvikt),
      across(Form:Stress, as.numeric)
    ) %>% 
    mutate(
      Kommentar = wo_comment
    )
  
  # Remaining lines: workout table
  wo_txt <- 
    if(has_comment) {
      data_raw_tbl %>% slice((ln_start + 2):ln_end) %>% pull(text)
    } else {
      data_raw_tbl %>% slice((ln_start + 1):ln_end) %>% pull(text)
    }
  
  # Randomly, the line "Instance of 'Loc'" may be inserted into a workout.
  # I don't understand it.
  wo_txt_clean <- wo_txt[!wo_txt == "Instance of 'Loc'"]
  wo_tab <- 
    wo_txt_clean %>% 
    # decimal points
    str_replace_all('(?<=\\"\\d{1,3}),(?=\\d{1,2}\\")', ".") %>%
    str_remove_all('\\"') %>% 
    str_split(",\\s*") %>% 
    map( ~
      tibble(
        var = ..1[seq(1, length(..1), by = 2)],
        val = ..1[seq(2, length(..1), by = 2)]
      ) %>% 
      pivot_wider(
        names_from = var, values_from = val
      )
    ) %>% 
    bind_rows()
  
  return(tibble(wo_summary = list(wo_summary), wo_tab = list(wo_tab)))
  
}

workouts <- 
  ln_workouts %>% 
  mutate(tmp = map2(ln_start, ln_end, get_workout, data_raw_tbl)) %>% 
  unnest(tmp) %>% 
  mutate(wo_id = nrow(.):1) %>% 
  relocate(wo_id)

dir.create(here(dir_tier_2), recursive = TRUE)
filename <- file_tier_1 %>% str_replace(".csv", ".rsd")
write_rds(workouts, here(dir_tier_2, filename))
