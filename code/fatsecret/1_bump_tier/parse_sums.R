library(tidyverse)
library(lubridate)
library(glue)
get_diary_header <- function(diary_file) {
  # read and trim csv-file
  diary <-
    readLines(diary_file, encoding = "UTF-8", warn = FALSE) %>%
    enframe(name = "line", "txt_csv")
  
  # Identify report-header line and trim all text above
  header_idx <- which(diary$txt_csv == "# Report Details") + 2
  header <-
    diary$txt_csv[header_idx] %>%
    str_split(",") %>%
    unlist() %>%
    str_remove_all("\\s{1,}") %>%
    str_replace("\\(", " \\(")
  
}
read_diary <- function(diary_file) {
  # read and trim csv-file
  diary <-
    readLines(diary_file, encoding = "UTF-8", warn = FALSE) %>%
    enframe(name = "line", "txt_csv")
  
  # Identify report-header line and trim all text above
  header_idx <- which(diary$txt_csv == "# Report Details") + 2
  diary <- diary %>% slice(header_idx:n())
  
  # drop last line with whole month's`Total`
  diary <- diary %>% head(-1)
  
}
get_dates_lnidx <- function(diary) {
  # Identify line-id defining start and stop for each individual date
  ln_start <-
    diary %>%
    filter(
      str_detect(
        txt_csv,
        #'^\\S*måndag|tisdag|onsdag|torsdag|fredag|lördag|söndag'
        '^\\S*Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday'
      )
    ) %>%
    pull(line)
  date_startstop <- tibble(
    ln_start = ln_start,
    ln_stop  = c(ln_start[-1] -1, diary$line[nrow(diary)])
  )
}
get_meal_lnidx <- function(diary_date) {
  # start/stop lines for each meal
  ln_start_meals <- c(
    date_tot = diary_date %>% slice(1) %>% pull(line),
    breakfast =
      diary_date %>%
      filter(str_detect(txt_csv, '^ Breakfast')) %>%
      pull(line),
    lunch =
      diary_date %>%
      filter(str_detect(txt_csv, '^ Lunch')) %>%
      pull(line),
    dinner =
      diary_date %>%
      filter(str_detect(txt_csv, '^ Dinner')) %>%
      pull(line),
    snacks =
      diary_date %>%
      filter(str_detect(txt_csv, '^ Snacks/Other')) %>%
      pull(line)
  )
  ln_idx_meals <-
    enframe(ln_start_meals, name = "meal", value = "ln_start") %>%
    mutate(
      ln_stop = c(
        ln_start_meals[1],
        ln_start_meals[-c(1:2)] -1,
        diary_date$line[nrow(diary_date)]
      )
    )
}
format_date <- function(diary_date) {
  diary_date$txt_csv[1] %>%
    str_extract('[[:alnum:]\\,\\s]+') %>%
    tibble(date_txt = .) %>%
    mutate(
      parsed_txt = str_split(date_txt, "(, | )"),
      wday     = parsed_txt[[1]][1],
      mth_txt  = parsed_txt[[1]][2],
      date     = parsed_txt[[1]][3],
      yr       = parsed_txt[[1]][4],
      mth = case_when(
        mth_txt == "January"   ~ "01",
        mth_txt == "February"  ~ "02",
        mth_txt == "Mars"      ~ "03",
        mth_txt == "April"     ~ "04",
        mth_txt == "May"       ~ "05",
        mth_txt == "June"      ~ "06",
        mth_txt == "July"      ~ "07",
        mth_txt == "August"   ~ "08",
        mth_txt == "September" ~ "09",
        mth_txt == "October"   ~ "10",
        mth_txt == "November"  ~ "11",
        mth_txt == "December"  ~ "12"
      ),
      date_iso = str_c(yr, mth, date, sep = "-") %>% lubridate::as_date()
    ) %>%
    select(date_iso, mth_txt, wday)
}
make_missingness_explicit <- function(ds, sums_or_items = "items") {

  # All dates/meals
  if (sums_or_items == "items") {
    meals <- c("breakfast", "lunch", "dinner", "snacks")
  } else if (sums_or_items == "sums") {
    meals <- c("breakfast", "lunch", "dinner", "snacks", "all_meals")
  }
  
  all_datesmeals <-
    list(
      date =
        seq(
          from = ds$date[1],
          to   = ds$date[nrow(ds)],
          by = "1 days"
        ) %>%
        # https://community.rstudio.com/t/
        # how-can-i-make-my-dates-maintain-their-type-in-after-a-purrr-cross-df/
        # 18951/2
        as.character(),
      meal = meals
    ) %>%
    cross_df() %>%
    mutate(date = lubridate::ymd(date)) %>%
    arrange(date) %>%
    mutate(
      year = lubridate::year(date),
      mth = lubridate::month(date) %>% str_pad(width = 2, pad = "0"),
      mth_txt = case_when(
        mth == "01" ~ "January",
        mth == "02" ~ "February",
        mth == "03" ~ "Mars",
        mth == "04" ~ "April",
        mth == "05" ~ "Maj",
        mth == "06" ~ "June",
        mth == "07" ~ "July",
        mth == "08" ~ "August",
        mth == "09" ~ "September",
        mth == "10" ~ "October",
        mth == "11" ~ "November",
        mth == "12" ~ "December"
      ),
      week = lubridate::week(date),
      wday = lubridate::wday(
        date, 
        label = TRUE, 
        abbr = FALSE, 
        locale = "english"
      )
    ) %>%
    select(year, mth, mth_txt, week, date, wday, meal)
  
  # Explicit dates/meals with missing diary entries
  full_join(
    all_datesmeals,
    ds,
    by = c("year", "mth_txt", "week", "date", "wday", "meal")
  )
}
format_units <- function(d_items) {
  # identify patterns ----
  # `amount_txt` for each item is typically on one of four patterns
  ds <-
    d_items %>%
    mutate(
      rid = row_number(),
      amount_txt_formated =
        amount_txt %>%
        str_remove_all('\\"') %>%
        str_trim() %>%
        # remove size specifications, e.g. '(7 cm i diameter)'
        str_remove("\\(.+(cm).*\\)") %>%
        str_remove("\\(ca.+\\)") %>%
        # format 'av 150 ml' to '(150 ml)', to conform with majority
        str_replace("av (\\d+) ml", "\\(\\1 ml\\)") %>%
        # format '(200 g per portion)' to (200 g), to conform with majority
        str_remove(" per portion") %>%
        # fyror
        str_replace("(\\d+) ((fyra|fyror))", "\\1 \\2 (4 cl)") %>%
        # format to decimal numbers
        str_replace("(\\d+)?\\s?1/2", "\\1.5") %>%
        str_replace("(\\d+)?\\s?1/3", "\\1.33") %>%
        str_replace("(\\d+)?\\s?2/3", "\\1.66") %>%
        str_replace("(\\d+)?\\s?1/4", "\\1.25") %>%
        str_replace("(\\d+)?\\s?3/4", "\\1.75") %>%
        # special cases
        str_replace("(\\d+)cm", "\\1 cm") %>%
        str_replace("0;2 frukt", "0.2 frukt"),
      pat = case_when(
        # e.g. '20 g'
        str_detect(
          amount_txt_formated,
          "^\\d+ (g|ml|cl)$"
        ) ~ "pat1",
        
        # e.g. '1 kubb; 26.3 g', '2 x 1 rad (4 rutor); 60 g'
        str_detect(
          amount_txt_formated,
          "^[[:digit:]\\.]+ (x \\d )?.+; [[:digit:]\\.]+ (g|ml|cl)"
        ) ~ "pat2",
        
        # e.g. '2 burkar (330 ml)', '1 portion (255 g)'
        str_detect(
          amount_txt_formated,
          "^[[:digit:]\\.]+ .+ \\([[:digit:]\\.]+ (g|ml|cl)\\)"
        ) ~ "pat3",
        
        # e.g. '0.5 burk'
        str_detect(
          amount_txt_formated,
          "^[[:digit:]\\.]+ .+"
        ) ~ "pat4",
        
        TRUE ~ "none"
      )
    )
  
  # format `amount` ----
  # parse `amount_txt` text string to define the variables:
  #  - no:        number of units
  #  - a:         quantity of a unit
  #  - amount:    amount :), e.g. "70"
  #  - unit:      the unit of `amount`, e.g. "g"
  #  - unit_name: e.g. "glas", "potatisar"...
  format_pat_1 <- function(d_pat) {
    d_pat %>%
      mutate(
        no = NA_real_,
        a = NA_real_,
        amount =
          amount_txt_formated %>%
          str_extract("^[[:digit:]\\.]+") %>%
          as.numeric(),
        unit =
          amount_txt_formated %>%
          str_extract("\\w+$"),
        unit_name = NA_character_
      )
  }
  format_pat_2 <- function(d_pat) {
    # Den totala mängden anges
    d_pat %>%
      mutate(
        no =
          amount_txt_formated %>%
          str_extract("^[[:digit:]\\.]+") %>%
          as.numeric(),
        amount =
          amount_txt_formated %>%
          str_extract("(?<=;\\s)[[:digit:]\\.]+") %>%
          as.numeric(),
        a =
          amount / no,
        unit =
          amount_txt_formated %>%
          str_extract("\\S+$"),
        unit_name =
          amount_txt_formated %>%
          str_extract("\\S+(?=;)")
      )
  }
  format_pat_3 <- function(d_pat) {
    # Mängden per enhet anges
    d_pat %>%
      mutate(
        no =
          amount_txt_formated %>%
          str_extract("^[[:digit:]\\.]+") %>%
          as.numeric(),
        a =
          amount_txt_formated %>%
          str_extract("(?<=\\()[[:digit:]\\.]+(?= (g|ml|cl)\\))") %>%
          as.numeric(),
        amount =
          no * a,
        unit =
          amount_txt_formated %>%
          str_extract("(?<=[:digit:] )(g|ml|cl)(?=\\))"),
        unit_name =
          amount_txt_formated %>%
          str_extract("\\S+(?= \\()"),
      )
  }
  format_pat_4 <- function(d_pat) {
    # Ingen angiven mängd
    d_pat %>%
      mutate(
        no =
          amount_txt_formated %>%
          str_extract("^[[:digit:]\\.]+") %>%
          as.numeric(),
        a = NA_real_,
        amount = NA_real_,
        unit =
          amount_txt_formated %>%
          str_extract("(?<=[:digit:] )(g|ml|cl)(?=\\))"),
        unit_name =
          amount_txt_formated %>%
          str_extract("(?<=[:digit:] ).+$"),
      )
  }
  format_pat_none <- function(d_pat) {
    # Ingen info
    d_pat %>%
      mutate(
        no = NA_real_,
        a = NA_real_,
        amount = NA_real_,
        unit = NA_character_,
        unit_name = NA_character_
      )
  }
  ds_formated <-
    ds %>%
    group_by(pat) %>%
    nest() %>%
    mutate(
      data_formated = case_when(
        pat == "pat1" ~ map(data, format_pat_1),
        pat == "pat2" ~ map(data, format_pat_2),
        pat == "pat3" ~ map(data, format_pat_3),
        pat == "pat4" ~ map(data, format_pat_4),
        pat == "none" ~ map(data, format_pat_none)
      )
    )
  
  ds_formated %>%
    select(-data) %>%
    unnest(data_formated) %>%
    ungroup() %>%
    select(
      rid, year:Item, amount_txt, amount_txt_formated, no, a, amount, unit,
      unit_name, everything()
    )
}
categorize_items <- function(d_items) {
  # Categorize Items into larger categories
  d_items %>%
    mutate(
      itemtmp = str_to_lower(Item),
      Item_cat = case_when(
        is.na(itemtmp) ~ NA_character_,
        str_detect(
          itemtmp,
          glue(
            "(\\
            mjölk|ost|bregott|norrgott|grädde|fraiche|halloumi|haloumi|\\
            manchego|keso|ägg|mozzarella|glass|philadelphia|smör|yoggi|\\
            yoghurt|gräddfil\\
            )"
          )
        ) ~ "Dairy",
        
        str_detect(
          itemtmp,
          glue(
            "(\\
            sås|sylt|ketchup|sauce|senap|soya|marmelad|äppelmos|balsamico|\\
            kaviar|dressing|majonäs|majonnäs|aioli|chutney\\
            )"
          )
        ) ~ "Sås/Sylt/Etc",
        
        str_detect(
          itemtmp,
          "(öl|vin|whisky|gin|martini|vodka|champagne)"
        ) ~ "Alkohol",
        
        str_detect(
          itemtmp,
          glue(
            "(\\
            morötter|kål|sallad|champinjoner|svamp|paprika|avokado|grönsaker|\\
            tomat|broccoli|lök|rödbet|gurka|squash|pak choi|bönor\\
            )"
          )
        ) ~ "Grönsaker",
        
        str_detect(
          itemtmp,
          glue(
            "(\\
            äpple|banan|apelsin|päron|jordgubbar|clementin|hallon|magno|\\
            Satsumas\\
            )"
          )
        ) ~ "Frukt",
        
        str_detect(
          itemtmp,
          glue(
            "(\\
            pasta|ris|spaghetti|bulgur|couscous|makaroner|nudlar|tortellini|\\
            fusilli\\
            )"
          )
        ) ~ "Pasta/Ris",
        
        str_detect(
          itemtmp,
          glue(
            "(\\
            nötter|lindt|godis|choklad|after eight|fazer|gott och blandat|\\
            snickers|nötmix|salta pinnar\\
            )"
          )
        ) ~ "Godis",
        
        str_detect(
          itemtmp,
          glue(
            "(\\
            korv|kött|kyckling|fläskfilé|entrecote|bacon|scan|biff|skinka|\\
            pork|pastej|karré|oxfilé|lammskav|viltskav|hjort|falafel|\\
            lamm|serrano|salsiccia|kalkon|salami|chorizo\\
            )"
          )
        ) ~ "Charkuteri",
        
        str_detect(
          itemtmp,
          glue(
            "(\\
            lax|torsk|fisk|sej|sushi|räkor|röding|citronbitare|fish\\
            )"
          )
        ) ~ "Fisk",
        
        str_detect(
          itemtmp,
          glue(
            "(\\
            bröd|limpa|toast|hönökaka|scone|rågrut|lingongrova|skorp|baguette|\\
            knäcke|pärlan vete|bagel|tortilla|baugette|hönö råg|råg rut|fralla\\
            )"
          )
        ) ~ "Bröd",
        
        str_detect(
          itemtmp,
          glue(
            "(\\
            bulle|kaka|tårta|singoalla|ballerina|kubb|paj|finska pinnar|\\
            digestive|kärleksmums|havreflarn|lu tuc|jätten|saltiner|semla|\\
            hallongrotta|biscotti|brago|lussekatt|muffin|mazarin\\
            )"
          )
        ) ~ "Bulle/Kaka",
        
        str_detect(itemtmp, "(havregryn|gröt)") ~ "Gröt",
        str_detect(itemtmp, "whey-100") ~ "Protein",
        str_detect(itemtmp, "(müsli|musli|axa apple|russin)") ~ "Müsli",
        str_detect(itemtmp, "(potatis|pommes|gnocchi)") ~ "Potatis/Pommes",
        TRUE ~ "Other"
      )
    ) %>%
    select(-itemtmp)
}

impute <- function(ds, items_or_sums = "items") {
  impute_meal <- function(meal_data, all_meals, ...) {
    
    l <- list(...)
    if ("reg_or_imputed" %in% names(meal_data)) {
      # This meal is seen previously - return untouched
      return (meal_data)
    } else if (any(!is.na(meal_data$Item))) {
      # Meal is registered; add `reg_or_imputed`
      meal_data_2 <- meal_data %>% mutate(reg_or_imputed = "registered")
      return (meal_data_2)
    } else {
      # Impute missing meal
      # Pick at random a _registered_ meal that fulfills:
      # 1) same type of meal
      # 2) same weekday
      # 3) within +/- 3 weeks of meal of interest
      set.seed(42)
      meal_data_2 <-
        all_meals %>%
        ungroup() %>%
        filter(
          meal == l$meal,
          wday == l$wday,
          year == l$year,
          between(week, l$week - 3, l$week + 3)
        ) %>%
        mutate(
          # Mark registered data to use for imputation
          use2imp = map_lgl(
            meal_data,
            function (x) {
              if ("reg_or_imputed" %in% names(x)) {
                # previously processed: registered
                r <- all(x$reg_or_imputed == "registered") & any(!is.na(x$Item))
                return (r)
              } else {
                # previously unprocessed: registered
                r <- any(!is.na(x$Item))
                return (r)
              }
            }
          )
        ) %>%
        filter(use2imp) %>%
        filter(date == sample(.$date, size = 1)) %>%
        select(meal_data) %>%
        unnest(cols = c(meal_data)) %>%
        mutate(reg_or_imputed = "imputed")
      
      return(meal_data_2)
    }
  }
  if (items_or_sums == "items") {
    
    col_order <- names(ds)
    
    # Can't understand why this does not work!
    # ds <-
    #   ds %>%
    #   group_by(date, meal) %>%
    #   nest(meal_data = -c(year, mth_txt, week, date, wday, meal)) %>%
    #   mutate(
    #     meal_data2 = pmap(., impute_meal)  # <--- this works with map2...
    #   )
    
    # Ugly workaround:
    ds_temp <-
      ds %>%
      group_by(date, meal) %>%
      nest(meal_data = -c(year, mth, mth_txt, week, date, wday, meal))
    ds_temp$meal_data2 <- pmap(ds_temp, impute_meal, all_meals = ds_temp)
    
    d_items_imp <-
      ds_temp %>%
      ungroup() %>%
      select(-meal_data) %>%
      unnest(cols = meal_data2) %>%
      select(all_of(c(col_order, "reg_or_imputed"))) %>%
      arrange(date, rid) %>%
      mutate(rid_imp = row_number()) %>%
      relocate(rid_imp)
    
    return(d_items_imp)
    
  } else {
    stop(
      glue::glue(
        "Error. Function not defined for `items_or_sums` = {items_or_sums}"
      )
    )
  }
  
}

parse_date_sums <- function(ln_start_date, ln_stop_date, diary, header) {
  parse_meal_tot <- function(meal) {
    meals_header <- header %>% str_replace("Date", "meal")
    # decimal (`,`) numbers are within escaped double quotes
    # (e.g. \"0,09\"). Format to number (e.g. `0.09`).
    diary_date %>%
      filter(
        line >= ln_idx_meals %>% filter(meal == !!meal) %>% pull(ln_start),
        line <= ln_idx_meals %>% filter(meal == !!meal) %>% pull(ln_start)
      ) %>%
      pull(txt_csv) %>%
      {
        if (meal == "date_tot")
          str_replace(., '[[:alnum:]\\,\\s]+', "date_tot")
        else
          .
      } %>%
      str_replace_all('\\"(\\d+),(\\d+)\\"', '\\1.\\2') %>%
      str_remove('^\\s+') %>%
      str_split(",") %>%
      map(~na_if(.x, "")) %>%
      map(setNames, meals_header) %>%
      unlist()
  }
  diary_date <-
    diary %>%
    filter(line >= ln_start_date, line <= ln_stop_date) %>%
    filter(txt_csv != "")
  date_df <- format_date(diary_date)
  ln_idx_meals <- get_meal_lnidx(diary_date)
  
  # parse
  date_tot      <- parse_meal_tot("date_tot")
  breakfast_tot <- parse_meal_tot("breakfast")
  lunch_tot     <- parse_meal_tot("lunch")
  dinner_tot    <- parse_meal_tot("dinner")
  snacks_tot    <- parse_meal_tot("snacks")
  
  sums <-
    bind_rows(
      list(
        all_meals = date_tot,
        breakfast = breakfast_tot,
        lunch     = lunch_tot,
        dinner    = dinner_tot,
        snacks    = snacks_tot
      ),
      .id = "meal"
    ) %>%
    mutate_at(.vars = vars(`Cals (kcal)`:`Sod (mg)`), as.numeric) %>%
    bind_cols(date_df, .)
  
  # if parsing was successful there should be 14 columns in `sums`
  stopifnot(ncol(sums) == 14)
  
  return (sums)
  
}
parse_sums <- function(diary_file, dir_out) {
  diary <- read_diary(diary_file)
  header <- get_diary_header(diary_file)
  date_startstop <- get_dates_lnidx(diary)
  
  diary_sums <-
    date_startstop %>%
    mutate(
      diary_parsed = map2(
        ln_start,
        ln_stop,
        parse_date_sums,
        diary,
        header
      )
    ) %>%
    unnest(diary_parsed) %>%
    select(-ln_start, -ln_stop) %>%
    rename(date = date_iso) %>%
    mutate(
      week = lubridate::isoweek(date),
      year = lubridate::isoyear(date)
    ) %>%
    select(year, week, date, mth_txt, wday, everything())
  
  diary_sums <- make_missingness_explicit(diary_sums, sums_or_items = "sums")

  file_out <- file.path(
    dir_out, 
    glue("{diary_sums$year[1]}_{diary_sums$mth[1]}_fooddiary_sums.rsd")
  )
  write_rds(diary_sums, file = file_out)
}

parse_date_items <- function(ln_start_date, ln_stop_date, diary, header) {
  parse_meal_items <- function(meal) {
    # Every other line is `item` (plus nutrition values) and every other line
    # is `amount`
    items_header <- header %>% str_replace("Date", "Item")
    
    meal_items <-
      diary_date %>%
      filter(
        line >  ln_idx_meals %>% filter(meal == !!meal) %>% pull(ln_start),
        line <= ln_idx_meals %>% filter(meal == !!meal) %>% pull(ln_stop)
      )
    
    if(nrow(meal_items) == 0) {
      empty <-
        tibble(!!!c(items_header, "amount")) %>%
        slice(-1) %>%
        rename_all(~str_remove_all(., '"'))
      return(empty)
    }
    
    amounts <-
      meal_items$txt_csv[seq(2, nrow(meal_items), by = 2)] %>%
      # decimal (`,`) numbers  --> Format to number (e.g. `0.09`), e.g.
      #  "2 potatisar (6.5 cm i diameter, klot)" -->
      #  "2 potatisar (6.5 cm i diameter, klot)"
      str_replace_all('\\"(.*\\d+),(\\d+.*)\\"', '\\1.\\2') %>%
      str_remove('^\\s+') %>%
      # some amount-specifications contain commas (`,`),
      # (e.g. "2 potatisar (6.5 cm i diameter, klot)"), replace by
      # semicolon (`;`)
      str_replace_all(',', ';')
    
    items <-
      meal_items$txt_csv[seq(1, nrow(meal_items), by = 2)] %>%
      # decimal (`,`) numbers are within escaped double quotes
      # (e.g. \"0,09\"). Format to number (e.g. `0.09`).
      str_replace_all('\\"(\\d+),(\\d+)\\"', '\\1.\\2') %>%
      # some item-specifications contain multiple commas (`,`),
      # (e.g. "\"   Potatis (Skalad, med Salt, Kokt)\"), replace by
      # semicolon (`;`)
      #
      # this handles up to 4 comma signs, then remove surrounding
      # double-quotes:
      str_replace_all('\\"(.+),(.+)\\"', '\\"\\1;\\2\\"') %>%
      str_replace_all('\\"(.+),(.+)\\"', '\\"\\1;\\2\\"') %>%
      str_replace_all('\\"(.+),(.+)\\"', '\\"\\1;\\2\\"') %>%
      str_replace_all('\\"(.+),(.+)\\"', '\\"\\1;\\2\\"') %>%
      str_replace_all('\\"(.+)\\"', '\\1') %>%
      str_remove('^\\s+') %>%
      str_split(",") %>%
      map(~na_if(.x, "")) %>%
      map(setNames, items_header) %>%
      bind_rows() %>%
      bind_cols(amount = amounts)
  }
  format_units <- function(ds) {
    ds %>%
      rename(amount_txt = amount) %>%
      mutate(
        amount_txt_formated =
          amount_txt %>%
          str_replace("1/2", "0.5") %>%
          str_replace("1/3", "0.33") %>%
          str_replace("1/4", "0.25") %>%
          str_replace("3/4", "0.75") %>%
          str_replace("(\\d+)cm", "\\1 cm") %>%
          str_replace(";", ".") %>%
          str_replace("medelstort .+", "st") %>%
          str_replace("potatisar .+", "st") %>%
          # many more units could be formatted here...
          str_remove_all('\\"') %>%
          str_trim(),
        amount =
          str_extract(amount_txt_formated, "^[:graph:]+") %>%
          as.numeric(),
        unit = str_extract(amount_txt_formated, "(?<=^[:graph:]{1,9}\\s).+$")
      ) %>%
      select(-amount_txt_formated)
  }
  
  diary_date <-
    diary %>%
    filter(line >= ln_start_date, line <= ln_stop_date) %>%
    filter(txt_csv != "")
  date_df <- format_date(diary_date)
  ln_idx_meals <- get_meal_lnidx(diary_date)
  
  # parse
  breakfast_items <- parse_meal_items("breakfast")
  lunch_items     <- parse_meal_items("lunch")
  dinner_items    <- parse_meal_items("dinner")
  snacks_items    <- parse_meal_items("snacks")
  
  items <-
    bind_rows(
      list(
        breakfast = breakfast_items,
        lunch = lunch_items,
        dinner = dinner_items,
        snacks = snacks_items
      ),
      .id = "meal"
    ) %>%
    mutate_at(.vars = vars(`Cals (kcal)`:`Sod (mg)`), as.numeric) %>%
    format_units(.) %>%
    bind_cols(date_df, .)
  
  # if parsing was successful there should be 18 columns in `items`
  stopifnot(ncol(items) == 18)
  
  return(items)
  
}
parse_items <- function(diary_file, dir_out) {
  diary <- read_diary(diary_file)
  header <- get_diary_header(diary_file)
  date_startstop <- get_dates_lnidx(diary)
  
  diary_items <-
    date_startstop %>%
    mutate(
      diary_parsed = map2(
        ln_start,
        ln_stop,
        parse_date_items,
        diary,
        header
      )
    ) %>%
    unnest(diary_parsed) %>%
    select(-ln_start, -ln_stop) %>%
    rename(date = date_iso) %>%
    mutate(
      week = lubridate::week(date),
      year = lubridate::year(date)
    ) %>%
    select(year, week, date, mth_txt, wday, everything())
  
  diary_items %>%
    make_missingness_explicit() %>%
    format_units() %>%
    categorize_items()
  
  mth <- month(diary_items$date[1]) %>% str_pad(width = 2, pad = "0")
  file_out <- file.path(
    dir_out, 
    glue("{diary_items$year[1]}_{mth}_fooddiary_items.rsd")
  )
  write_rds(diary_items, file = file_out)
  
  # Impute missing values
  diary_items_imputed <- impute(diary_items)
  file_out_imputed <- file.path(
    str_c(dir_out, "imputed", sep = "_"),
    glue("{diary_items$year[1]}_{mth}_fooddiary_items_imputed.rsd")
  )
  write_rds(diary_items_imputed, file = file_out_imputed)
}

