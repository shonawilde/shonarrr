#' Read Genesis
#'
#' Reads and cleans process air survey emissions calculation data sheets 
#'
#' @param file_list Files to read
#' 
#' @param verbose Should a message to the user be displayed?
#' 
#' @author Shona Wilde
#' 
#' @return Tibble
#' 
#' @export

read_genesis <- function(file_list, verbose = TRUE) {
  
  df <- purrr::map_dfr(
    file_list,
    read_genesis_worker,
    verbose = verbose
    )
  
  return(df)
  
}


read_genesis_worker <- function(file, verbose) {
  
  # display file name
  if (verbose) message(threadr::date_message(), "`", file, "`...")
  
  # META data
  suppressMessages(
    df_meta <- readxl::read_xlsx(file,
                                 sheet = 2)
  )
  
  # get line containing name of rig and dates of calculations
  meta <- df_meta %>% 
    filter_all(any_vars(stringr::str_detect(., pattern = "Emission loads for the"))) %>% 
    janitor::remove_empty("cols") %>% 
    pull() %>% 
    `[`(1) 
  
  # pull out platform name
  platform <- meta %>% 
    stringr::str_match("the\\s*(.*?)\\s*installation") %>% 
    `[`(2)
  
  # get dates of campaign period
  date_range <- meta %>% 
    stringr::str_match("of\\s*(.*?)\\s*(tonnes)") %>% 
    `[`(2) %>% 
    stringr::str_remove(" \\(")
  
  # assign start and end dates
  stringr::str_split(date_range, "-") %>% 
    unlist() %>% 
    purrr::set_names(c("date_start", "date_end")) %>% 
    tibble::enframe() %>% 
    mutate(value = lubridate::dmy(value)) %>% 
    mutate(assign = purrr::walk2(name, value, ~assign(x = .x, 
                                                      value = .y,
                                                      env = .GlobalEnv)))
  
  # get length of period
  # add one to include end date
  date_length <- difftime(date_end, date_start, units = "days") %>% 
    as.double() + 1
  
  
  # EMISSION DATA 
  
  # read file and remove cases where B2 appears for version 2
  df_loads <- readxl::read_xlsx(file,
                                sheet = "4 Emission loads ",
                                .name_repair = ~paste0("col_", 1:length(.x)),
                                range = readxl::anchored("B10", c(NA, NA))) %>% 
    mutate_all(. %>% na_if("B2")) %>% 
    janitor::remove_empty("rows") 
  
  
  # find start of second set of tables
  df_loads %>% 
    filter_all(any_vars(stringr::str_detect(., pattern = "Emission"))) %>% 
    janitor::remove_empty("cols") %>% 
    tibble::rowid_to_column() %>% 
    tidyr::pivot_longer(cols = -rowid) %>% 
    filter(stringr::str_detect(value, "Emission")) %>% 
    distinct(name) %>% 
    pull() %>% 
    #slice(1) %>% 
    # remove_empty("cols") %>% 
    #names() %>% 
    stringr::str_split("_") %>% 
    purrr::map(`[`(2)) %>% 
    purrr::flatten_chr() %>% 
    as.numeric() %>% 
    purrr::set_names(c("index_start", "index_end")) %>% 
    tibble::enframe() %>% 
    mutate(assign = purrr::walk2(name, value, ~assign(x = .x, 
                                                      value = .y,
                                                      env = .GlobalEnv)))
  
  # get both halves of data
  df1 <- df_loads %>% 
    select(index_start:index_end-1) %>% 
    janitor::remove_empty("cols")
  
  df2 <- df_loads %>% 
    select(all_of(index_end):last_col()) %>% 
    janitor::remove_empty("cols") %>% 
    purrr::set_names(paste0("col_",1:length(.)))
  
  # bind together
  df_all <- df1 %>% 
    bind_rows(df2) %>% 
    filter(!str_detect(col_1, pattern = "note")) %>% 
    janitor::remove_empty(c("rows", "cols")) %>% 
    mutate(across(.cols = everything(), tolower)) %>% 
    tibble::rowid_to_column()
  
  # names of tables
  table_names <- df_all %>% 
    filter(stringr::str_detect(col_1, "emission"),
           stringr::str_detect(col_1, "emissions$", negate = TRUE)) %>% 
    pull(col_1)
  
  # number of data tables
  n_tables <- length(table_names) %>% 
    as.numeric()
  
  # create vector of table names
  table_vector <- purrr::map_chr(1:n_tables, ~paste0("table", .))
  
  # match table number to correct name
  table_identifer <- table_vector %>% 
    purrr::set_names(table_names) %>% 
    tibble::enframe(name = "emission_type_long",
                    value = "table")
  
  # find row numbers to split data on
  rows_to_split <- table_names %>% 
    tibble::enframe(value = "table_name") %>% 
    left_join(df_all, 
              by = c("table_name" = "col_1")) %>% 
    arrange(rowid) %>% 
    pull(rowid) %>% 
    as.numeric() %>% 
    unique() 
  
  # calculate length of each table
  table_lengths <- rows_to_split %>% 
    diff() %>% 
    c(nrow(df_all)-sum(.))
  
  
  # create vector of correct length to identify tables within df
  df_tables <- purrr::map2(table_vector, table_lengths, ~rep(x = .x, times = .y)) %>% 
    purrr::flatten_chr() %>% 
    tibble::enframe(name = "rowid",
                    value = "table")
  
  # join to data and remove first row containing table name
  df_nest <- df_tables %>% 
    left_join(df_all,
              by = "rowid") %>% 
    slice(-rows_to_split) %>% 
    select(-rowid) %>% 
    group_nest(table) %>% 
    left_join(table_identifer, 
              by = "table") %>% 
    relocate(emission_type_long, 1) %>% 
    mutate(data = map(data, as_tibble))
  
  # clean each table of data
  suppressWarnings(
    df_nest_clean <- df_nest %>%
      mutate(data = map(data, janitor::remove_empty, "cols"),
             data = map(data, janitor::row_to_names, 1),
             data = map(data, janitor::clean_names),
             data = map(data, rename_with, .cols = everything(), stringr::str_remove_all, "_1"),
             data = map(data, rename_with, .cols = everything(), stringr::str_remove_all, "_"),
             data = map(data, rename_with, .cols = everything(), stringr::str_replace_all, "na", "total"))
    
  )
  
  # put back into single data frame, parse dates and more cleaning
  suppressWarnings(
    df_unnest <- df_nest_clean %>% 
      tidyr::unnest(data) %>% 
      filter(!date == "total") %>% 
      mutate(date = threadr::str_rm_round_brackets(date)) %>% 
      mutate(date = if_else(is.na(threadr::parse_excel_date(date)),
                            lubridate::dmy(date) %>% as.POSIXct(),
                            threadr::parse_excel_date(date)),
             date = lubridate::date(date)) %>%
      #  select(-total, -reference) %>% 
      mutate(platform = platform,
             .before = emission_type_long) %>% 
      mutate(across(co2:voc, as.numeric),
             across(co2:voc, round, digits = 3)) %>% 
      filter_at(vars(co2:voc), any_vars(!is.na(.))) %>% 
      janitor::remove_empty(c("rows", "cols"))
  )
  
  
  # clean up emission types
  df_final <- df_unnest %>% 
    mutate(emission_type = case_when(
      stringr::str_detect(emission_type_long, "flaring") ~ "flaring",
      stringr::str_detect(emission_type_long, "venting") ~ "venting",
      stringr::str_detect(emission_type_long, "helicopter") ~ "helicopter",
      stringr::str_detect(emission_type_long, "vessel") ~ "vessel",
      stringr::str_detect(emission_type_long, "fugitive") ~ "fugitive",
      stringr::str_detect(emission_type_long, "combustion") ~ "combustion",
      stringr::str_detect(emission_type_long, "total") ~ "total",
      TRUE ~ emission_type_long),
      .before = emission_type_long)
  
  return(df_final)
  
  
}


