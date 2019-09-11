library(tidyverse)
library(scales)

# replace values for column with NAs based on values within a vector

make_na <- function(x, vals) {
  x[x %in% vals] <- NA
  x
}

#' function to get weighted borda count for any number of vectors
#'
#' @param df data frame
#' @param columns vectors to be ranked
#' @param weights weight vector. Should be the same length as vectors provided for ranking
#' @param exclude vector of values to exclude from ranking analysis
#' @param ranks number of ranks to be calculated
borda_count <- function(df, columns, weighting_function = NULL, exclude = NULL, ranks = 3, return_names = T) {
  df <- mutate_at(df, columns, make_na, "") %>%
    filter_at(columns, any_vars(!is.na(.)))
  
  if (is.null(weighting_function)) {
    weights <- rep(1, nrow(df))
  } else {
    weights <- weighting_function(df)
  }
  
  df <- select(df, columns)
  
  vals <- map(df, ~ unique(.x[!is.na(.x)]))
  vals <- unique(unlist(vals)) # getting unique values to rank
  ranks <- min(ranks, length(vals)) # ensuring that end result doesn't have NA values for calculating for non-existent rankings
  table <- map_dfc(vals, ~ tibble(!!(.x) := map_dbl(df, function(x) sum((x == .x) * weights, na.rm = T)))) %>% # counting the weighted occurences of each value
    mutate_all(~ .x * (length(vals) - 1:n() + 1))  %>% # multiplying row values by their borda rank
    summarize_all(sum)
  
  table <- table[,order(-table[1,])] # reordering based on score
  
  table <- table %>%
    mutate_all(~ scales::percent(.x / rowSums(table)))# getting the total vote score for each item

  names <- paste(names(table)[1:ranks], collapse = " ") # getting the total borda count for each
  percents <- paste(table[1, 1:ranks], collapse = " ")
  if (return_names) {
    names
  } else {
    percents
  }
}

#' get borda counts for questions defined by script CSV
#' 
#' @param 
borda_applier <- function(script, df, weighting_function = NULL) {
  script <- script %>%
    rowwise() %>%
    mutate(vars = list(c(variable_1, variable_2, variable_3, variable_4, variable_5))) %>%
    ungroup() %>%
    mutate(exclude = stringr::str_split(exclude, " "),
           vars = map(vars, ~.x[!is.na(.x)])) %>%
    select(vars, exclude, disaggregate, repeat_var, ranks) 
  
  pmap_df(list(
    script$vars,
    script$exclude,
    script$disaggregate,
    script$repeat_var,
    script$ranks),
    borda_analyzer,
    df,
    weighting_function) %>%
    separate(result, into = paste0("rank_", 1:max(script$ranks)), sep = " ") %>%
    separate(percent, into = paste0("percent_", 1:max(script$ranks)), sep = " ")
}

borda_analyzer <- function(vars, exclude, disaggregate, repeat_var, ranks, df, weighting_function = NULL) {
  if (!is.na(repeat_var)) {
    if (!is.na(disaggregate)) {
      repeats <- df %>%
        rename(repeat_var = !!sym(repeat_var), disaggregation = !!sym(disaggregate)) %>%
        group_by(repeat_var, disaggregation) %>%
        nest
    } else {
      repeats <- df %>%
        rename(repeat_var = !!sym(repeat_var)) %>%
        group_by(repeat_var) %>%
        nest %>%
        add_column(disaggregation = NA)
    }
    
  } else if (!is.na(disaggregate)) {
    repeats <- df %>%
      rename(disaggregation = !!sym(disaggregate)) %>%
      group_by(disaggregation) %>%
      nest %>%
      add_column(repeat_var = NA)
  } else {
    repeats <- tibble(repeat_var = NA, disaggregation = NA, data = nest(df)) %>%
      unnest
  }
  
  mutate(repeats,
         result = map_chr(data, borda_count, vars, weighting_function, exclude, ranks),
         percent = map_chr(data, borda_count, vars, weighting_function, exclude, ranks, return_names = F),
         vars = paste(vars, collapse = " ")) %>%
    select(vars, repeat_var, disaggregation, result, percent)
}
