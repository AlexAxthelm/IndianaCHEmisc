validate_termkey <- function(termkey){
  key_year <- suppressWarnings(as.integer(substr(termkey, 1, 4)))
  key_term <- suppressWarnings(as.integer(substr(termkey, 5, 5)))

  is_positive <- !(termkey %in% c(-2, -1))
  good_length <- nchar(termkey) == 5
  is_int_year <- !is.na(as.integer(key_year))
  is_int_term <- !is.na(as.integer(key_term))
  is_good_summer_new <- !(key_term %in% c(1,4) & key_year >= 2017)
  is_good_summer_old <- !(key_term == 5 & key_year <= 2015)
  is_good_summer_2016 <- termkey != 20164
  key_term_valid <- key_term %in% 1:5

  is_termkey <- is_positive &
    good_length &
    is_int_year &
    is_int_term &
    is_good_summer_new &
    is_good_summer_old &
    is_good_summer_2016 &
    key_term_valid

  ret_termkey <- ifelse(is_termkey, termkey, NA_integer_)
  return(ret_termkey)
}

split_termkey <- function(termkey){
  termkey <- validate_termkey(termkey = termkey)
  key_year <- as.integer(substr(termkey, 1, 4))
  termcode <- as.integer(substr(termkey, 5, 5))

  academic_year <- ifelse(termcode == 1, key_year - 1, key_year)
  academic_year <- ifelse(is.na(termkey), NA_integer_, academic_year)

  term_name <- dplyr::case_when(
    termcode == 1 ~ "Summer A",
    termcode == 2 ~ "Fall",
    termcode == 3 ~ "Spring",
    termcode == 4 ~ "Summer B",
    termcode == 5 ~ "Trailing Summer",
    is.na(termcode) ~ NA_character_
  )

  term_season <- dplyr::case_when(
    termcode == 1 ~ "Summer",
    termcode == 2 ~ "Fall",
    termcode == 3 ~ "Spring",
    termcode == 4 ~ "Summer",
    termcode == 5 ~ "Summer",
    is.na(termcode) ~ NA_character_
  )

  fiscal_year <- ifelse(termcode == 1 & key_year == 2016, NA_integer_, key_year)
  # In 2016 CHE stopped using the Summer AB distinction across fiscal years. The 20161 term is the last of the AB distinctions
  fiscal_year <- ifelse(is.na(termkey), NA_integer_, fiscal_year)

  return_list <- list(
    academic_year = academic_year,
    term_season = term_season,
    termcode = termcode,
    term_name = term_name,
    fiscal_year = fiscal_year
  )

  return(return_list)
}

count_seasons <- function(termkey1, termkey2, neg = TRUE){
  # neg = TRUE indicates that the function  will return a negative number if termkey1 is later than termkey2 (negative time)
  # neg = FALSE will simply give the absolute difference
  #order the termkeys
  neg_factor <- 1
  termlist1 <- split_termkey(termkey1)
  termlist2 <- split_termkey(termkey2)

  if (termkey1 > termkey2) {
    if (neg) {neg_factor <- -1}
    foo <- termlist1
    termlist1 <- termlist2
    termlist2 <- foo
  }

  diff_years <- termlist2$academic_year - termlist1$academic_year
  mod_seasons <- dplyr::case_when(
    termlist2$term_season == termlist1$term_season ~ 0,
    termlist2$term_season == "Spring" & termlist1$term_season == "Fall" ~ +1,
    termlist2$term_season == "Summer" & termlist1$term_season == "Fall" ~ +2,
    termlist2$term_season == "Summer" & termlist1$term_season == "Spring" ~ +1,
    termlist2$term_season == "Fall" & termlist1$term_season == "Spring" ~ -1,
    termlist2$term_season == "Fall" & termlist1$term_season == "Summer" ~ -2, # Here we are rolling over to the next academic year
    termlist2$term_season == "Spring" & termlist1$term_season == "Summer" ~ -1#,
    # TRUE ~ NA_integer_ # This should never happen. I think I captured everything else
    )

  num_seasons = (diff_years*3) + mod_seasons
  return(num_seasons * neg_factor)
}
