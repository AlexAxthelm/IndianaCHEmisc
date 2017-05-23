#'  Determine if a termkey is valid
#'
#'  For use with Indiana CHE \code{TermKey}s.
#'  Takes into account the change in reporting method in summer 2016.
#'
#' @param termkey TermKey for record pulled from SQL database
#'
#' @return either the valid termkey, or \code{NA_integer} is not a valid termkey
#' @export
#'
#' @examples
#' validate_termkey(20081) # Valid, summer 2, 2007
#' validate_termkey(20082) # Valid, Fall 2007
#' validate_termkey(20083) # Valid, Spring 2008
#' validate_termkey(20084) # Valid, Summer 1, 2008
#' validate_termkey(20085) # Not Valid
#'
#' validate_termkey(20181) # Not Valid
#' validate_termkey(20182) # Valid, Fall 2017
#' validate_termkey(20183) # Valid, Spring 2018
#' validate_termkey(20184) # Not Valid
#' validate_termkey(20185) # Valid, Trailing Summer 2018
validate_termkey <- function(termkey){

  # get rid of any underscores put into the term key by pasting or `unite`ing.
  termkey <- gsub(pattern = "_", replacement = "", x = termkey)

  key_year <- suppressWarnings(as.integer(substr(termkey, 1, 4)))
  key_term <- suppressWarnings(as.integer(substr(termkey, 5, 5)))

  is_positive <- !(termkey %in% c(-2, -1))
  good_length <- nchar(termkey) == 5
  is_int_year <- !is.na(as.integer(key_year))
  is_int_term <- !is.na(as.integer(key_term))
  is_good_summer_new <- !(key_term %in% c(1, 4) & key_year >= 2017)
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

#' Split Termkey into important information
#'
#' @param termkey
#'
#' @return List:
#' academic_year,
#' term_season,
#' term_season_number,
#' termcode,
#' term_name,
#' fiscal_year
#' @export
#'
#' @examples
#' split_termkey(20083)
#' split_termkey("20291")
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

  # I know it seems like I'm reinventing the wheel here, to get back to the
  # same term codes as we started with, but:
  # 1: I want a sanity check
  # 2: I want to be sure that the (old) summer terms are smooshing correctly
  term_season_number <- dplyr::case_when(
    term_season == "Fall" ~ 2L,
    term_season == "Spring" ~ 3L,
    term_season == "Summer" ~ 5L,
    is.na(term_season) ~ NA_integer_
  )

  fiscal_year <- ifelse(termcode == 1 & key_year == 2016, NA_integer_, key_year)
  # In 2016 CHE stopped using the Summer AB distinction
  # across fiscal years. The 20161 term is the last of the AB distinctions
  fiscal_year <- ifelse(is.na(termkey), NA_integer_, fiscal_year)

  return_list <- list(
    academic_year = academic_year,
    term_season = term_season,
    term_season_number = term_season_number,
    termcode = termcode,
    term_name = term_name,
    fiscal_year = fiscal_year
  )

  return(return_list)
}

#' Count the number of seasons between two termkeys
#'
#' Fall, spring, summer.
#' If ignore_summers = TRUE, but one of the termkeys is a summer, it will make the summer term collapse into the previous spring. 
#' This gives an accurate count of the number of fall and spring terms in between two termkeys.
#'
#' @param termkey1 A Termkey (will be validated). By convention, should be earlier
#' @param termkey2 A Termkey (will be validated). By convention, should be later
#' @param neg Boolean: return a negative number of seasons if termkey 1 is later than termkey2, if FALSE, provide an absolute difference
#' @param ignore_summers Boolean: if TRUE, count fall and spring semesters only
#'
#' @return
#' @export
#'
#' @examples
count_seasons <- function(
  termkey1,
  termkey2,
  neg = TRUE,
  ignore_summers = FALSE
  ){
  # neg = TRUE indicates that the function  will return a negative number if
  # termkey1 is later than termkey2 (negative time)
  # neg = FALSE will simply give the absolute difference
  #order the termkeys
  neg_factor <- 1
  termlist1 <- split_termkey(termkey1)
  termlist2 <- split_termkey(termkey2)

  if (termkey1 > termkey2) {
    if (neg) {
      neg_factor <- -1
    }
    foo <- termlist1
    termlist1 <- termlist2
    termlist2 <- foo
  }

  # Upon wokring things out by hand, it appears that when ignoring summers, if
  # one of the terms is a summer term, we should collapse it into the previous
  # spring. This gives an accurate count of the number of fall and spring terms
  # in between any two terms. If counting backwards, however, we need to
  # collapse back to the following fall.
  if (ignore_summers){
    if (termlist1$term_season == "Summer"){
      if (termkey1 > termkey2) {
        termlist1$term_season <- "Fall"
        # Note that it needs to be the following fall
        termlist1$academic_year <- termlist1$academic_year + 1
      } else {
        termlist1$term_season <- "Spring"
      }
    }
    if (termlist2$term_season == "Summer"){
      if (termkey1 > termkey2) {
        termlist2$term_season <- "Fall"
        # Note that it needs to be the following fall
        termlist2$academic_year <- termlist2$academic_year + 1
      } else {
        termlist2$term_season <- "Spring"
      }
    }
  }

  mod_seasons <- dplyr::case_when(
    termlist2$term_season == termlist1$term_season ~ 0,
    termlist2$term_season == "Spring" & termlist1$term_season == "Fall" ~ +1,
    termlist2$term_season == "Summer" & termlist1$term_season == "Fall" ~ +2,
    termlist2$term_season == "Summer" & termlist1$term_season == "Spring" ~ +1,
    termlist2$term_season == "Fall" & termlist1$term_season == "Spring" ~ -1,
    # Here we are rolling over to the next academic year
    termlist2$term_season == "Fall" & termlist1$term_season == "Summer" ~ -2,
    termlist2$term_season == "Spring" & termlist1$term_season == "Summer" ~ -1
  )

  diff_years <- termlist2$academic_year - termlist1$academic_year
  if (ignore_summers){
    # If we are not ignoring summer, there are two seasons per year
    num_seasons <- (diff_years * 2) + mod_seasons
  } else {
    # If we are not ignoring summer, there are three seasons per year
    num_seasons <- (diff_years * 3) + mod_seasons
  }


  return(num_seasons * neg_factor)
}
