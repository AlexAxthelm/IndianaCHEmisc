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
#' @return Integer number of seasons between term keys
#' @export
#'
#' @examples count_seasons(20062, 20185)
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
  if (neg) {
    neg_factor_num <- -2
  } else {
    neg_factor_num <- 0
  }

  going_backwards <- (termkey1 > termkey2)

  neg_factor <- 1 + (as.integer(going_backwards) * neg_factor_num)
  foo <- dplyr::case_when(
    going_backwards ~ termkey2,
    TRUE ~ termkey1
    )
  bar <- dplyr::case_when(
    going_backwards ~ termkey1,
    TRUE ~ termkey2
    )
  termkey1 <- foo
  termkey2 <- bar

  termlist1 <- split_termkey(termkey1)
  termlist2 <- split_termkey(termkey2)


  # Upon wokring things out by hand, it appears that when ignoring summers, if
  # one of the terms is a summer term, we should collapse it into the previous
  # spring. This gives an accurate count of the number of fall and spring terms
  # in between any two terms. If counting backwards, however, we need to
  # collapse back to the following fall.
  if (ignore_summers){
    termlist1$academic_year <- dplyr::case_when(
      termlist1$term_season == "Summer" & going_backwards ~
        termlist1$academic_year + 1L,
      TRUE ~ termlist1$academic_year
      )
    termlist1$term_season <- dplyr::case_when(
      termlist1$term_season == "Summer" & going_backwards ~ "Fall",
      termlist1$term_season == "Summer" ~ "Spring",
      TRUE ~ termlist1$term_season
      )

    termlist2$academic_year <- dplyr::case_when(
      termlist2$term_season == "Summer" & going_backwards ~
        termlist2$academic_year + 1L,
      TRUE ~ termlist2$academic_year
      )
    termlist2$term_season <- dplyr::case_when(
      termlist2$term_season == "Summer" & going_backwards ~ "Fall",
      termlist2$term_season == "Summer" ~ "Spring",
      TRUE ~ termlist2$term_season
      )
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
