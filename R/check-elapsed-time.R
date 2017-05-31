#' Printing for timing processes
#'
#' Creates a set of printed results for displaying the time it takes to run a long process
#' Rounds to the nearest hundreth of a second, so not suitible for very short processes
#' Note that this \code{print}s. Do not use unless you are expecting that.
#'
#' @param last_time **optional** object of class \code{POSIXct}, indicating the most recent check. Usually the results of this function
#' @param start_time **optional** object of class \code{POSIXct}, indicating the beginning of the run.
#'
#' @return results of \code{\link{Sys.time()}}
#' @export
#'
#' @examples
#' ptm <- proc.time()
#' Sys.sleep(4)
#' checkpoint <- check_elapsed_time(start_time = ptm)
#' Sys.sleep(8)
#' checkpoint <- check_elapsed_time(last_time = checkpoint, start_time = ptm)
check_elapsed_time <- function(last_time, start_time){
  time_now <- Sys.time()
  show_start <- FALSE
  show_last <- FALSE

  #Check if last_time exists
  if (hasArg(last_time)){
    last_time
    if (!is.null(last_time)){
      if (!is.na(last_time)){
        elapsed_time <- format(round(time_now - last_time, 2))
        last_msg <- paste(elapsed_time, "since last check")
        show_last <- TRUE
      }
    }
  }

  #Check if start_time exists
  if (hasArg(start_time)){
    start_time
    if (!is.null(start_time)){
      if (!is.na(start_time)){
        elapsed_time <- format(round(time_now - start_time, 2))
        start_msg <- paste(elapsed_time, "total")
        show_start <- TRUE
      }
    }
  }

  if (show_start & show_last){
    message(paste(last_msg, start_msg, sep = ", "))
  }
  if (show_last & !show_start){
    message(last_msg)
  }
  if (!show_last & show_start){
    message(start_msg)
  }

  return(Sys.time())
}
