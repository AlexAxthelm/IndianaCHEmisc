#' Messages for timing processes
#'
#' Creates a set of messages for displaying the time it takes to run a long process
#' Rounds to the nearest hundreth of a second, so not suitible for very short processes
#'
#' @param last_time **optional** object of class \code{proc_time}, indicating the most recent check. Usually the results of this function
#' @param start_time **optional** object of class \code{proc_time}, indicating the beginning of the run.
#'
#' @return results of \code{\link{proc.time}}
#' @export
#'
#' @examples
check_elapsed_time <- function(last_time, start_time){
  time_now <- proc.time()

  #Check if last_time exists
  if (hasArg(last_time)){
    if (!exists(deparse(substitute(last_time)))){
      last_time <- NULL
    }
    if (!is.null(last_time)){
    elapsed_time <- round(time_now["elapsed"] - last_time["elapsed"], 2)
    message(elapsed_time, " Seconds since last check")
    }
  }

  #Check if last_time exists
  if (hasArg(start_time)){
    if (!exists(deparse(substitute(start_time)))){
      start_time <- NULL
    }
    if (!is.null(start_time)){
    total_elapsed_time <- round(time_now["elapsed"] - start_time["elapsed"], 2)
    message(total_elapsed_time, " Seconds total")
    }
  }
  return(proc.time())
}
