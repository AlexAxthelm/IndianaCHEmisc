#' Create a subset of data
#' TODO: Expand this documentation
#'
#' @param frame_df a dataframe or similar (tbl)
#' @param keycolumn character: name of column by which to sample. If missing, defaults to row names
#' @param randomvec a vector a values in keycolumn to return
#' @param sample_n number of samples to return, if randomvec is missing
#'
#' @return a subsetted object of same type as frame_df
#' @export
#'
#' @examples make_tiny_frame(mtcars)
#' make_tiny_frame(ChickWeight, keycolumn = "Chick")
make_tiny_frame <- function(
  frame_df,
  keycolumn,
  randomvec,
  sample_n = 5
  ){
  if (missing(keycolumn)){
    keycolumn_vec <- unlist(rownames(frame_df))
  } else {
    keycolumn_vec <- unlist(frame_df[keycolumn])
  }

  if (missing(randomvec)){
    randomvec <- sample(
        x = unique(keycolumn_vec),
        size = min(sample_n, length(unique(keycolumn_vec)))
      )
  }

  rows_for_tiny <- which(keycolumn_vec %in% randomvec)

  tiny_frame <- frame_df[rows_for_tiny, ]
  return(tiny_frame)
}

#' Save an encrypted RDS
#' TODO: Expand this Documentation
#'
#' @param frame_df data.frame, or similar
#' @param keycolumn column of frame_df to select from
#' @param randomvec vector of values that are in keycolumn
#'
#' @return foobar
#' @export
#'
#' @examples TRUE
save_frame_and_tiny <- function(
  frame_df,
  keycolumn,
  randomvec,
  path = "data",
  make_tiny_args = list()
){
  make_tiny_args <- make_tiny_args

  frame_name <- deparse(substitute(frame_df))
  tiny_frame_name <- paste0(frame_name, "_tiny")

  # frame_df
  mtf_args <- list(frame_df = frame_df)
  if (!missing(keycolumn)){
    mtf_args <- c(mtf_args, keycolumn = keycolumn)
  }
  if (!missing(randomvec)){
    mtf_args <- c(mtf_args, randomvec = randomvec)
  }
  mtf_args <- c(mtf_args, as.list(make_tiny_args))

  tiny_frame <- do.call(
    what = make_tiny_frame,
    args = mtf_args
    )

  big_path <- file.path(path, paste0(frame_name, ".RDS.gpg"))
  tiny_path <- file.path(path, paste0(frame_name, ".RDS.gpg"))

  saveRDS(frame_name, big_path)
  saveRDS(tiny_frame_name, tiny_path)

  return(invisible(list(frame = big_path, tiny = tiny_path)))
}

#' Load an encrypted RDS
#' TODO: Expand this Documentation
#'
#' @param encrypted_file foobar
#' @param use_tiny foobar
#' @param failsafe foobar
#'
#' @return foobar
#' @export
#'
#' @examples TRUE
read_frame_and_tiny <- function(
  file,
  use_tiny = TRUE,
  failsafe = TRUE
){
  if (grepl(x = file, pattern = "_tiny.RDS.gpg")){
    tiny_filename <- file
    big_filename <- gsub(
      x = file,
      pattern = "_tiny.RDS.gpg",
      replacement = ".RDS.gpg"
      )
  } else {
    big_filename <- file
    tiny_filename <- gsub(
      x = big_filename,
      pattern = ".RDS.gpg",
      replacement = "_tiny.RDS.gpg"
      )
  }

  filename_valid <- identical(big_filename, file) |
    identical(tiny_filename, file)

  file_to_read <- if (use_tiny) {
    tiny_filename
  } else {
    big_filename
  }

  if (!file.exists(big_filename)){
    warning(big_filename, " does not exist.")
    if (failsafe & !use_tiny){
      warning("Attempting to use ", tiny_filename, " instead")
      file_to_read <- tiny_filename
    }
  }

  if (!file.exists(tiny_filename)){
    warning(tiny_filename, " does not exist.")
    if (failsafe & use_tiny){
      warning("Attempting to use ", big_filename, " instead")
      file_to_read <- big_filename
    }
  }

  stopifnot(
    filename_valid,
    file.exists(file_to_read)
  )

  readRDS(file_to_read)

}
