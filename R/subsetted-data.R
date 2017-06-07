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
#' @param frame_df foobar
#' @param recipient foobar
#' @param ... ...
#' @param make_tiny_args foobar
#' @param gpg_rds_encrypt_args foobar
#'
#' @return foobar
#' @export
#'
#' @examples TRUE
save_gpg_frame_and_tiny <- function(
  frame_df,
  recipient,
  ...,
  make_tiny_args = list(...),
  gpg_rds_encrypt_args = list(...)
){
  make_tiny_args <- make_tiny_args
  gpg_rds_encrypt_args <- gpg_rds_encrypt_args

  frame_name <- deparse(substitute(frame_df))
  tiny_frame_name <- paste0(frame_name, "_tiny")

  frame_df
  tiny_frame <- do.call(
    what = make_tiny_frame,
    args = c(list(frame_df = frame_df), as.list(make_tiny_args))
    )

  big_path <- do.call(
    what = gpg_rds_encrypt,
    args = c(
      list(x = frame_df, recipient = recipient),
      as.list(gpg_rds_encrypt_args),
      obj_name = frame_name
      )
    )
  tiny_path <- do.call(
    what = gpg_rds_encrypt,
    args = c(
      list(x = tiny_frame, recipient = recipient),
      as.list(gpg_rds_encrypt_args),
      obj_name = tiny_frame_name
      )
    )
  return(list(frame = big_path, tiny = tiny_path))
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
read_gpg_frame_and_tiny <- function(
  encrypted_file,
  use_tiny = TRUE,
  failsafe = TRUE
){
  if (grepl(x = encrypted_file, pattern = "_tiny.RDS.gpg")){
    tiny_filename <- encrypted_file
    big_filename <- gsub(
      x = encrypted_file,
      pattern = "_tiny.RDS.gpg",
      replacement = ".RDS.gpg"
      )
  } else {
    big_filename <- encrypted_file
    tiny_filename <- gsub(
      x = big_filename,
      pattern = ".RDS.gpg",
      replacement = "_tiny.RDS.gpg"
      )
  }

  filename_valid <- identical(big_filename, encrypted_file) |
    identical(tiny_filename, encrypted_file)

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

  gpg_rds_decrypt(file_to_read)

}
