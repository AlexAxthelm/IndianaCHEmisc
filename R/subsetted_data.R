make_tiny_frame <- function(frame_df, ..., keycolumn = "CurrentCSN", randomvec = NULL, sample_size = .01){
  # keycolumn
  # eval(frame_df)
  if (sample_size < 1) {sample_size <- sample_size*length(unique(frame_df[ , keycolumn]))}
  if (is.null(randomvec)){
    randomvec <- sample(x = unique(frame_df[ , keycolumn]), size = sample_size)
  }
  # I can probably improve this with dplyr.
  #TODO: implement a dplyr solution, but make it optional.
  rows_to_return <- which(frame_df[ , keycolumn] %in% randomvec)
  return(frame_df[rows_to_return, ])
}

save_gpg_frame_and_tiny <- function(
  frame_df,
  keycolumn = "CurrentCSN",
  recipient,
  ...,
  make_tiny_args = list(...),
  gpg_rds_encrypt_args = list(...)
){
  three_dots <- list(...)
  make_tiny_args <- make_tiny_args
  frame_name <- deparse(substitute(frame_df))
  tiny_frame_name <- paste0(frame_name, "_tiny")
  # tiny_frame <- make_tiny_frame(frame_df = frame_df, keycolumn = keycolumn, as.list(make_tiny_args))
  frame_df
  tiny_frame <- do.call(what = make_tiny_frame, args = c(list(frame_df = frame_df), keycolumn = keycolumn, as.list(make_tiny_args)))

  gpg_rds_encrypt(x = frame_df, recipient = recipient, ..., as.list(gpg_rds_encrypt_args),  obj_name = frame_name)
  gpg_rds_encrypt(x = tiny_frame, recipient = recipient, ..., as.list(gpg_rds_encrypt_args), obj_name = tiny_frame_name)
  return(TRUE)
}

read_gpg_frame_and_tiny <- function(
  encrypted_file,
  use_tiny = TRUE
){
  tiny_filename <- gsub(x = encrypted_file, pattern = ".RDS.gpg", replacement = "_tiny.RDS.gpg")
  big_filename <- gsub(x = encrypted_file, pattern = "_tiny.RDS.gpg", replacement = ".RDS.gpg")
  filename_valid <- identical(big_filename, encrypted_file) | identical(tiny_filename, encrypted_file)
  file_to_read <- if (use_tiny) {tiny_filename} else {big_filename}

  if (!file.exists(big_filename)) warning(big_filename, " does not exist.")
  if (!file.exists(tiny_filename)) warning(tiny_filename, " does not exist.")

  stopifnot(
    filename_valid,
    file.exists(file_to_read)
  )

  gpg_rds_decrypt(file_to_read)

}
