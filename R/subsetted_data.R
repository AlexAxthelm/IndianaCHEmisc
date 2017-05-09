make_tiny_frame <- function(frame_df, ..., keycolumn = "CurrentCSN", randomvec = NULL, sample_size = .01){
  if (sample_size < 1) {sample_size <- sample_size*length(unique(frame_df[ , keycolumn]))}
  if (is.null(randomvec)){
    randomvec <- sample(x = unique(frame_df[ , keycolumn]), size = sample_size)
  }
  # I can probably improve this with dplyr.
  #TODO: implement a dplyr solution, but make it optional.
  rows_to_return <- which(frame_df[ , keycolumn] %in% randomvec)
  return(frame_df[rows_to_return, ])
}

save_frame_and_tiny <- function(
  frame_df,
  keycolumn = "CurrentCSN",
  recipient,
  ...,
  make_tiny_args = list(NULL),
  gpg_rds_encrypt_args = list(NULL)
){
  frame_name <- deparse(substitute(frame_df))
  tiny_frame_name <- paste0(frame_name, "_tiny")
  tiny_frame <- make_tiny_frame(frame_df = frame_df, keycolumn = keycolumn, ..., as.list(make_tiny_args))

  gpg_rds_encrypt(x = frame_df, recipient = recipient, ..., as.list(gpg_rds_encrypt_args),  obj_name = frame_name)
  gpg_rds_encrypt(x = tiny_frame, recipient = recipient, ..., as.list(gpg_rds_encrypt_args), obj_name = tiny_frame_name)
  return(TRUE)
}
