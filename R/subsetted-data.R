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

secet_rewrite <- function(name, value, users, vault){
  if (name %in% secret::list_secrets(vault)){
    secret::update_secret(
      name = name,
      value = value,
      vault = vault
      )
  } else {
    secret::add_secret(
      name = name,
      value = value,
      useres = users,
      vault = vault
      )
  }
}

#' Put a frame and its subset into the `secret` vault
#' TODO: Expand this documentation
#'
#' @param frame_df a dataframe or similar (tbl)
#' @param keycolumn character: name of column by which to sample. If missing, defaults to row names
#' @param randomvec a vector a values in keycolumn to return
#' @param vault the vault to which to save
#'
#' @return a subsetted object of same type as frame_df
#' @export
vault_frame_and_tiny <- function(
  frame_df,
  keycolumn,
  randomvec,
  vault,
  users
){
  frame_name <- deparse(substitute(frame_df))
  frame_name_tiny <- paste0(frame_name, "_tiny")

  frame_tiny <- make_tiny_frame(
      frame_df = frame_df,
      keycolumn = keycolumn,
      randomvec = randomvec
    )

  secet_rewrite(
    name = frame_name,
    value = frame_df,
    users = users,
    vault = vault
    )
  secet_rewrite(
    name = frame_name_tiny,
    value = frame_tiny,
    users = users,
    vault = vault
    )

  return(list(frame = frame_name, tiny = frame_name_tiny))
}
