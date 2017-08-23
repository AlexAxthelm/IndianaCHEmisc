#' Find a sample common to two sets
#' @export
find_sample <- function(
  new_vec,
  old_sample = list(vec = NULL, sample = NULL),
  size_pct = 0.002
  ){
  if (!exists(deparse(substitute(old_sample)))){
    old_sample <- list(vec = NULL, sample = NULL)
  }
  old_vec <- old_sample[["vec"]]
  old_samp <- old_sample[["sample"]]
  overlap <- intersect(old_vec, new_vec)
  pct <- length(old_samp) / length(old_vec)

  new_unique <- setdiff(new_vec, old_vec)
  new_unique_samp <- sample(
    x = new_unique,
    # the c() in here is for error trapping incase old_sample is NULL
    size = round(ifelse(is.nan(pct), size_pct, pct) * length(new_unique))
    )

  if (length(overlap) == 0){
    overlap_new_samp <- NULL
  } else{
    old_overlap_sample <- intersect(old_samp, overlap)
    overlap_not_in_sample <- setdiff(overlap, old_samp)
    pct_overlap_to_sample <- max(pct - (
      length(old_overlap_sample) / length(overlap)
      ), 0)
    overlap_new_samp <- sample(
      x = overlap_not_in_sample,
      size = min(
        round(pct_overlap_to_sample * length(overlap_not_in_sample)),
        length(overlap_not_in_sample)
        )
    )
  }

  new_sample <- union(old_samp, union(new_unique_samp, overlap_new_samp))
  return(list(vec = sort(union(old_vec, new_vec)), sample = sort(new_sample)))
}

#' Create a name suitible for feeding into vault_save
#' @export
sample_name <- function(
  name,
  use_sample = config$use_sample,
  sample_pct = config$sample_pct
  ){
  if (use_sample){
    sample_suffix <- paste0(
      "_",
      gsub(
        x = sample_pct,
        pattern = "\\.",
        replacement = "p"
        )
      )
  } else {
    sample_suffix <- NULL
  }

  return(paste0(name, sample_suffix))
}
