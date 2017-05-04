#` Create GPG encrypted RDS of R objects.
#' This function does not accept vectorized inputs. It will only deal with whole objects
gpg_rds_encrypt <- function(x, recipient, path = "data", overwrite = TRUE){
  x_name <- deparse(substitute(x))
  file_temp <- tempfile(fileext = ".RDS")
  file_encrypt <- file.path(path, paste0(x_name, ".RDS.gpg"))

  # Just in case the path doesn't exist
  dir.create(path = path, recursive = TRUE, showWarnings = FALSE)
  
  if (file.exists(file_encrypt) & overwrite) {file.remove(file_encrypt)} #Delete Previous Version of the file

  if (requireNamespace("readr", quietly = TRUE)) {
    # Maybe we can save some time here
    readr::write_rds(x = x, path = file_temp)
  } else {
    # But also leave a failsafe option
    saveRDS(object = x, file = file_temp)
  }

  # Now we will call gpg to encrypt the file
  system(paste("gpg --recipient", recipient, "--output", file_encrypt, "--encrypt", file_temp))
  file.remove(file_temp)
}

gpg_rds_decrypt <- function(encrypted_file){
  file_temp <- tempfile(fileext = ".RDS")
  system(paste("gpg --output", file_temp, "--decrypt", encrypted_file))

  if (requireNamespace("readr", quietly = TRUE)) {
    # Maybe we can save some time here
    clear <- readr::read_rds(path = file_temp)
  } else {
    # But also leave a failsafe option
    clear <- readRDS(file = file_temp)
  }

  file.remove(file_temp)
  return(clear)
}
