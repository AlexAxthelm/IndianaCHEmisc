#` Create GPG encrypted RDS of R objects.
gpg_rds_encrypt <- function(x, recipient, path = "data"){
  x_name <- deparse(substitute(x))
  file_temp <- tempfile(fileext = ".RDS")
  file_encrypt <- file.path(path, paste0(x_name, ".RDS.gpg"))
  saveRDS(object = x, file = file_temp)
  # Now we will call gpg to encrypt the file
  system(paste("gpg --recipient", recipient, "--output", file_encrypt, "--encrypt", file_temp))
  file.remove(file_temp)
}

gpg_rds_decrypt <- function(encrypted_file){
  file_temp <- tempfile(fileext = ".RDS")
  system(paste("gpg --output", file_temp, "--decrypt", encrypted_file))
  tmp <- readRDS(file_temp)
  file.remove(file_temp)
  return(tmp)
}
