#' Save an R object as an encrypted file
#'
#' @param x any R object
#' @param recipient the GPG recipient (passes args to \code{-r})
#' @param ... ...
#' @param path Path to which file should be written
#' @param overwrite If TRUE, will overwrite existing file with same name
#' @param obj_name if not NULL, will use string as filename
#'
#' @return Path to encrypted file
#' @export
#'
#' @examples
#' \dontrun{ 
#' path <- gpg_rds_encrypt(mtcars, recipient = "AAxthelm@che.in.gov")
#' mtc2 <- gpg_rds_decrypt(path)
#' stopifnot(identical(mtc2, mtcars))
#' }
gpg_rds_encrypt <- function(
  x,
  recipient,
  ...,
  path = "data",
  overwrite = TRUE,
  obj_name = NULL
  ){
  if (is.null(obj_name)){
    x_name <- deparse(substitute(x))
  } else {
    x_name <- obj_name
  }

  file_temp <- tempfile(fileext = ".RDS")
  file_encrypt <- file.path(path, paste0(x_name, ".RDS.gpg"))

  # Just in case the path doesn't exist
  dir.create(path = path, recursive = TRUE, showWarnings = FALSE)

  if (file.exists(file_encrypt) & overwrite) {
    file.remove(file_encrypt)
  } #Delete Previous Version of the file

  if (requireNamespace("readr", quietly = TRUE)) {
    # Maybe we can save some time here
    readr::write_rds(x = x, path = file_temp)
  } else {
    # But also leave a failsafe option
    saveRDS(object = x, file = file_temp)
  }

  # Now we will call gpg to encrypt the file
  system(command = paste(
      "gpg",
      paste(" --recipient", recipient, collapse = ""),
      "--output", file_encrypt, "--encrypt", file_temp
      ))
  file.remove(file_temp)
  return(file_encrypt)
}

#' Load encrypted data
#'
#' @param encrypted_file Path to an encrypted file made using this package
#'
#' @return Original object stored in file
#' @export
#'
#' @examples
#' \dontrun{ 
#' path <- gpg_rds_encrypt(mtcars, recipient = "AAxthelm@che.in.gov")
#' mtc2 <- gpg_rds_decrypt(path)
#' stopifnot(identical(mtc2, mtcars))
#' }
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
