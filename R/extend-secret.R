#' Title
#'
#' @param object
#' @param vault
#' @param users
#'
#' @return
#' @export
#'
#' @examples
vault_save <- function(object, name, users = config$users, ...){
  if (missing(name)){
    name <- deparse(substitute(object))
  }
  if (length(secret::list_owners(name)) > 0){
    secret::update_secret(
      name = name,
      value = object,
      ...
      )
  } else {
    secret::add_secret(
      name = name,
      value = object,
      users = users,
      ...
      )
  }
}
