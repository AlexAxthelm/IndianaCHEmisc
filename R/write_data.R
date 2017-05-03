rewrite_obj_rds <- function(object, dir = "data"){
  dir.create(path = dir, recursive = TRUE)
  file <- file.path(dir, paste0(substitute(object), ".RDS"))
  if (file.exists(file)) {file.remove(file)} #Delete Previous Version of the file
  saveRDS(object = object, file = file)
}
