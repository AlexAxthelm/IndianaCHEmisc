# Special thanks to SO user cbymar
# https://stackoverflow.com/a/30551944/7571303
lineclean <- function(x){
  # remove all tabs
  x <- gsub("\t+", "", x, perl = TRUE);
  # remove leading whitespace
  x <- gsub("^\\s+", "", x, perl = TRUE);
  # remove trailing whitespace
  x <- gsub("\\s+$", "", x, perl = TRUE);
  # collapse multiple spaces to a single space
  x <- gsub("[  ]+", " ", x, perl = TRUE);
  # destroy any comments
  x <- gsub("^[--]+.*$", "", x, perl = TRUE);
}

#' Turn an SQL file into a query the DBI can understand
#' @export
read_SQL_query_from_file <- function(
  file,
  printquery = FALSE
  ){
  # ensure the file is there
  stopifnot(file.exists(file))
  # read in the query to a list of lines
  sqllines <- readLines(file)
  # process each line
  cleaned <- lapply(sqllines, IndianaCHEmisc:::lineclean)
  # remove blank and/or comment lines
  noblanks <- Filter(function(x) x != "", cleaned)
  # paste lines together into one-line string, spaces between.
  oneline <- paste(unlist(noblanks), collapse = " ")
  if (printquery){
    cat(paste(
        unlist(noblanks),
        collapse = "\n"
        ))
  }
  return(oneline)
}
