web_browse <- function(x) { 
  file = stringr::str_c(tempfile(deparse(substitute(x)),
                        fileext = ".csv"))
  readr::write_csv(x, path = file)
  browseURL(file) 
} 
