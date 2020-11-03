web_browse <- function(x, ...) { 
  UseMethod("web_browse")
} 

web_browse.data.frame <- function(x, ...){ 
  
  file = stringr::str_c(tempfile(deparse(substitute(x)),
                                 fileext = ".csv"))
  readr::write_csv(x, path = file)
  browseURL(file) 

}

web_browse.gg <- function(x, 
                          width = 20, 
                          height = 20, 
                          ...){ 
  file = stringr::str_c(tempfile(deparse(substitute(x)),
                                 fileext = ".png"))
  ggplot2::ggsave(plot = x, filename = file, width = width, height = height)
  browseURL(file)
 }


 

