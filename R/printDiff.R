printDiff <- function(x, ...) {
  UseMethod("printDiff")
}

printDiff.list <- function(diff_list, names = NULL) {
  if (is.null(names)) {
    names <- names(diff_list)
  }
  
  if (is.null(names(diff_list)) && is.null(names)) {
    stop(
      "The provided list must be named or the names must be specified \n using the names argument"
    )
  }
  
  if (length(diff_list) > 2) {
    warning(
      "The length of list is greater than two. Only the first two \n elements of the list will be used."
    )
  }
  
  diff_list <- list(setdiff(diff_list[[1]], diff_list[[2]]),
                    setdiff(diff_list[[2]], diff_list[[1]]))
  
  empty_char <-
    lapply(diff_list, \(x) identical(character(0), x)) |>
    unlist()
  
  if (any(empty_char)) {
    cat(" There are no differences between", names[which(empty_char)], "and",
        names[which(!empty_char)])
    
    cat("\n There are",
        length(diff_list[[which(!empty_char)]]),
        "difference(s) between",
        names[which(!empty_char)],
        "and",
        names[which(empty_char)])
    
    cat("\n", diff_list[[which(!empty_char)]])
    
    
  } else if (all(empty_char)) {
    diff_list <- cat("There are no matches between",
                     names[which(empty_char)],
                     "and",
                     names[which(!empty_char)],
                     "in either direction")
    
  } else {
    cat("\n There are",
        length(diff_list[[1]]),
        "difference(s) between",
        names[[1]],
        "and",
        names[[2]])
    
    cat("\n", diff_list[[1]])
    
    cat("\n There are",
        length(diff_list[[2]]),
        "differences between",
        names[[2]],
        "and",
        names[[1]])
    
    cat("\n", diff_list[[1]])
    
  }
  
}
