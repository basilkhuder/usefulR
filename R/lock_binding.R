`:=` <- function(lhs, rhs) { 
  var <- deparse(substitute(lhs))
  ifelse(exists(var, parent.frame(), inherits = FALSE), stop("Variable `", var, "` is already defined.", call. = FALSE),
         assign(var, rhs, parent.frame()))
} 
