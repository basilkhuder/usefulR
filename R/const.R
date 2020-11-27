#Creates a constant variable type that cannot be changed. Similiar to Javascript's const.

`%=%` <- function(lhs, rhs){
  let <- deparse(substitute(lhs))
  if (exists(let, parent.frame(), inherits = FALSE)) { 
    stop (glue::glue("The Variable you are trying to create, {let}, already exists."))
    } else { 
      assign(let, rhs, parent.frame())
    } 
  } 
  
  
