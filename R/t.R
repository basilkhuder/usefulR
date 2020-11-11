t <- function(x, ...) {
  UseMethod("t")
}

t.tbl_df <-  function(x,
                      return_type = "tibble",
                      col_as_rnames = FALSE,
                      ...) {
  df_types <-
    setNames(
      c(
        "tibble::as_tibble",
        "as.data.frame",
        "data.table::as.data.table"
      ),
      c("tibble", "data.frame", "data.table")
    )
  
  df_transpose <- data.table::transpose(x)
  dimnames(df_transpose) <- list(colnames(x), rownames(x))
  df_transpose <- tibble::rownames_to_column(df_transpose)
  df_parse <- paste0(df_types[[return_type]], "(df_transpose)")
  df_transpose <- eval(parse(text = df_parse))
  colnames(df_transpose) <- as.character(df_transpose[1,])
  df_transpose <- df_transpose[2:nrow(df_transpose),]
  
  if (isTRUE(col_as_rnames)) {
    rownames(df_transpose) <- df_transpose[[1]]
    df_transpose <- df_transpose[-1]
  }
  
  return(df_transpose)
}

t.data.table <- function(x, ...) {
  return(data.table::transpose(x, ...))
}
