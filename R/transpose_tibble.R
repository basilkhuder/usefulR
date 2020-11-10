t.tbl_df <-  function(df,
                      return_type = "tibble",
                      col_as_rnames = FALSE,
                      ...) {
  
  df_types <- setNames(c("tibble::as_tibble", "as.data.frame", "data.table::as.data.table"),
                       c("tibble", "data.frame", "data.table"))
  
  df_transpose <- data.table::transpose(df)
  dimnames(df_transpose) <- list(colnames(df), rownames(df))
  df_transpose <- tibble::rownames_to_column(df_transpose)
  df_parse <- glue::glue("{df_types[[return_type]]}(transpose_df)")
  df_transpose <- eval(parse(text = df_parse))
  colnames(df_transpose) <- as.character(df_transpose[1,])
  df_transpose <- df_transpose[2:nrow(df_transpose),]
  
  if (isTRUE(col_as_rnames)){ 
    rownames(df_transpose) <- df_transpose[[1]]
    df_transpose <- df_transpose[-1]
  }
  
  return(df_transpose)
}
