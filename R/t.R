t.tbl_df <-  function(x, 
                      ...) {
  
  df_transpose <- data.table::transpose(x, ...)
  dimnames(df_transpose) <- list(colnames(x), rownames(x))
  df_transpose <- tibble::rownames_to_column(df_transpose)
  df_transpose <- tibble::as_tibble(df_transpose)
  colnames(df_transpose) <- as.character(df_transpose[1, ])
  df_transpose <- df_transpose[2:nrow(df_transpose), ]
  
  file <- tempfile("df_transpose", fileext = ".txt")
  readr::write_tsv(df_transpose, file)
  df_transpose <- suppressWarnings(readr::read_tsv(file, col_types = cols()))
  return(df_transpose)
}

t.data.table <- function(x, ...) {
  return(data.table::transpose(x, ...))
}

as_tibble.matrix <- function(matrix,
                             rn_as_col = TRUE,
                             rn = "rn") {
  mat_attr <- purrr::map(1:2, ~ dimnames(matrix)[[.x]])
  dimnames(matrix) <- NULL
  df <- as.data.frame(matrix)
  
  if (rn_as_col) {
    colnames(df) <- mat_attr[[2]]
    df <-
      dplyr::mutate(df,!!as.name(rn) := mat_attr[[1]], .before = everything())
  } else {
    dimnames(df) <- list(mat_attr[[1]], mat_attr[[2]])
  }
  return(as_tibble(df))
}
