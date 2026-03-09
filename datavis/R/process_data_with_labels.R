#' Process Data and Extract Labels
#'
#' This function cleans a data frame, converts columns to numeric where possible, and extracts variable labels.
#'
#' @param df A data frame, typically loaded from a Qualtrics CSV.
#' @param multi_cols Optional character string specifying a column name to retain as-is without numeric conversion. Default is NULL.
#' @return A list with two elements: \code{data} (the cleaned data frame) and \code{labels} (a named vector of variable labels).
#' @export

process_data_with_labels <- function(df, multi_cols = NULL) {
  question_labels <- as.character(df[1, ])
  variable_names <- names(df)
  label_mapping <- setNames(question_labels, variable_names)

  df_cleaned <- df[-(1:2), ]

  df_cleaned <- df_cleaned %>%
    mutate(across(everything(), ~ {
      col_name <- cur_column()
      if (!is.null(multi_cols) && col_name %in% multi_cols) {
        as.character(.)
      } else {
        suppressWarnings(as.numeric(as.character(.)))
      }
    }))

  return(list(data = df_cleaned, labels = label_mapping))
}
