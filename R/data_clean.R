#' Enforce Data Types
#'
#' Enforces data types on a tibble using a schema that maps feature names to dtypes.
#' @param tbl tibble to be dtyped
#' @param dtypes named list of character vectors: schema to be used for datatyping
#' names can include any of logical, double, int, date, factor, list
#' character vectors should be the names of all fields which should have that dtype
#' @param allcols logical: whether the whole tibble is being retyped,
#' in which case the function checks to ensure that there is a dtype provided
#' for every column
#' @keywords clean
#' @export
#' @examples
#' \dontrun{
#' fs_males_tbl_cleaned <- enforce_dtypes(fs_males_tbl, st_dtypes, TRUE)
#' }
enforce_dtypes <- function(tbl,
                           dtypes,
                           allcols = TRUE) {

  cols_in_data <- colnames(tbl)

  if (allcols) {
    # ensure that all columns have specified dtypes
    cols_with_dtypes <- purrr::reduce(dtypes, union)

    cols_without_dtypes <- setdiff(cols_in_data, cols_with_dtypes)
    if (length(cols_without_dtypes) > 0) {
      warning(sprintf("serotrackr does not specify data types for the following columns.
                      Please add dtypes for these columns to serosurvr/R/st_dtypes.R: %s",
                      paste(cols_without_dtypes, collapse = ", ")),
              call. = FALSE)
    }
  }

  # refactor opportunity below: some duplicated code
  # would be ideal to clean this up by creating a mapping between dtypes names and functions
  # and calling all relevant functions automatically
  tbl_retyped <- tbl %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(dplyr::intersect(dtypes$logical,
                                                                    cols_in_data)),
                                as.logical),
                  dplyr::across(tidyselect::all_of(dplyr::intersect(dtypes$double,
                                                                    cols_in_data)),
                                as.double),
                  dplyr::across(tidyselect::all_of(dplyr::intersect(dtypes$integer,
                                                                    cols_in_data)),
                                as.integer),
                  dplyr::across(tidyselect::all_of(dplyr::intersect(dtypes$character,
                                                                    cols_in_data)),
                                as.character),
                  dplyr::across(tidyselect::all_of(dplyr::intersect(dtypes$date,
                                                                    cols_in_data)),
                                lubridate::as_date),
                  dplyr::across(tidyselect::all_of(dplyr::intersect(dtypes$factor,
                                                                    cols_in_data)),
                                forcats::as_factor))

  tbl_retyped
}

#' Enforce SeroTracker Data Types
#'
#' Enforces SeroTracker data types on a tibble using a predefined schema.
#' @param tbl tibble to be dtyped
#' @seealso wrapper method for [serosurvr::enforce_dtypes]
#' @keywords clean
#' @export
#' @examples
#' \dontrun{
#' fs_males_tbl_cleaned <- enforce_st_dtypes(fs_males_tbl)
#' }
enforce_st_dtypes <- function(tbl) {
  enforce_dtypes(tbl, serosurvr::st_dtypes, TRUE)
}
