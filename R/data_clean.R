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
#' fs_males_tbl_cleaned <- enforce_dtypes(fs_males_tbl, serotracker_dtypes, TRUE)
enforce_dtypes <- function(tbl,
                           dtypes,
                           allcols = TRUE) {
  
  if (allcols) {
    # ensure that all columns have specified dtypes 
    dtyped_cols <- purrr::reduce(dtypes, union)
    all_cols <- colnames(tbl)
    
    undtyped_cols <- setdiff(all_cols, dtyped_cols)
    if (length(undtyped_cols) > 0) {
      warning(sprintf("serotrackr does not specify data types for the following columns.
                      Please add dtypes for these columns to serosurvr/R/st_dtypes.R: %s",
                      paste(undtyped_cols, collapse = ", ")),
              call. = FALSE)
    }
  }
  
  # refactor opportunity below: some duplicated code
  # would be ideal to clean this up by creating a mapping between dtypes names and functions
  # and calling all relevant functions automatically
  tbl_retyped <- tbl %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(dtypes$logical), as.logical),
                  dplyr::across(tidyselect::all_of(dtypes$double), as.double),
                  dplyr::across(tidyselect::all_of(dtypes$integer), as.integer),
                  dplyr::across(tidyselect::all_of(dtypes$chracter), as.character),
                  dplyr::across(tidyselect::all_of(dtypes$date), lubridate::as_date),
                  dplyr::across(tidyselect::all_of(dtypes$factor), forcats::as_factor))
        
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
#' fs_males_tbl_cleaned <- enforce_st_dtypes(fs_males_tbl)
enforce_st_dtypes <- function(tbl) {
  enforce_dtypes(tbl, st_dtypes, TRUE)
}