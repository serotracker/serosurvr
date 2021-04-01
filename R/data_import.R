#' Data Request Parameters
#'
#' Creates an object specifying parameters for a request for SeroTracker data.
#' @param reqname Request name. Used as an identifier and for caching.
#' @param research_fields Should research fields be pulled? Defaults to TRUE
#' @param prioritize_estimates Should one priority estimate per study be returned (TRUE), or all estimates (FALSE)? Defaults to FALSE.
#' @param prioritize_estimates_mode What mode should be used to prioritize estimates? Defaults to 'analysis_dynamic'; other options are 'analysis_static' and 'dashboard'
#' @param columns List or empty; if not empty, returns only the specified columns
#' @param sampling_start_date Filter results by sampling_start_date; format: YYYY-MM-DD
#' @param sampling_end_date Filter results by sampling_end_date; format: YYYY-MM-DD
#' @param publication_start_date Filter results by publication_start_date; format: YYYY-MM-DD
#' @param publication_end_date Filter results by publication_end_date; format: YYYY-MM-DD
#' @param filters Named list, with filter names and a list of allowed options for that filter
#' @seealso [serosurvr::get_data] which takes these params as input
#' @keywords import
#' @export
#' @examples
#' fs_males_params <- datareq_params(reqname = "france_spain_males",
#'                                   research_fields = TRUE,
#'                                   prioritize_estimates = TRUE,
#'                                   filters = list(country = list("France", "Spain"),
#'                                                  sex = list("Male")))
datareq_params <- function(reqname = character(),
                           research_fields = TRUE,
                           prioritize_estimates = FALSE,
                           prioritize_estimates_mode = 'analysis_dynamic',
                           columns = NULL,
                           sampling_start_date = NULL,
                           sampling_end_date = NULL,
                           publication_start_date = NULL,
                           publication_end_date = NULL,
                           filters = list()) {
  stopifnot(is.character(reqname) &
            is.logical(research_fields) &
            is.logical(prioritize_estimates) &
            is.character(prioritize_estimates_mode) &
            (is.list(columns) | is.null(columns)) &
            (is.character(sampling_start_date) | is.null(sampling_start_date)) &
            (is.character(sampling_end_date) | is.null(sampling_end_date)) &
            (is.character(publication_start_date) | is.null(publication_start_date)) &
            (is.character(publication_end_date) | is.null(publication_end_date)) &
            is.list(filters))

  params <- list(research_fields = research_fields,
                 prioritize_estimates = prioritize_estimates,
                 prioritize_estimates_mode = prioritize_estimates_mode,
                 columns = columns,
                 sampling_start_date = sampling_start_date,
                 sampling_end_date = sampling_end_date,
                 publication_start_date = publication_start_date,
                 publication_end_date = publication_end_date,
                 filters = filters)

  params <- params[!sapply(params, is.null)]

  structure(
    params,
    reqname = reqname,
    class = "datareq_params"
  )
}

#' Retrieve Data
#'
#' Request SeroTracker data from a data_provider endpoint
#' @param endpoint endpoint to be requested; "records" or "filter_options"
#' @param params [serosurvr::datareq_params] object itemizing request parameters
#' @seealso [serosurvr::get_data] and [serosurvr::filter_options] call this
#' @keywords import
retrieve_data <- function(endpoint, params = NA) {

  dotenv::load_dot_env(file = ".env")

  if (endpoint == "records") {

    stopifnot(inherits(params, "datareq_params"))
    url <- Sys.getenv("RECORDS_URL")
    response <- httr::POST(url = url,
                           body = params,
                           encode = "json")

  } else if (endpoint == "filter_options") {

    url <- Sys.getenv("FILTER_OPTIONS_URL")
    response <- httr::GET(url = url)

  } else {
    stop(sprintf("endpoint %s is an invalid endpoint option", endpoint))
  }

  if (httr::http_type(response) != "application/json") {
    stop("Endpoint did not return JSON", call. = FALSE)
  }

  result <- httr::content(response,
                          as = "text",
                          encoding = "UTF-8")

  if (httr::http_error(response)) {
    stop(
      sprintf(
        "Records endpoint request failed [%s]\n%s",
        httr::status_code(response),
        result
      ),
      call. = FALSE
    )
  }

  result <- jsonlite::fromJSON(result, simplifyDataFrame = TRUE)

  result
}

#' Get Data
#'
#' Gets serosurvey data from a cache or the SeroTracker servers.
#' If new data is requested, it is cached to the `path_to_cache_folder`.
#' Otherwise, new data is loaded from the `path_to_cache_folder`.
#' @param params [serosurvr::datareq_params] object itemizing request parameters
#' @param import_new_data logical: requests data from server if TRUE, from cache if FALSE
#' @param path_to_cache_folder path to cache new data to, or read cache from
#' @seealso calls [serosurvr::retrieve_data]
#' @keywords import
#' @export
#' @examples
#' \dontrun{
#' fs_males_tbl <- retrieve_data(fs_males_params,
#'                               TRUE,
#'                               '.cache')
#' }
get_data <- function(params,
                     import_new_data,
                     path_to_cache_folder) {
  stopifnot(inherits(params, "datareq_params") &
            is.logical(import_new_data) &
            is.character(path_to_cache_folder))

  reqname <- attr(params, "reqname")
  path_to_cache_file = fs::path(path_to_cache_folder,
                                reqname,
                                ext = "Rds")

  if (import_new_data) {
    fs::dir_create(path_to_cache_folder)

    tbl <-
      retrieve_data("records", params) %>%
      tibble::as_tibble() %>%
      readr::write_rds(path_to_cache_file,
                       compress = "gz")
  } else {
    if (!fs::file_exists(path_to_cache_file)) {
      stop(sprintf("No cache for request %s at filepath %s",
                   reqname,
                   path_to_cache_file))
    }
    tbl <- readr::read_rds(path_to_cache_file)
  }

  tbl
}

#' Filter Options
#'
#' Retrieve filter options from the data_provider/filter_options endpoint
#' @keywords import
#' @seealso Calls [serosurvr::retrieve_data]
#' @export
#' @examples
#' \dontrun{
#' filter_options()
#' }
filter_options <- function() {
  retrieve_data("filter_options")
}
