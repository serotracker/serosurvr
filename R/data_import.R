#' Data Request Parameters
#'
#' Creates an object specifying parameters for a request for SeroTracker data.
#' @param reqname Request name. Used as an identifier and for caching.
#' @param research_fields Should research fields be pulled? Defaults to TRUE
#' @param prioritize_estimates If TRUE and estimates_subgroup not specified, will set estimates_subgroup to 'prioritize_estimates'. Defaults to FALSE.
#' @param estimates_subgroup Should one primary estimate per study be returned ('primary_estimates'), one priority estimate per study ('prioritize_estimates'), or all estimates ('all_estimates')? Defaults to 'all_estimates'.
#' @param prioritize_estimates_mode What mode should be used to prioritize estimates? Defaults to 'analysis_dynamic'; other options are 'analysis_static' and 'dashboard'
#' @param columns List or empty; if not empty, returns only the specified columns
#' @param sampling_start_date Filter results by sampling_start_date; format: YYYY-MM-DD
#' @param sampling_end_date Filter results by sampling_end_date; format: YYYY-MM-DD
#' @param publication_start_date Filter results by publication_start_date; format: YYYY-MM-DD
#' @param publication_end_date Filter results by publication_end_date; format: YYYY-MM-DD
#' @param include_in_srma Whether to filter results by a custom variable in AirTable;
#' not recommended to use this unless you have confirmed that the AirTable flag you are using
#' is properly configured to work with this variable
#' @param include_records_without_latlngs Whether to include records without latitude and longitude coordinates. Defaults to TRUE
#' @param include_disputed_regions Whether to include disputed regions. Defaults to TRUE
#' @param calculate_country_seroprev_summaries Defaults to FALSE to return only records
#' @param filters Named list, with filter names and a list of allowed options for that filter
#' @seealso [serosurvr::get_data] which takes these params as input
#' @keywords import
#' @export
#' @examples
#' fs_males_params <- datareq_params(reqname = "france_spain_males",
#'                                   research_fields = TRUE,
#'                                   estimates_subgroup = 'prioritize_estimates',
#'                                   filters = list(country = list("France", "Spain"),
#'                                                  sex = list("Male")))
datareq_params <- function(reqname = character(),
                           research_fields = TRUE,
                           prioritize_estimates = FALSE,
                           estimates_subgroup = 'all_estimates',
                           prioritize_estimates_mode = 'analysis_dynamic',
                           columns = NULL,
                           sampling_start_date = NULL,
                           sampling_end_date = NULL,
                           publication_start_date = NULL,
                           publication_end_date = NULL,
                           include_in_srma = NULL,
                           include_records_without_latlngs = TRUE,
                           include_disputed_regions = TRUE,
                           calculate_country_seroprev_summaries = FALSE,
                           filters = list()) {

  if (prioritize_estimates == TRUE & estimates_subgroup == 'all_estimates') {
    estimates_subgroup = 'prioritize_estimates'
  }

  stopifnot(is.character(reqname) &
              is.logical(research_fields) &
              is.logical(prioritize_estimates) &
              is.character(estimates_subgroup) &
              is.character(prioritize_estimates_mode) &
              (is.list(columns) | is.null(columns)) &
              (is.character(sampling_start_date) | is.null(sampling_start_date)) &
              (is.character(sampling_end_date) | is.null(sampling_end_date)) &
              (is.character(publication_start_date) | is.null(publication_start_date)) &
              (is.character(publication_end_date) | is.null(publication_end_date)) &
              (is.logical(include_in_srma) | is.null(include_in_srma)) &
              is.logical(include_records_without_latlngs) &
              is.logical(include_disputed_regions) &
              is.logical(calculate_country_seroprev_summaries) &
              is.list(filters))

  params <- list(research_fields = research_fields,
                 estimates_subgroup = estimates_subgroup,
                 prioritize_estimates_mode = prioritize_estimates_mode,
                 columns = columns,
                 sampling_start_date = sampling_start_date,
                 sampling_end_date = sampling_end_date,
                 publication_start_date = publication_start_date,
                 publication_end_date = publication_end_date,
                 include_in_srma = include_in_srma,
                 include_records_without_latlngs = include_records_without_latlngs,
                 include_disputed_regions = include_disputed_regions,
                 calculate_country_seroprev_summaries = calculate_country_seroprev_summaries,
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
#' @param server whether to use the production server ('prod') or a local dev server ('dev')
#' @seealso [serosurvr::get_data] and [serosurvr::filter_options] call this
#' @keywords import
retrieve_data <- function(endpoint,
                          params = NA,
                          server = 'prod') {

  if (server == 'prod') {
    records_envvar <- 'RECORDS_URL'
    filter_envvar <- 'FILTER_OPTIONS_URL'
  } else if (server == 'dev') {
    records_envvar <- 'RECORDS_DEV_URL'
    filter_envvar <- 'FILTER_OPTIONS_DEV_URL'
  } else {
    stop(sprintf('server must be either prod or dev'))
  }

  dotenv::load_dot_env(file = ".env")

  if (endpoint == "records") {

    stopifnot(inherits(params, "datareq_params"))
    url <- Sys.getenv(records_envvar)
    response <- httr::POST(url = url,
                           body = params,
                           encode = "json")

  } else if (endpoint == "filter_options") {

    url <- Sys.getenv(filter_envvar)
    response <- httr::GET(url = url)

  } else {
    stop(sprintf("endpoint %s is an invalid endpoint option", endpoint))
  }

  if (httr::http_type(response) != "application/json") {
    sprintf("Endpoint did not return JSON, trying again")
    Sys.sleep(15)
    stopifnot(inherits(params, "datareq_params"))
    url <- Sys.getenv(records_envvar)
    response <- httr::POST(url = url,
                           body = params,
                           encode = "json")
    if (httr::http_type(response) != "application/json") {
      stop("Endpoint did not return JSON", call. = FALSE)
    }
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
#' Note that if import_new_data is TRUE, data will be repeatedly requested from server
#' even if a cache is present, and that cache will be overwritten
#' import_new_data must be set to FALSE to use cached data
#' @param path_to_cache_folder path to cache new data to, or read cache from
#' @param server whether to use the production server ('prod') or a local dev server ('dev')
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
                     path_to_cache_folder,
                     server = 'prod') {
  stopifnot(inherits(params, "datareq_params") &
              is.logical(import_new_data) &
              is.character(path_to_cache_folder) &
              ((server == 'prod') | (server == 'dev')))

  reqname <- attr(params, "reqname")
  path_to_cache_file = fs::path(path_to_cache_folder,
                                reqname,
                                ext = "Rds")

  if (import_new_data) {
    fs::dir_create(path_to_cache_folder)

    tbl <-
      retrieve_data("records", params, server)$records %>%
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
#' @param server whether to use the production server ('prod') or a local dev server ('dev')
#' @seealso Calls [serosurvr::retrieve_data]
#' @export
#' @examples
#' \dontrun{
#' filter_options()
#' }
filter_options <- function(server = 'prod') {
  stopifnot((server == 'prod') | (server == 'dev'))
  retrieve_data("filter_options", server)
}
