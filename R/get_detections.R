#' Get detections data
#'
#' Get detections data, with options to filter results. Use `limit` to limit
#' the number of returned records.
#'
#' @param connection A connection to the ETN database. Defaults to `con`.
#' @param application_type Character. `acoustic_telemetry` or `cpod`.
#' @param network_project_code Character (vector). One or more network
#'   projects. Case-insensitive.
#' @param animal_project_code Character (vector). One or more animal projects.
#'   Case-insensitive.
#' @param start_date Character. Date in ISO 8601 format, e.g. `2018-01-01`.
#'   Date selection on month (e.g. `2018-03`) or year (e.g. `2018`) are
#'   supported as well.
#' @param end_date Character. Date in ISO 8601 format, e.g. `2018-01-01`.
#'   Date selection on month (e.g. 2018-03) or year (e.g. 2018) are
#'   supported as well.
#' @param station_name Character (vector). One or more deployment station
#'   names. Case-insensitive.
#' @param tag_id Character (vector). One or more tag identifiers.
#' @param receiver_id Character (vector). One or more receiver identifiers.
#' @param scientific_name Character (vector). One or more scientific names.
#' @param limit Logical. Limit the number of returned records to 100 (useful
#'  for testing purposes). Defaults to `FALSE`.
#'
#' @return A tibble with detections data, sorted by `tag_id` and `date_time`.
#'  See also
#'  [field definitions](https://inbo.github.io/etn/articles/etn_fields.html).
#'
#' @export
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr %>% arrange as_tibble
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#' con <- connect_to_etn(your_username, your_password)
#'
#' # Get detections filtered by start year, limited to 100 records by default
#' get_detections(con, start_date = "2017")
#'
#' # Get detections for a specific application type, limited to 100 records
#' get_detections(con, application_type = "acoustic_telemetry")
#'
#' # Get detections within a time frame for a specific animal project and
#' # network project
#' get_detections(
#'   con,
#'   animal_project_code = "phd_reubens",
#'   network_project_code = "thornton", start_date = "2011-01-28",
#'   end_date = "2011-02-01"
#' )
#'
#' # Get detections for a specific animal project at specific stations
#' get_detections(
#'   con,
#'   animal_project_code = "phd_reubens",
#'   station_name = c("R03", "R05")
#' )
#'
#' # Get detections for a specific tag
#' get_detections(
#'   con,
#'   tag_id = "A69-1303-65302"
#' )
#'
#' # Get detections for a specific receiver during a specific time period
#' get_detections(
#'   con,
#'   receiver_id = "VR2W-122360",
#'   start_date = "2015-12-03",
#'   end_date = "2015-12-05"
#' )
#' # Get detections for a specific species during a given period
#' get_detections(
#'   con,
#'   scientific_name = "Anguilla anguilla",
#'   start_date = "2015-12-03",
#'   end_date = "2015-12-05"
#' )
#' }
get_detections <- function(connection = con,
                           application_type = NULL,
                           network_project_code = NULL,
                           animal_project_code = NULL,
                           start_date = NULL,
                           end_date = NULL,
                           station_name = NULL,
                           tag_id = NULL,
                           receiver_id = NULL,
                           scientific_name = NULL,
                           limit = FALSE) {
  # Check connection
  check_connection(connection)

  # Check application_type
  if (is.null(application_type)) {
    application_type_query <- "True"
  } else {
    check_value(application_type, c("acoustic_telemetry", "cpod"), "application_type")
    application_type_query <- glue_sql("application_type IN ({application_type*})", .con = connection)
  }

  # Check network_project_code
  if (is.null(network_project_code)) {
    network_project_code_query <- "True"
  } else {
    network_project_code <- tolower(network_project_code)
    valid_network_project_codes <- tolower(list_network_project_codes(connection))
    check_value(network_project_code, valid_network_project_codes, "network_project_code")
    network_project_code_query <- glue_sql(
      "LOWER(network_project_code) IN ({network_project_code*})",
      .con = connection
    )
  }

  # Check animal_project_code
  if (is.null(animal_project_code)) {
    animal_project_code_query <- "True"
  } else {
    animal_project_code <- tolower(animal_project_code)
    valid_animal_project_codes <- tolower(list_animal_project_codes(connection))
    check_value(animal_project_code, valid_animal_project_codes, "animal_project_code")
    animal_project_code_query <- glue_sql(
      "LOWER(animal_project_code) IN ({animal_project_code*})",
      .con = connection
    )
  }

  # Check start_date
  if (is.null(start_date)) {
    start_date_query <- "True"
  } else {
    start_date <- check_date_time(start_date, "start_date")
    start_date_query <- glue_sql("date_time >= {start_date}", .con = connection)
  }

  # Check end_date
  if (is.null(end_date)) {
    end_date_query <- "True"
  } else {
    end_date <- check_date_time(end_date, "end_date")
    end_date_query <- glue_sql("date_time <= {end_date}", .con = connection)
  }

  # Check station_name
  if (is.null(station_name)) {
    station_name_query <- "True"
  } else {
    station_name <- tolower(station_name)
    valid_station_names <- tolower(list_station_names(connection))
    check_value(station_name, valid_station_names, "station_name")
    station_name_query <- glue_sql(
      "LOWER(station_name) IN ({station_name*})",
      .con = connection
    )
  }

  # Check tag_id
  if (is.null(tag_id)) {
    tag_id_query <- "True"
  } else {
    valid_tag_ids <- list_tag_ids(connection)
    check_value(tag_id, valid_tag_ids, "tag_id")
    tag_id_query <- glue_sql("tag_id IN ({tag_id*})", .con = connection)
  }

  # Check receiver_id
  if (is.null(receiver_id)) {
    receiver_id_query <- "True"
  } else {
    valid_receiver_ids <- list_receiver_ids(connection)
    check_value(receiver_id, valid_receiver_ids, "receiver_id")
    receiver_id_query <- glue_sql("receiver_id IN ({receiver_id*})", .con = connection)
  }

  # Check scientific_name
  if (is.null(scientific_name)) {
    scientific_name_query <- "True"
  } else {
    scientific_name_ids <- list_scientific_names(connection)
    check_value(scientific_name, scientific_name_ids, "scientific_name")
    scientific_name_query <- glue_sql("scientific_name IN ({scientific_name*})", .con = connection)
  }

  # Check limit
  assert_that(is.logical(limit), msg = "limit must be a logical: TRUE/FALSE.")
  if (limit) {
    limit_query <- glue_sql("LIMIT 100", .con = connection)
  } else {
    limit_query <- glue_sql("LIMIT ALL}", .con = connection)
  }

  # Build query
  query <- glue_sql("
    SELECT
      *
    FROM
      acoustic.detections_view2
    WHERE
      {application_type_query}
      AND {network_project_code_query}
      AND {animal_project_code_query}
      AND {start_date_query}
      AND {end_date_query}
      AND {station_name_query}
      AND {tag_id_query}
      AND {receiver_id_query}
      AND {scientific_name_query}
    {limit_query}
    ", .con = connection)
  detections <- dbGetQuery(connection, query)

  # Sort data (faster than in SQL)
  detections <-
    detections %>%
    arrange(
      factor(.data$tag_id, levels = list_tag_ids(connection)),
      .data$date_time
    )

  as_tibble(detections)
}
