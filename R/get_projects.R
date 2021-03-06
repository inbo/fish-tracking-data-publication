#' Get project data
#'
#' Get data for projects, with options to filter results.
#'
#' @param connection A connection to the ETN database. Defaults to `con`.
#' @param project_type Character. `animal` or `network`.
#' @param application_type Character. `acoustic_telemetry` or `cpod`.
#'
#' @return A tibble with project data, sorted by `project_code`. See also
#'   [field definitions](https://inbo.github.io/etn/articles/etn_fields.html).
#'
#' @export
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr %>% arrange as_tibble
#'
#' @examples
#' \dontrun{
#' con <- connect_to_etn(your_username, your_password)
#'
#' # Get all projects
#' get_projects(con)
#'
#' # Get all animal projects
#' get_projects(con, project_type = "animal")
#'
#' # Get all network projects
#' get_projects(con, project_type = "network")
#'
#' # Get projects for a specific application type
#' get_projects(con, application_type = "cpod")
#' }
get_projects <- function(connection = con,
                         project_type = NULL,
                         application_type = NULL) {
  # Check connection
  check_connection(connection)

  # Check project_type
  if (is.null(project_type)) {
    project_type_query <- "True"
  } else {
    check_value(project_type, c("animal", "network"), "project_type")
    project_type_query <- glue_sql("project_type IN ({project_type*})", .con = connection)
  }

  # Check application_type
  if (is.null(application_type)) {
    application_type_query <- "True"
  } else {
    check_value(application_type, c("acoustic_telemetry", "cpod"), "application_type")
    application_type_query <- glue_sql("application_type IN ({application_type*})", .con = connection)
  }

  # Build query
  query <- glue_sql("
    SELECT
      *
    FROM
      acoustic.projects_view2
    WHERE
      {project_type_query}
      AND {application_type_query}
    ", .con = connection)
  projects <- dbGetQuery(connection, query)

  # Sort data
  projects <-
    projects %>%
    arrange(.data$project_code)

  as_tibble(projects)
}
