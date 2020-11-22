con <- connect_to_etn(
  username = Sys.getenv("userid"),
  password = Sys.getenv("pwd")
)

# Expected column names
expected_col_names <- c(
  "pk",
  "date_time",
  "receiver_id",
  "application_type",
  "network_project_code",
  "tag_id",
  "tag_fk",
  "animal_id",
  "animal_project_code",
  "scientific_name",
  "station_name",
  "deploy_latitude",
  "deploy_longitude",
  "sensor_type",
  "sensor_value",
  "sensor_unit",
  "sensor_value_depth",
  "sensor_value_acceleration",
  "sensor_value_temperature",
  "signal_to_noise_ratio",
  "source_file",
  "qc_flag",
  "deployment_fk"
)


testthat::test_that("Error is returned for a wrong connection", {
  con_string = "not_a_connection"
  expect_error(
    get_detections(con_string),
    "Not a connection object to database."
  )
})


testthat::test_that("Argument limit works good", {

  # limit is logical and a tibble df is returned
  output <- get_detections(con, limit = TRUE)
  expect_equal(nrow(output), 100)
  expect_is(output, "tbl_df")

  # error is returned if limit is not a logical
  expect_error(get_detections(con, limit = 1),
               msg = "limit must be a logical: TRUE/FALSE.")
})


testthat::test_that("Error is returned for a wrong application type", {

  # wrong application_type
  wrong_application_type <- "not_an_application_type"
  expect_error(
    get_detections(con, application_type = wrong_application_type)
  )
})

testthat::test_that("It's possible to select by application type", {

  # acoustic telemetry data
  acoustic_telemetry <- "acoustic_telemetry"
  output <- get_detections(con,
                           application_type = acoustic_telemetry,
                           limit = TRUE)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(unique(output$application_type) == acoustic_telemetry)
})


testthat::test_that("Error is returned for a wrong network_project_code", {
  bad_project_name <- "not_a_project"
  expect_error(
    get_detections(con, network_project_code = bad_project_name)
  )
})


testthat::test_that("It's possible to select by network project code(s)", {

  # one project
  network_project <- "thornton"
  output <- get_detections(con,
                           network_project_code = network_project,
                           limit = TRUE)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(unique(output$network_project_code) == network_project)

  # multiple projects
  network_projects <- c("albert", "thornton")
  output <- get_detections(con,
                           network_project_code = network_projects,
                           limit = TRUE)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(all(unique(output$network_project_code) %in% network_projects))
})


testthat::test_that("Error is returned for a wrong animal_project_code", {
  bad_project_name <- "not_a_project"
  expect_error(
    get_detections(con, animal_project_code = bad_project_name)
  )
})


testthat::test_that("It's possible to select by animal project code(s)", {

  # one project
  animal_project <- "2010_phd_reubens"
  output <- get_detections(con,
                           animal_project_code = animal_project,
                           limit = TRUE)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(unique(output$animal_project_code) == animal_project)

  # multiple projects
  animal_projects <- c("2015_homarus", "2010_phd_reubens")
  output <- get_detections(con,
                           animal_project_code = animal_projects,
                           limit = TRUE)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(all(unique(output$animal_project_code) %in% animal_projects))
})


testthat::test_that("It's possible to get detections within a time period", {

  start_date <- "2011"
  end_date <- "2011-02-01"
  start_date <- as.POSIXct(check_date_time(start_date, "start_date"))
  end_date <- as.POSIXct(check_date_time(end_date, "end_date"))

  # only start
  output <-  get_detections(con,
                            start_date = start_date,
                            limit = TRUE)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(all(output$date_time >= start_date))

  # only end
  output <-  get_detections(con,
                            end_date = end_date,
                            limit = TRUE)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(all(output$date_time <= end_date))

  # both start and end
  output <-  get_detections(con,
                            start_date = start_date,
                            end_date = end_date,
                            limit = TRUE)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(all(output$date_time >= start_date &
                    output$date_time <= end_date))
})


testthat::test_that("Error is returned for a wrong station name", {
  wrong_station <- "wrong_station"
  expect_error(get_detections(con,
                              station_name = wrong_station,
                              limit = TRUE))
})


testthat::test_that("It's possible to select by station name(s)", {

  # one station
  station <- "R03"
  output <- get_detections(con,
                           station_name = station,
                           limit = TRUE)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(unique(output$station_name) == station)

  # multiple stations
  stations <- c("s-4c", "R03")
  output <- get_detections(con,
                           station_name = stations,
                           limit = TRUE)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(all(unique(output$station_name) %in% stations))
})


testthat::test_that("Error is returned for a wrong tag", {
  wrong_tag <- "wrong_tag"
  expect_error(get_detections(con,
                              tag_id = wrong_tag,
                              limit = TRUE))
})


testthat::test_that("It's possible to select by tag(s)", {

  # one tag
  tag <- "A69-1303-65302"
  output <- get_detections(con, tag_id = tag, limit = TRUE)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(unique(output$tag_id) == tag)

  # multiple tags
  tags <- c("A69-1303-65304", "A69-1303-65302")
  output <- get_detections(con,
                           tag_id = tags,
                           limit = TRUE)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(all(unique(output$tag_id) %in% tags))
})


testthat::test_that("Error is returned for a wrong receiver", {
  wrong_receiver <- "wrong_receiver"
  expect_error(get_detections(con,
                              receiver_id = wrong_receiver,
                              limit = TRUE))
})


testthat::test_that("It's possible to select by receiver(s)", {

  # one receiver
  receiver <- "VR2W-122360"
  output <- get_detections(con, receiver_id = receiver, limit = TRUE)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(unique(output$receiver_id) == receiver)

  # multiple receivers
  receivers <- c("VR2W-110784", "VR2W-122360")
  output <- get_detections(con,
                           receiver_id = receivers,
                           limit = TRUE)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(all(unique(output$receiver_id) %in% receivers))
})


testthat::test_that("Error is returned for a wrong scientific name", {
  wrong_scientific_name <- "wrong_scientific_name"
  expect_error(get_detections(con,
                              scientific_name = wrong_scientific_name,
                              limit = TRUE))
})


testthat::test_that("It's possible to select by scientific name(s)", {

  # one name
  scientific_name <- "Gadus morhua"
  output <- get_detections(con, scientific_name = scientific_name, limit = TRUE)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(unique(output$scientific_name) == scientific_name)

  # multiple names
  scientific_names <- c("Anguilla anguilla", "Gadus morhua")
  output <- get_detections(con,
                           scientific_name = scientific_names,
                           limit = TRUE)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(all(unique(output$scientific_name) %in% scientific_name))
})
