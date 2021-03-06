con <- connect_to_etn(
  username = Sys.getenv("userid"),
  password = Sys.getenv("pwd")
)

# Expected column names
expected_col_names <- c(
  "pk",
  "receiver_id",
  "application_type",
  "network_project_code",
  "station_name",
  "station_description",
  "station_manager",
  "deploy_date_time",
  "deploy_latitude",
  "deploy_longitude",
  "intended_latitude",
  "intended_longitude",
  "mooring_type",
  "bottom_depth",
  "riser_length",
  "deploy_depth",
  "battery_installation_date",
  "battery_estimated_end_date",
  "activation_date_time",
  "recover_date_time",
  "recover_latitude",
  "recover_longitude",
  "download_date_time",
  "download_file_name",
  "valid_data_until_date_time",
  "sync_date_time",
  "time_drift",
  "ar_battery_installation_date",
  "ar_confirm",
  "transmit_profile",
  "transmit_power_output",
  "log_temperature_stats_period",
  "log_temperature_sample_period",
  "log_tilt_sample_period",
  "log_noise_stats_period",
  "log_noise_sample_period",
  "log_depth_stats_period",
  "log_depth_sample_period",
  "comments"
)

application1 <- "cpod"
project1 <- "ws1"
project_multiple <- c("ws1", "ws2")

deployments_all <- get_deployments(con)
deployments_application1 <- get_deployments(con, application_type = application1)
deployments_project1 <- get_deployments(con, network_project_code = project1)
deployments_project1_uppercase <- get_deployments(con, network_project_code = toupper(project1))
deployments_project_multiple <- get_deployments(con, network_project_code = project_multiple)
deployments_project1_openfalse <- get_deployments(con, network_project_code = project1, open_only = FALSE)

testthat::test_that("Test input", {
  expect_error(
    get_deployments("not_a_connection"),
    "Not a connection object to database."
  )
  expect_error(
    get_deployments(con, application_type = "not_an_application_type")
  )
  expect_error(
    get_deployments(con, network_project_code = "not_a_project")
  )
  expect_error(
    get_deployments(con, network_project_code = c("thornton", "not_a_project"))
  )
})

testthat::test_that("Test output type", {
  expect_is(deployments_all, "tbl_df")
  expect_is(deployments_application1, "tbl_df")
  expect_is(deployments_project1, "tbl_df")
  expect_is(deployments_project_multiple, "tbl_df")
  expect_is(deployments_project1_openfalse, "tbl_df")
})

testthat::test_that("Test column names", {
  expect_equal(names(deployments_all), expected_col_names)
  expect_equal(names(deployments_application1), expected_col_names)
  expect_equal(names(deployments_project1), expected_col_names)
  expect_equal(names(deployments_project_multiple), expected_col_names)
  expect_equal(names(deployments_project1_openfalse), expected_col_names)
})

testthat::test_that("Test number of records", {
  expect_gt(nrow(deployments_all), nrow(deployments_application1))
  expect_gt(nrow(deployments_all), nrow(deployments_project1))
  expect_gt(nrow(deployments_all), nrow(deployments_project_multiple))
  expect_gt(nrow(deployments_project_multiple), nrow(deployments_project1))
  expect_gte(nrow(deployments_project1_openfalse), nrow(deployments_project1))
})

testthat::test_that("Argument network_project_code is case-insensitive", {
  expect_equal(deployments_project1, deployments_project1_uppercase)
})

testthat::test_that("Test if data is filtered on parameter", {
  expect_equal(
    deployments_application1 %>% distinct(application_type) %>% pull(),
    c(application1)
  )
  expect_equal(
    deployments_project1 %>% distinct(network_project_code) %>% pull(),
    c(project1)
  )
  expect_equal(
    deployments_project_multiple %>% distinct(network_project_code) %>% arrange(network_project_code) %>% pull(),
    project_multiple
  )
  expect_equal(
    deployments_project1_openfalse %>% distinct(network_project_code) %>% pull(),
    c(project1)
  )
})

testthat::test_that("Test open ended date", {
  expect_true(
    all(deployments_project1 %>% select(recover_date_time) %>% is.na())
  )
  expect_equal(
    nrow(deployments_project1_openfalse %>% filter(is.na(recover_date_time))),
    nrow(deployments_project1)
  )
})
