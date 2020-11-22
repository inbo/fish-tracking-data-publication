con <- connect_to_etn(
  username = Sys.getenv("userid"),
  password = Sys.getenv("pwd")
)

# Expected column names
expected_col_names <- c(
  "pk",
  "animal_id",
  "animal_project_code",
  "tag_id",
  "tag_fk",
  "scientific_name",
  "common_name",
  "aphia_id",
  "animal_label",
  "animal_nickname",
  "tagger",
  "capture_date_time",
  "capture_location",
  "capture_latitude",
  "capture_longitude",
  "capture_method",
  "capture_depth",
  "capture_temperature_change",
  "release_date_time",
  "release_location",
  "release_latitude",
  "release_longitude",
  "recapture_date_time",
  "length1_type",
  "length1",
  "length1_unit",
  "length2_type",
  "length2",
  "length2_unit",
  "length3_type",
  "length3",
  "length3_unit",
  "length4_type",
  "length4",
  "length4_unit",
  "weight",
  "weight_unit",
  "age",
  "age_unit",
  "sex",
  "life_stage",
  "wild_or_hatchery",
  "stock",
  "surgery_date_time",
  "surgery_location",
  "surgery_latitude",
  "surgery_longitude",
  "treatment_type",
  "tagging_type",
  "tagging_methodology",
  "dna_sample",
  "sedative",
  "sedative_concentration",
  "anaesthetic",
  "buffer",
  "anaesthetic_concentration",
  "buffer_concentration_in_anaesthetic",
  "anaesthetic_concentration_in_recirculation",
  "buffer_concentration_in_recirculation",
  "dissolved_oxygen",
  "pre_surgery_holding_period",
  "post_surgery_holding_period",
  "holding_temperature",
  "comments"
)


testthat::test_that("Error is returned for a wrong connection", {
  con_string = "not_a_connection"
  expect_error(get_animals(con_string), "Not a connection object to database.")
})


testthat::test_that("Error is returned for a wrong animal_id", {

  # animal id is a string
  wrong_class <- "wrong_animal_id"
  expect_error(get_animals(con, animal_id = wrong_class))

  # animal id is a decimal number
  decimal_number <- 666.12
  expect_error(get_animals(con, animal_id = decimal_number))

  # animal id is a negative number
  negative_number <- -1
  expect_error(get_animals(con, animal_id = negative_number))
})


testthat::test_that("It's possible to select by animal id", {

  # animal id is an integer
  animal_id <- 666
  output <- get_animals(con, animal_id = animal_id)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(unique(output$animal_id) == animal_id)

  # animal id is the character version of an integer
  animal_id_string <- as.character(animal_id)
  output_string <- get_animals(con, animal_id = animal_id_string)
  expect_is(output_string, "tbl_df")
  expect_equal(names(output_string), expected_col_names)
  expect_true(unique(output_string$animal_id) == animal_id)

  # same output for animal id as integer or character version of it
  expect_equivalent(output_string, output)

  # multiple animal ids
  animal_ids <- c(666, 667)
  output_multiple <- get_animals(con, animal_id = animal_ids)
  expect_is(output_multiple, "tbl_df")
  expect_equal(names(output_multiple), expected_col_names)
  expect_true(all(unique(output_multiple$animal_id) %in% animal_ids))

  # output for multiple animals hasn't less rows than output for one animal
  expect_gte(nrow(output_multiple), nrow(output))

  #' even if multiple animals are asked the same information is returned for a
  #' specific animal
  expect_equivalent(output_multiple[output_multiple$animal_id == animal_id,],
                    output)
})


testthat::test_that("Error is returned for a wrong animal_project_code", {
  bad_project_name <- "not_a_project"
  expect_error(get_animals(con, animal_project_code = bad_project_name))
})


testthat::test_that("It's possible to select by animal project code(s)", {

  # one project
  animal_project <- "2010_phd_reubens"
  output <- get_animals(con, animal_project_code = animal_project)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(unique(output$animal_project_code) == animal_project)

  # multiple projects
  animal_projects <- c("2015_homarus", "2010_phd_reubens")
  output_multiple <- get_animals(con, animal_project_code = animal_projects)
  expect_is(output_multiple, "tbl_df")
  expect_equal(names(output_multiple), expected_col_names)
  expect_true(all(unique(output_multiple$animal_project_code) %in%
                    animal_projects)
  )

  # output for multiple projects hasn't less rows than output for one project
  expect_gte(nrow(output_multiple), nrow(output))

  #' even if multiple projects are asked the same information is
  #' returned for a specific project
  expect_equivalent(
    output_multiple[output_multiple$animal_project_code == animal_project,],
    output
  )
})


testthat::test_that("Error is returned for a wrong scientific name", {
  wrong_scientific_name <- "wrong_scientific_name"
  expect_error(get_animals(con, scientific_name = wrong_scientific_name))
})


testthat::test_that("It's possible to select by scientific name(s)", {

  # one name
  scientific_name <- "Gadus morhua"
  output <- get_animals(con, scientific_name = scientific_name)
  expect_is(output, "tbl_df")
  expect_equal(names(output), expected_col_names)
  expect_true(unique(output$scientific_name) == scientific_name)

  # multiple names
  scientific_names <- c("Anguilla anguilla", "Gadus morhua")
  output_multiple <- get_animals(con, scientific_name = scientific_names)
  expect_is(output_multiple, "tbl_df")
  expect_equal(names(output_multiple), expected_col_names)
  expect_true(all(unique(output_multiple$scientific_name) %in% scientific_names))

  # output for multiple names hasn't less rows than output with one name
  expect_gte(nrow(output_multiple), nrow(output))

  #' even if multiple scientific names are asked the same information is
  #' returned for a specific one
  expect_equivalent(
    output_multiple[output_multiple$scientific_name == scientific_name,],
    output
  )
})
