# Run all tests with the project library: uvr run tests/testthat.R
testthat::test_dir(here::here("tests/testthat"), stop_on_failure = TRUE)
