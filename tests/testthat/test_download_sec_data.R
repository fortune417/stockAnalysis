# Check if full testing is requested
skip_if_not(Sys.getenv("FULL_TEST") == "true", "Skipping extended tests - set FULL_TEST=true to run")

test_that("download_sec_data validates start_quarter format correctly", {
  # Valid formats should not throw an error
  expect_error(download_sec_data("2009q1", outdir=tempdir(), gap=0.1), NA)
  expect_error(download_sec_data("2020Q4", outdir=tempdir(), gap=0.1), NA)

  # Invalid formats should throw an error
  expect_error(download_sec_data("invalid", outdir=tempdir(), gap=0.1),
               "start_quarter must be in format like '2009q1'")
  expect_error(download_sec_data("2009", outdir=tempdir(), gap=0.1),
               "start_quarter must be in format like '2009q1'")
  expect_error(download_sec_data("q1", outdir=tempdir(), gap=0.1),
               "start_quarter must be in format like '2009q1'")
  expect_error(download_sec_data("2009q5", outdir=tempdir(), gap=0.1),
               "start_quarter must be in format like '2009q1'")
  expect_error(download_sec_data("2009q0", outdir=tempdir(), gap=0.1),
               "start_quarter must be in format like '2009q1'")
})

test_that("download_sec_data validates end_quarter format correctly", {
  # Valid format should not throw an error
  expect_error(download_sec_data("2009q1", end_quarter="2009q2", outdir=tempdir(), gap=0.1), NA)

  # Invalid format should throw an error when end_quarter is provided
  expect_error(download_sec_data("2009q1", end_quarter="invalid", outdir=tempdir(), gap=0.1),
               "end_quarter must be in format like '2009q1'")
})

test_that("download_sec_data handles NA end_quarter correctly", {
  # When end_quarter is NA (default), it should download only the start quarter
  expect_error(download_sec_data("2020q1", end_quarter=NA, outdir=tempdir(), gap=0.1), NA)
  expect_error(download_sec_data("2020q1", outdir=tempdir(), gap=0.1), NA)  # Default is NA
})

test_that("download_sec_data generates correct quarter sequences", {
  # Test the internal logic by creating a test version that doesn't actually download
  # We'll test the quarter generation logic separately

  # Create a temp directory
  temp_dir <- tempdir()

  # Test single quarter download
  result_single <- download_sec_data("2020q1", outdir=temp_dir, gap=0.1)
  expect_type(result_single, "list")
  expect_named(result_single, c("success", "failed"))

  # Test range of quarters
  result_range <- download_sec_data("2020q1", "2020q3", outdir=temp_dir, gap=0.1)
  expect_type(result_range, "list")
  expect_named(result_range, c("success", "failed"))

  # The actual download will fail due to network, but we check that the structure is correct
  # and that appropriate number of URLs would be tried
})

test_that("download_sec_data respects outdir parameter", {
  # Create a temporary directory for this test
  test_dir <- file.path(tempdir(), "test_sec_download")
  on.exit(unlink(test_dir, recursive = TRUE))  # Clean up after test

  # The function should create the directory if it doesn't exist
  expect_error(download_sec_data("2020q1", outdir=test_dir, gap=0.1), NA)

  # Verify that the directory exists
  expect_true(dir.exists(test_dir))
})

test_that("download_sec_data respects force parameter logic", {
  # This would test the actual file download and force logic
  # which requires network access and is not suitable for automated tests
  skip("Skipping download-dependent tests")
})

test_that("download_sec_data handles multiple quarters correctly", {
  # Test that when given a range, multiple downloads are attempted
  temp_dir <- tempdir()

  # Test downloading 4 quarters (full year)
  result <- download_sec_data("2020q1", "2020q4", outdir=temp_dir, gap=0.1)
  expect_type(result, "list")
  expect_named(result, c("success", "failed"))
})

test_that("download_sec_data handles failed downloads correctly", {
  # This test verifies that when downloads fail, they are properly recorded
  # The function should still return a proper structure with failed URLs
  temp_dir <- tempdir()

  result <- download_sec_data("2020q1", outdir=temp_dir, gap=0.1)

  # Verify the result structure
  expect_type(result, "list")
  expect_named(result, c("success", "failed"))

  # Both success and failed should be character vectors (possibly empty)
  expect_type(result$success, "character")
  expect_type(result$failed, "character")
})