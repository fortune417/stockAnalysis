# Additional unit tests for stockAnalysis package functions

library(testthat)
library(stockAnalysis)

# Test more financial analysis functions
test_that("robust_lm works correctly", {
  # Create test data with a clear linear relationship
  x <- 1:5
  y <- 2 * x + 1  # Perfect linear relationship

  # Test with exponential model
  result_exp <- suppressWarnings(robust_lm(data.frame(x=x, y=y), model="exp"))
  expect_true(is.numeric(result_exp) | is.na(result_exp))

  # Test with linear model
  result_lin <- robust_lm(data.frame(x=x, y=y), model="linear")
  expect_true(is.numeric(result_lin) | is.na(result_lin))
})

test_that("create_financial_ts works correctly", {
  # Create test data
  test_data <- data.frame(
    cik = rep("0000320193", 5),
    period_date = as.Date(c("2020-12-31", "2019-12-31", "2018-12-31", "2017-12-31", "2016-12-31")),
    tag = rep("revenue", 5),
    value = c(274515, 260174, 265595, 229234, 215639),
    stringsAsFactors = FALSE
  )
  
  # Test creating time series
  result <- create_financial_ts(test_data, metrics = "revenue", company_cik = "0000320193")
  
  # Check that result is a named list
  expect_true(is.list(result))
  expect_true("revenue" %in% names(result))
})

test_that("stock_screen works correctly", {
  # Create test data
  test_data <- data.frame(
    cik = c("0000320193", "0000789019", "0001652044"),  # Apple, Microsoft, Google
    period = c(20201231, 20201231, 20201231),
    tag = c("roa", "roa", "roa"),
    value = c(0.15, 0.18, 0.12),
    stringsAsFactors = FALSE
  )
  
  # Test screening with criteria
  result <- stock_screen(
    test_data,
    criteria = list(roa = list(min = 0.14)),
    weights = list(roa = 1.0)
  )
  
  # Check that result is a data frame
  expect_true(is.data.frame(result))
})

test_that("add_valuation_score works correctly", {
  # Create test data with valuation metrics
  test_data <- data.frame(
    ticker = "AAPL",
    `Last/userPrice.mean` = 0.8,  # 20% discount = potentially undervalued
    debtToEquity = 1.2,
    currentRatio = 1.0,
    roa = 0.15,
    rate.revenue.5 = 0.1,
    rate.epsDiluted.5 = 0.12,
    stringsAsFactors = FALSE
  )
  
  # Add other necessary columns
  test_data$eps <- 3.5
  test_data$sps <- 20.0
  
  # Test adding valuation score
  result <- add_valuation_score(test_data)
  
  # Check that valuation score was added
  expect_true("valuation_score" %in% colnames(result))
  expect_true(is.numeric(result$valuation_score))
})

test_that("rank_stocks_by_valuation works correctly", {
  # Create test data with valuation scores
  test_data <- data.frame(
    ticker = c("AAPL", "MSFT", "GOOGL"),
    valuation_score = c(85, 90, 75),
    stringsAsFactors = FALSE
  )
  
  # Test ranking stocks
  result <- rank_stocks_by_valuation(test_data)
  
  # Check that result is a data frame ordered by valuation score (descending)
  expect_true(is.data.frame(result))
  expect_equal(result$ticker[1], "MSFT")  # Highest score should be first
})

test_that("export functions work correctly", {
  # Create test data
  test_data <- data.frame(
    company = c("Apple", "Microsoft"),
    revenue = c(274515, 143015),
    stringsAsFactors = FALSE
  )
  
  # Test export to CSV
  temp_csv <- tempfile(fileext = ".csv")
  export_to_csv(test_data, temp_csv)
  expect_true(file.exists(temp_csv))
  unlink(temp_csv)
  
  # Note: Testing export to Excel and PDF would require those dependencies
})

test_that("merge_sec_data works correctly", {
  # Create minimal test data structure similar to what process_sec_data returns
  test_submissions <- data.frame(
    adsh = "0000320193-20-000010",
    cik = "0000320193",
    name = "Apple Inc",
    period = 20201231,
    form = "10-K",
    fy = 2020,
    stringsAsFactors = FALSE
  )
  
  test_tags <- data.frame(
    tag = "revenue",
    version = "us-gaap/2020",
    tlabel = "Revenue",
    datatype = "monetary",
    stringsAsFactors = FALSE
  )
  
  test_numbers <- data.frame(
    adsh = "0000320193-20-000010",
    tag = "revenue",
    version = "us-gaap/2020",
    ddate = 20201231,
    qtrs = 4,
    uom = "USD",
    value = 274515000000,
    stringsAsFactors = FALSE
  )
  
  # Create the sec_data structure
  sec_data <- list(
    submissions = test_submissions,
    tags = test_tags,
    numbers = test_numbers
  )
  
  # Test merging the data
  result <- merge_sec_data(sec_data)
  
  # Check that result is a data frame
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})

test_that("calculate_ratios works correctly with more metrics", {
  # Create test data with multiple financial metrics
  test_data <- data.frame(
    cik = rep("0000320193", 6),
    period = rep(20201231, 6),
    tag = c("revenue", "netIncome", "totalAsset", "shareholderEquity", "totalLiability", "totalCurLiability"),
    value = c(274515, 57411, 323888, 65339, 258549, 100000),  # in millions
    stringsAsFactors = FALSE
  )

  # Test calculating ratios with specific ratios
  result <- calculate_ratios(test_data, ratios = c("roa", "roequity", "debt_ratio"))

  # Check that result is a data frame with the expected ratios
  expect_true(is.data.frame(result))
  expect_true("roa" %in% colnames(result))
  expect_true("roequity" %in% colnames(result))
  expect_true("debt_ratio" %in% colnames(result))
})

test_that("dcf function edge cases", {
  # Test with zero growth
  result1 <- dcf(profits=100, g=0, r=0.1, multiple=15)
  expect_true(is.finite(result1))
  
  # Test with different terminal models
  result2 <- dcf(profits=100, g=0.05, r=0.1, multiple=15, terminalModel="multiple")
  expect_true(is.finite(result2))
  
  result3 <- dcf(profits=100, g=0.03, r=0.08, terminalG=0.02, terminalModel="perpetual") 
  expect_true(is.finite(result3))
})

test_that("reduce_digits works with different data types", {
  # Test with matrix
  test_matrix <- matrix(c(1.1234567, 2.9876543, 3.4567891, 4.5678901), nrow=2)
  result1 <- reduce_digits(test_matrix, digits=3)
  expect_equal(dim(result1), dim(test_matrix))
  
  # Test with data frame
  test_df <- data.frame(
    col1 = c(1.12345, 2.98765),
    col2 = c(3.45678, 4.56789),
    stringsAsFactors = FALSE
  )
  result2 <- reduce_digits(test_df, digits=2)
  expect_true(is.data.frame(result2))
  
  # Test with vector
  test_vec <- c(1.12345, 2.98765, 3.45678)
  result3 <- reduce_digits(test_vec, digits=2)
  expect_true(is.vector(result3))
})