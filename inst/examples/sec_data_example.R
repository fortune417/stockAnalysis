#' Example script showing how to use the stockAnalysis package with SEC data
#' 
library(stockAnalysis)

# Check what SEC data is available in the repository
sec_dir <- file.path("SEC", "2025q2")
if (!dir.exists(sec_dir)) {
  stop("SEC data directory not found. Please make sure SEC/2025q2 exists.")
}

# List available data files
cat("Available SEC data files:\n")
files <- list.files(sec_dir)
print(files)

# Read the submission data (smaller file to start with)
sub_data <- read_sec_sub(file.path(sec_dir, "sub.txt"))

# Show basic info about the data
cat("Submission data dimensions:", dim(sub_data), "\n")
cat("First few rows of submission data:\n")
print(head(sub_data[, c("cik", "name", "form", "period", "fy")]))

# Read a sample of the numbers data (since it's very large)
num_data <- read_sec_num(file.path(sec_dir, "num.txt"), n_max = 10000)

# Show basic info about numbers data
cat("Numbers data dimensions (sample):", dim(num_data), "\n")
cat("First few rows of numbers data:\n")
print(head(num_data[, c("adsh", "tag", "value", "ddate", "uom")]))

# Read tag data
tag_data <- read_sec_tag(file.path(sec_dir, "tag.txt"))

# Show basic info about tag data
cat("Tag data dimensions:", dim(tag_data), "\n")
cat("First few tags:\n")
print(head(tag_data[, c("tag", "tlabel", "datatype")]))

# Process a small sample of SEC data for a specific company (e.g., first CIK in the data)
sample_cik <- sub_data$cik[1]
cat("Processing data for sample CIK:", sample_cik, "\n")

# Process the data for the sample company
sec_sample <- process_sec_data(
  data_dir = sec_dir,
  cik = sample_cik,
  years = 2020:2025  # Recent years
)

# Merge the data
merged_sample <- merge_sec_data(sec_sample)

# Show the merged data
cat("Merged data dimensions:", dim(merged_sample), "\n")
cat("Available tags in sample:", length(unique(merged_sample$tag)), "\n")

# Calculate ratios for the sample company
if (nrow(merged_sample) > 0) {
  ratios <- calculate_ratios(merged_sample)
  cat("Calculated ratios for sample company:\n")
  print(head(ratios))
  
  # Create time series for revenue
  if ("revenue" %in% merged_sample$tag) {
    revenue_ts <- create_financial_ts(merged_sample, metrics = "revenue", company_cik = sample_cik)
    if (length(revenue_ts) > 0) {
      cat("Revenue time series for the company:\n")
      print(revenue_ts$revenue)
    }
  }
}

# Show how to find undervalued stocks (if we had proper metrics data)
cat("\nDemonstrating undervalued stock functions:\n")
cat("- Use find_undervalued_stocks() with proper metrics data\n")
cat("- Use add_valuation_score() to add valuation metrics\n")
cat("- Use rank_stocks_by_valuation() to rank stocks by value\n")

cat("\nTo run the Shiny app:\n")
cat("shiny::runApp(system.file('shiny-app', package = 'stockAnalysis'))\n")