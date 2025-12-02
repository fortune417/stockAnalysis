# functions for exporting data

#' Export financial data to CSV
#'
#' This function exports financial data to a CSV file.
#'
#' @param data The financial data to export
#' @param file Path to the output file
#' @param ... Additional arguments to pass to write.csv
export_to_csv <- function(data, file, ...) {
  utils::write.csv(data, file, row.names = FALSE, ...)
  message("Data exported to: ", file)
}

#' Export financial data to Excel
#'
#' This function exports financial data to an Excel file.
#'
#' @param data The financial data to export (can be a single data frame or list of data frames)
#' @param file Path to the output Excel file
#' @param sheet_names Names for the Excel sheets (if data is a list)
export_to_excel <- function(data, file, sheet_names = NULL) {
  # Check if writexl package is available
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required for Excel export. Please install it: install.packages('writexl')")
  }
  
  if (is.data.frame(data) || is.matrix(data)) {
    # Single data frame
    writexl::write_xlsx(data, file)
  } else if (is.list(data)) {
    # Multiple data frames
    if (is.null(sheet_names)) {
      sheet_names <- paste("Sheet", seq_along(data))
    }
    
    # Ensure sheet names are valid
    sheet_names <- make.names(sheet_names, unique = TRUE)
    
    # Limit sheet names to 31 characters (Excel limit)
    sheet_names <- substr(sheet_names, 1, 31)
    
    # Create a named list for export
    data_list <- data
    names(data_list) <- sheet_names
    
    writexl::write_xlsx(data_list, file)
  } else {
    stop("Data must be a data frame, matrix, or list of data frames")
  }
  
  message("Data exported to: ", file)
}

#' Export financial data to PDF report
#'
#' This function exports financial data to a PDF report with visualizations.
#'
#' @param data The financial data to include in the report
#' @param file Path to the output PDF file
#' @param title Title for the report
#' @param metrics Metrics to include in the report
export_to_pdf <- function(data, file, title = "Financial Analysis Report", metrics = NULL) {
  # Check if required packages are available
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required for PDF export. Please install it: install.packages('rmarkdown')")
  }

  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required for PDF export. Please install it: install.packages('knitr')")
  }

  # Create a temporary Rmd file for the report
  rmd_content <- create_financial_report_rmd(data, title, metrics)

  # Write the Rmd content to a temporary file
  temp_rmd <- base::tempfile(fileext = ".Rmd")
  base::writeLines(rmd_content, temp_rmd)

  # Render to PDF
  tryCatch({
    rmarkdown::render(
      temp_rmd,
      output_format = "pdf_document",
      output_file = file,
      quiet = TRUE
    )
    message("PDF report exported to: ", file)
  }, error = function(e) {
    stop("Error creating PDF: ", e$message)
  })
  
  # Clean up temporary file
  unlink(temp_rmd)
}

# Helper function to create R markdown content for financial reports
create_financial_report_rmd <- function(data, title, metrics) {
  # Start with the R Markdown header
  rmd <- c(
    "---",
    paste("title:", shQuote(title)),
    "output: pdf_document",
    "params:",
    "  data: !r data",
    "---",
    "",
    "# Financial Analysis Report",
    "",
    "## Executive Summary",
    "",
    paste("This report contains financial analysis based on data as of", Sys.Date()),
    "",
    "## Data Overview",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE)",
    "library(knitr)",
    "```",
    "",
    "The dataset contains data for the following metrics:",
    "",
    "```{r metrics}",
    "if (is.data.frame(params$data)) {",
    "  metric_names <- names(params$data)",
    "  if (!is.null(params$metrics)) {",
    "    metric_names <- intersect(metric_names, params$metrics)",
    "  }",
    "  head(data.frame(Metric = metric_names), 20)",
    "} else {",
    "  cat('Data is not in a data frame format')",
    "}",
    "```",
    "",
    "## Data Summary",
    "",
    "```{r summary}",
    "if (is.data.frame(params$data)) {",
    "  # Show summary statistics",
    "  numeric_cols <- sapply(params$data, is.numeric)",
    "  if (any(numeric_cols)) {",
    "    kable(summary(params$data[, numeric_cols, drop=FALSE]))",
    "  } else {",
    "    cat('No numeric columns found for summary')",
    "  }",
    "} else {",
    "  cat('Data is not in a data frame format')",
    "}",
    "```",
    "",
    "## Visualizations",
    "",
    "```{r plots, fig.width=10, fig.height=6}",
    "if (require(ggplot2) && is.data.frame(params$data)) {",
    "  # Example visualization - you could customize this based on your needs",
    "  # For now, just show a histogram of a numeric variable if available",
    "  numeric_cols <- sapply(params$data, is.numeric)",
    "  if (any(numeric_cols)) {",
    "    first_numeric_col <- params$data[[which(numeric_cols)[1]]]",
    "    p <- ggplot(data.frame(value = first_numeric_col), aes(x = value)) +",
    "      geom_histogram(bins = 20, fill = 'steelblue', alpha = 0.7) +",
    "      labs(title = 'Distribution of Numeric Values', x = 'Value', y = 'Frequency') +",
    "      theme_minimal()",
    "    print(p)",
    "  } else {",
    "    cat('No numeric columns available for visualization')",
    "  }",
    "} else {",
    "  cat('ggplot2 not available or data not in data frame format')",
    "}",
    "```",
    "",
    "# End of Report"
  )
  
  return(rmd)
}

#' Export portfolio analysis results
#'
#' This function exports portfolio analysis results in multiple formats.
#'
#' @param data The portfolio analysis data
#' @param base_filename Base name for the output files (without extension)
#' @param export_formats Character vector specifying export formats ('csv', 'excel', 'pdf')
#' @param ... Additional arguments to pass to export functions
export_portfolio_analysis <- function(data, base_filename, export_formats = c("csv", "excel"), ...) {
  
  if ("csv" %in% export_formats) {
    csv_file <- paste0(base_filename, ".csv")
    export_to_csv(data, csv_file, ...)
  }
  
  if ("excel" %in% export_formats) {
    excel_file <- paste0(base_filename, ".xlsx")
    export_to_excel(data, excel_file)
  }
  
  if ("pdf" %in% export_formats) {
    pdf_file <- paste0(base_filename, ".pdf")
    export_to_pdf(data, pdf_file)
  }
  
  message("Portfolio analysis exported in formats: ", paste(export_formats, collapse = ", "))
}