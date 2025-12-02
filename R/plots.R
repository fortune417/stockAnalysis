# functions for plots

#' Plot financial time series
#'
#' This function creates time series plots of financial metrics.
#'
#' @param fs_data Financial statement data
#' @param metrics Character vector of metrics to plot
#' @param company_cik CIK of the company to plot (optional, if NULL plots all)
#' @param title Plot title
#' @return A ggplot object
plot_financial_ts<-function(fs_data, metrics, company_cik=NULL, title="Financial Metrics Time Series") {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for this function")
  }

  # Filter by company if specified
  if(!is.null(company_cik)) {
    fs_data <- fs_data[fs_data$cik == company_cik, ]
  }

  # Ensure period is converted to date
  if("period_date" %in% colnames(fs_data)) {
    fs_data$period_date <- as.Date(fs_data$period_date)
  } else {
    fs_data$period_date <- as.Date(as.character(fs_data$period), format="%Y%m%d")
  }

  # Filter for requested metrics
  fs_filtered <- fs_data[fs_data$tag %in% metrics, ]

  # Create the plot
  p <- ggplot(fs_filtered, aes(x=period_date, y=value, color=tag)) +
    geom_line(size=1) +
    geom_point(size=2) +
    labs(
      title = title,
      x = "Date",
      y = "Value",
      color = "Metric"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  return(p)
}

#' Plot financial ratios
#'
#' This function creates visualizations of financial ratios.
#'
#' @param ratios_data Data frame containing calculated ratios
#' @param ratio_name Name of the ratio to plot
#' @param company_cik CIK to filter (optional)
#' @param title Plot title
#' @return A ggplot object
plot_ratios<-function(ratios_data, ratio_name, company_cik=NULL, title="Financial Ratio Trend") {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for this function")
  }

  # Filter for the specific ratio
  if(!ratio_name %in% colnames(ratios_data)) {
    stop("Ratio '", ratio_name, "' not found in ratios_data")
  }

  # Filter by company if specified
  if(!is.null(company_cik)) {
    ratios_data <- ratios_data[ratios_data$cik == company_cik, ]
  }

  # Create the plot
  p <- ggplot2::ggplot(ratios_data, ggplot2::aes(x=period_date, y=get(ratio_name))) +
    ggplot2::geom_line(size=1, color="steelblue") +
    ggplot2::geom_point(size=2, color="steelblue") +
    ggplot2::labs(
      title = title,
      x = "Date",
      y = ratio_name
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent)

  return(p)
}

#' Compare financial metrics across companies
#'
#' This function creates comparative plots of financial metrics across multiple companies.
#'
#' @param fs_data Financial statement data
#' @param metrics Character vector of metrics to compare
#' @param companies Vector of CIKs or company names to compare
#' @param period Date or year to compare (most recent if NULL)
#' @param title Plot title
#' @return A ggplot object
plot_compare_companies<-function(fs_data, metrics, companies, period=NULL, title="Company Comparison") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for this function")
  }

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required for this function")
  }

  # Filter for requested companies and metrics
  fs_filtered <- fs_data[fs_data$cik %in% companies & fs_data$tag %in% metrics, ]

  # If period is not specified, use the most recent data
  if(is.null(period)) {
    # Convert period to date if needed
    if("period_date" %in% colnames(fs_filtered)) {
      fs_filtered$period_date <- as.Date(fs_filtered$period_date)
    } else {
      fs_filtered$period_date <- as.Date(as.character(fs_filtered$period), format="%Y%m%d")
    }

    # Get the most recent period for each company and metric
    fs_filtered <- dplyr::group_by(fs_filtered, cik, tag)
    fs_filtered <- dplyr::slice_max(fs_filtered, order_by = period_date, n = 1)
    fs_filtered <- dplyr::ungroup(fs_filtered)
  }

  # Create the comparison plot
  p <- ggplot2::ggplot(fs_filtered, ggplot2::aes(x=cik, y=value, fill=tag)) +
    ggplot2::geom_bar(stat="identity", position="dodge") +
    ggplot2::labs(
      title = title,
      x = "Company",
      y = "Value",
      fill = "Metric"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  return(p)
}

#' Plot financial health indicators
#'
#' This function creates visualizations of financial health indicators.
#'
#' @param health_data Data frame containing health indicators
#' @param indicators Character vector of health indicators to plot
#' @param company_cik CIK to filter (optional)
#' @param title Plot title
#' @return A ggplot object
plot_health_indicators<-function(health_data, indicators, company_cik=NULL, title="Financial Health Indicators") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for this function")
  }

  # Filter by company if specified
  if(!is.null(company_cik)) {
    health_data <- health_data[health_data$cik == company_cik, ]
  }

  # For this function, we'll assume health_data is already in the format we need
  # with columns for different health indicators

  # Convert to long format for plotting
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("reshape2 package is required for this function")
  }

  # Select only the requested indicators
  indicators_to_plot <- intersect(indicators, colnames(health_data))
  if(length(indicators_to_plot) == 0) {
    stop("None of the requested indicators are found in health_data")
  }

  # Melt the data for plotting
  health_melted <- reshape2::melt(health_data,
                                  id.vars = c("cik", "period_date", "year"),
                                  measure.vars = indicators_to_plot,
                                  variable.name = "indicator",
                                  value.name = "value")

  # Create the plot
  p <- ggplot2::ggplot(health_melted, ggplot2::aes(x=period_date, y=value, color=indicator)) +
    ggplot2::geom_line(size=1) +
    ggplot2::geom_point(size=2) +
    ggplot2::labs(
      title = title,
      x = "Date",
      y = "Value",
      color = "Indicator"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  return(p)
}

#' Create a pie chart for financial composition
#'
#' @param data A data frame with category and value columns
#' @param category_col Name of the column containing categories
#' @param value_col Name of the column containing values
#' @param title Plot title
#' @return A ggplot object
pie_plot<-function(data, category_col, value_col, title="Financial Composition") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for this function")
  }

  # Check if the specified columns exist
  if (!category_col %in% colnames(data) || !value_col %in% colnames(data)) {
    stop("Specified columns not found in data")
  }

  # Create the pie chart using ggplot2
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x="", y=value_col, fill=category_col)) +
    ggplot2::geom_bar(stat="identity", width=1, color="white") +
    ggplot2::coord_polar("y", start=0) +
    ggplot2::labs(
      title = title,
      fill = "Category"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  return(p)
}