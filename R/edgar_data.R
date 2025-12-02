#' SEC EDGAR data
#'
#' This object contains the URLs for the source
#' EDGAR data.
#' @format `edgarData`
#'    a list containing URLs
#' @source SEC EDGAR data API
"edgarData"

#' Stock tickers
#'
#' This dataset contains the stock tickers listed in
#' US stock exchanges and their company names.
#'
#' @format ## `tickers`
#' A data frame with 4 columns:
#' - cik: the unique central index key for the ticker.
#' - name: company name
#' - ticker: ticker symbol
#' - exchange: listing stock exchange
#'
#' @source SEC EDGAR data
"tickers"