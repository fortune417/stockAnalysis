# functions to prepare data

#' Update SEC EDGAR data
#'
#' This function updates the latest versions of the
#' following files from SEC database.
#' - company_tickers_exchange.json
#' - company_tickers.json
#' - submissions.zip
#' - companyfacts.zip
#' The URLs for these files are in the object [edgarData]
#'
update_edgar_data<-function(outDir=NULL) {
  stopifnot(exists("edgarData"))
  if(is.null(outDir)) {
    outDir<-today(asStr=T)
  }
  if(!dir.exists(outDir)) {
    dir.create(outDir, recursive = T)
  }
  manualDownloadFiles<-c("companyfacts.zip","submissions.zip")
  status<-sapply(names(edgarData),
         function(x) {
           url<-edgarData[[x]]
           if(basename(url) %in% manualDownloadFiles) {
             message(glue::glue("Please download '{url}' manually and put it into {outDir}"))
             return(NULL)
           }
           dest<-file.path(outDir, basename(url))
           download_file(url, destfile=dest)
           return(dest)
         })
  return(invisible(status))
}


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
#' @source: see [edgarData$companyTickersExchange]
"tickers"

