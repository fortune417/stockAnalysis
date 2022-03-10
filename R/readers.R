# Functions to read data of different sources

#' Get the dictionary between raw and short/standard financial terms
#'
get_term_dict<-function() {
  nameDict<-c(
    "Cash & Equivalents", "cashAndEquivalent",
    "Short-Term Investments", "shortTermInvestment",
    "Cash & Cash Equivalents", "cashAndShortInvest",
    "Cash Growth", "cashGrowth",
    "Receivables", "receivables",
    "Inventory", "inventory",
    "Other Current Assets", "otherCurAsset",
    "Total Current Assets", "totalCurAsset",
    "Property, Plant & Equipment", "ppe",
    "Long-Term Investments", "longTermInvestment",
    "Goodwill and Intangibles", "goodwill",
    "Other Long-Term Assets", "otherLongTermAsset",
    "Total Long-Term Assets", "totalLongTermAsset",
    "Total Assets", "totalAsset",
    "Accounts Payable", "accountPayable",
    "Deferred Revenue", "deferredRevenue",
    "Current Debt", "currentDebt",
    "Other Current Liabilities", "otherCurLiability",
    "Total Current Liabilities", "totalCurLiability",
    "Long-Term Debt", "longTermDebt",
    "Other Long-Term Liabilities", "otherLongTermLiability",
    "Total Long-Term Liabilities", "totalLongTermLiability",
    "Total Liabilities", "totalLiability",
    "Total Debt", "totalDebt",
    "Debt Growth", "debtGrowth",
    "Common Stock", "commonStock",
    "Retained Earnings", "retainedEarning",
    "Comprehensive Income", "comprehensiveIncome",
    "Shareholders' Equity", "shareholderEquity",
    "Total Liabilities and Equity", "totalLiabilityAndEquity",
    "Net Cash / Debt", "netCashDebt",
    "Net Cash / Debt Growth", "netCashDebtGrowth",
    "Net Cash Per Share", "netCashPerShare",
    "Working Capital", "workingCapital",
    "Book Value Per Share", "bps",
    "Net Income","netIncome",
    "Depreciation & Amortization", "da",
    "Share-Based Compensation","shareBasedCompensation",
    "Other Operating Activities","otherOpActivity",
    "Operating Cash Flow","opCashFlow",
    "Capital Expenditures","capex",
    "Acquisitions","aquisitions",
    "Change in Investments","changeInvestment",
    "Other Investing Activities","otherInvestActivity",
    "Investing Cash Flow","investCashFlow",
    "Dividends Paid","dividendPaid",
    "Share Issuance / Repurchase","sharePurchase",
    "Debt Issued / Paid","debtPaid",
    "Other Financing Activities", "otherFinActivity",
    "Financing Cash Flow","finCashFlow",
    "Net Cash Flow","netCashFlow",
    "Free Cash Flow","freeCashFlow",
    "Free Cash Flow Growth","freeCashFlowGrowth",
    "Free Cash Flow Margin","freeCashFlowMargin",
    "Free Cash Flow Per Share","fcfps",
    "Revenue","revenue",
    "Revenue Growth","revenueGrowth",
    "Cost of Revenue","costOfRevenue",
    "Gross Profit","grossProfit",
    "Selling, General & Admin","sga",
    "Research & Development","rd",
    "Other Operating Expenses","otherOpExpense",
    "Operating Expenses","opExpense",
    "Operating Income","opIncome",
    "Other Expense / Income","otherIncome",
    "Pretax Income","pretaxIncome",
    "Income Tax","incomeTax",
    "Net Income","netIncome",
    "Shares Outstanding (Basic)","shareCountBasic",
    "Shares Outstanding (Diluted)","shareCountDiluted",
    "Shares Change","shareCountChange",
    "EPS (Basic)","epsBasic",
    "EPS (Diluted)","epsDiluted",
    "EPS Growth","epsGrowth",
    "Free Cash Flow Per Share","fcfps",
    "Dividend Per Share","dps",
    "Dividend Growth","dividendGrowth",
    "Gross Margin","grossMargin",
    "Operating Margin","opMargin",
    "Profit Margin","profitMargin",
    "FCF Margin","fcfMargin",
    "Effective Tax Rate","taxRate",
    "EBITDA","ebitda",
    "EBITDA Margin","ebitdaMargin",
    "EBIT","ebit",
    "EBIT Margin","ebitMargin",
    "Market Capitalization","marketCap",
    "Enterprise Value","ev",
    "PE Ratio","pe",
    "PS Ratio","ps",
    "PB Ratio","pb",
    "P/FCF Ratio","pfcf",
    "P/OCF Ratio","pocf",
    "EV/Sales Ratio","evSalesRatio",
    "EV/EBITDA Ratio","evEbitdaRatio",
    "EV/EBIT Ratio","evEbitRatio",
    "EV/FCF Ratio","evFcfRatio",
    "Debt / Equity Ratio","de",
    "Current Ratio","curRatio",
    "Inventory Turnover","inventoryTurnover",
    "Return on Equity (ROE)","roe",
    "Return on Assets (ROA)","roa",
    "Return on Capital (ROIC)","roic",
    "Earnings Yield","earningYield",
    "FCF Yield","fcfYield",
    "Payout Ratio","payoutRatio",
    "Interest Expense / Income", "interestExpense",
    "Preferred Dividends", "preferredDividends",
    "Net Income Common", "netIncome",
    "Asset Turnover", "assetTurnover",
    "Dividend Yield", "dividendYield",
    "Cash Growth (yoy)", "cashGrowthYoY",
    "Debt Growth (yoy)", "debtGrowthYoY",
    "Net cash / Debt growth (yoy)", "netCashGrowthYoY",
    "Free Cash Flow Growth (yoy)", "fcfGrowthYoY",
    "Revenue Growth (yoy)", "revenueGrowthYoY",
    "Shares Change (yoy)", "shareCountChangeYoY",
    "Eps Growth (yoy)", "epsGrowthYoY",
    "Dividend Growth (yoy)", "dividendGrowthYoY"
  )
  nameDict<-unique(matrix(nameDict, nc=2, byrow = T))
  stopifnot(!any(duplicated(nameDict[,1])))
  rownames(nameDict)<-tolower(nameDict[,1])
  colnames(nameDict)<-c("rawName","standardName")
  return(nameDict)
}

#' Read fundamental financial data
#'
#' This function reads the financial data of a company
#' given a stock symbol
#'
#' @param ticker A stock symbol, such as 'AAPL'
#' @param src Data source, currently support "sa" (stockanalysis.com) and 'mt' (macrotrends.net)
#' @param srcDir As an alternative to src, one can directly provide the folder containing the data files
#'
#' @return an object of FinData
#' @export
read_financials<-function(ticker,
                          src=c("sa","mt","fmp", "sec"),
                          period=c("annual","quarterly"),
                          srcDir=NULL, ...) {
  src<-match.arg(src)
  if(is.null(srcDir)) {
	if(src %in% c("fmp","sec")) {
		stop(glue::glue("data source '{src}' is not implemented yet"))
	}
    srcDir<-system.file(file.path("extdata", src), package = "stockAnalysis")
  }
  if(srcDir == "") {
    stop("argument 'srcDir' is required")
  }
  ticker<-tolower(ticker)
  # find files
  period<-match.arg(period)
  bsFile<-paste(ticker, "balance-sheet", period, sep="-")
  cfFile<-paste(ticker, "cash-flow-statement", period, sep="-")
  isFile<-paste(ticker, "income-statement", period, sep="-")
  ratioFile<-paste(ticker, "ratios", period, sep="-")
  # read the files
  bsObj<-read_balance_sheet(file.path(srcDir, bsFile), src=src)
  cfObj<-read_cash_flow(file.path(srcDir, cfFile), src=src)
  isObj<-read_income_statement(file.path(srcDir, isFile), src=src)
  ratioObj<-read_financial_ratios(file.path(srcDir, ratioFile), src=src)
  if(!all(is.matrix(bsObj), is.matrix(cfObj), is.matrix(isObj), is.matrix(ratioObj))) {
    return(NA)
  }
  # combine all data into one, including raios
  if(!( ncol(bsObj)==ncol(cfObj)
            && ncol(bsObj)==ncol(isObj)
			&& ncol(bsObj)==ncol(ratioObj)))
  {
	message(glue::glue("The input dates for '{ticker}' differ: bs {ncol(bsObj)}, cf {ncol(cfObj)}, is {ncol(isObj)}, ratio {ncol(ratioObj)}"))
  }
  tmpd<-merge.FinData(isObj, bsObj)
  tmpd<-merge.FinData(tmpd, cfObj)
  mat<-merge.FinData(tmpd, ratioObj)
  # further process duplicate rows, such as fcfps, caused by
  # different coding for negative values
  rows_to_remove<-function(mat, varName) {
    indice<-which(rownames(mat) == varName)
    # get the NAs in each row given by index
    NAcnt<-apply(mat[indice,,drop=F], 1, function(x) sum(is.na(x)))
    selected<-which.min(NAcnt)
    return(indice[-selected])
  }
  dupVars<-rownames(mat)[duplicated(rownames(mat))]
  if(length(dupVars) > 0) {
    rowIdx<-lapply(dupVars, rows_to_remove, mat=mat)
    rowIdx<-do.call(c, rowIdx)
    mat<-mat[-rowIdx,]
  }
  # check whether recent data have NA revenue, if so, remove them
  firstColIndex<-which.max(!is.na(mat["revenue", ]))
  if(firstColIndex > 1) {
	mat<-mat[, firstColIndex:ncol(mat)]
  }
  # construct the S3 object with classes and attrs
  structure(mat,
            class=c(class(mat), "FinData"),
            period=period
          )
}

#' Merge FinData
#' 
#' @param a,b Two FinData objects.
#' @param recentFirst logical, if TRUE (default), the most recent data will
#'    be listed first.
#'
merge.FinData<-function(x,y,mergeType=c("union","inter"), recentFirst=T, ...) {
	mergeType<-match.arg(mergeType)
	if(mergeType != "union") {
		stop(glue::glue("The mergeType '{mergeType}' for 'FinData' has not implemented"))
	}
	# merge the two datesets first
	suppressWarnings(dat<-base::merge(t(x),t(y),by=0,all=T))
	## remove suffixes for duplicate columns
	colnames(dat)<-sub("\\.[xy]$","", colnames(dat))
	rownames(dat)<-dat[["Row.names"]]
	dat[["Row.names"]]<-NULL
	# get the union of dates
	allDates<-sort(union(colnames(x), colnames(y)), decreasing=recentFirst)
	dat<-dat[allDates,] # order the data
	dat<-t(as.matrix(dat)) # convert to matrix and transpose
	return(dat)
}

#' Read excel file downloaded from stockAnalysis
#'
#' Read the data downloaded from stockanalysis.com and format them
#' into standard format with standard vocabulary.
#'
#' @keywords internal
#'
read_sa_file<-function(f) {
  ext<-tools::file_ext(f)
  if(ext == "") # no extension
  { f<-paste0(f, ".xlsx") }
  if(!file.exists(f)) {
    message(sprintf("File '%s' doesn't exist", f))
    return(NA)
  }
  requiredPkgs<-c("readxl", "data.table")
  for(pkg in requiredPkgs) {
    if(!requireNamespace(pkg, quietly = T)) {
      stop(sprintf("Package '%s' is required", pkg))
    }
  }

  suppressMessages(dat<-readxl::read_excel(f, na="-"))
  varNames<-dat[,1,drop=T]
  dat<-as.matrix(dat[,3:ncol(dat)])
  # standardize the variable names
  varNames<-standardize_var_names(varNames)
  rownames(dat)<-varNames
  # sort data columns with latest first
  if(grepl("\\.", colnames(dat)[1])) { # quarterly data
    # convert column names to dates
	## Excel uses 1900-01-01 as day 1 (Windows default) or
	## 1904-01-01 as day 0 (Mac default), so use "1899-12-30"
	## and "1904-01-01" as origins, respectively. See more
	## at  http://support.microsoft.com/kb/214330
    colNames<-as.Date(
        as.numeric(colnames(dat)),
        origin = '1899-12-31')
    colnames(dat)<-format(colNames, "%Y-%m-%d")
    dat<-dat[, rev(order(colNames))]
  }else { # yearly data
    dat<-dat[,order(-as.numeric(colnames(dat)))]
  }
  return(dat)
}

#' Standardize the variable names
#'
#' @param varNames A vector of variable names
#'
#' @return standardized variable names
#'
#' @keywords internal
standardize_var_names<-function(varNames) {
  nameDict<-get_term_dict()
  # start conversion
  varNames<-tolower(varNames)
  unknowns<-setdiff(varNames, rownames(nameDict))
  if(length(unknowns) > 0) {
    message("The following variable names are unknown: ",
            paste(unknowns,collapse = " , "))
  }
  updated<-nameDict[varNames, "standardName"]
  return(updated)
}

#' Read balance sheet statement
#'
#' @param src the source of the data, determining how
#'   the file is parsed.
#'
#' @return An object of 'BsData'
#'
read_balance_sheet<-function(f, src=c("sa","mt")) {
  src<-match.arg(src)
  reader<-switch (src,
    sa = read_sa_file,
  )
  if(is.null(reader)) {
    stop(src, " is not implemented")
  }
  reader(f)
}

#' Read cash flow statement
#'
#' @param src the source of the data, determining how
#'   the file is parsed.
#'
#' @return An object of 'cfData'
#'
read_cash_flow<-function(f, src=c("sa","mt")) {
  src<-match.arg(src)
  reader<-switch (src,
                  sa = read_sa_file,
				  stop(glue::glue("Unsupported data source: {src}"))
  )
  if(is.null(reader)) {
    stop(src, " is not implemented")
  }
  reader(f)
}

#' Read income statement
#'
#' @param src the source of the data, determining how
#'   the file is parsed.
#'
#' @return An object of 'IsData'
#'
read_income_statement<-function(f, src=c("sa","mt")) {
  src<-match.arg(src)
  reader<-switch (src,
                  sa = read_sa_file,
  )
  if(is.null(reader)) {
    stop(src, " is not implemented")
  }
  reader(f)
}

#' Read financial ratios
#'


read_financial_ratios<-function(f, src=c("sa","mt")) {
  src<-match.arg(src)
  reader<-switch (src,
                  sa = read_sa_file,
  )
  if(is.null(reader)) {
    stop(src, " is not implemented")
  }
  reader(f)
}

#' Get stocks' price
#'
#' This function reports stock prices in a given period.
#'
#' @param to The last day to report. Default is today.
#' @param from The fist day to report. This parameter has higher priority than
#'  `span` (see below).
#' @param span An integer to provide the days, months, or years before the date
#'	given by `to`. If both `from` and `span` are `NA`s, then
#'	realtime current stock quote will be returned. 
#' @param units Time unit for the value provided by `span`.
#' 
#' @return A data.frame for realtime data (rows are tickers), and a list for
#'  historical data.
#' @export
get_stock_price<-function(tickers, to=lubridate::today(), from=NA, span=NA, units=c("days","months", "years"), baseCurrency=NULL, tz="America/Los_Angeles", ...) {
	options("getSymbols.warning4.0"=FALSE)
	tickers<-toupper(tickers)
	if(is.na(span) && is.na(from)) { # real time data
		quoteInfo<-quantmod::yahooQF(c("Last Trade (Price Only)", 
							"Change in Percent", 
							"Days High",
							"Days Low",
							"Volume",
							"Percent Change From 50-day Moving Average", 
							"Percent Change From 200-day Moving Average",
							"Dividend Yield",
							"Price/Book",
							"Earnings/Share",
							"EPS Forward",
							"P/E Ratio",
							"Market Capitalization",
							"Currency"))
		prices<-quantmod::getQuote(tickers, what=quoteInfo)
		# change header names to make them shorter
		headers<-colnames(prices)
		headers[which(headers=="% Change From 200-day MA")]<-"% Change vs 200DMA"
		headers[which(headers=="% Change From 50-day MA")]<-"% Change vs 50DMA"
		headers[which(headers=="Dividend Yield")]<-"Div. Yield"
		headers[which(headers=="Price/Book")]<-"P/B"
		headers[which(headers=="Earnings/Share")]<-"EPS"
		headers[which(headers=="P/E Ratio")]<-"P/E"
		headers[which(headers=="Market Capitalization")]<-"Market Cap(M)"
		colnames(prices)<-headers
		# format the data
		prices[["% Change vs 200DMA"]]<-signif(prices[["% Change vs 200DMA"]]*100,3)
		prices[["% Change vs 50DMA"]]<-signif(prices[["% Change vs 50DMA"]]*100,3)
		prices[["Div. Yield"]]<-signif(prices[["Div. Yield"]]*100,3)
		prices[["P/B"]]<-signif(prices[["P/B"]],3)
		prices[["P/E"]]<-signif(prices[["P/E"]],3)
		prices[["% Change"]]<-signif(prices[["% Change"]],3)
		prices[["Market Cap(M)"]]<-signif(prices[["Market Cap(M)"]]/10^6,3)
		# convert values to base currency if provided
		if(!is.null(baseCurrency)) {
			message("Currency conversion hasn't implemented")
		}
        # change timezone to specified one
        prices[["Trade Time"]]<-as.POSIXct(format(prices[["Trade Time"]], tz=tz,usetz=TRUE))
		return(prices)
		# handle tikers without data, but this version is too slow
		# prices<-lapply(tickers, function(x) {
		# 					price<-tryCatch(suppressWarnings(
		# 							quantmod::getQuote(x, what=quoteInfo)
		# 							),
		# 						error=function(e) { warning(glue::glue("Ticker '{x}' has no price data")); return(NULL) }
		# 						)
		# 					return(price)
		# 					}
		# 	)
		# # combine the list into one data.frame
		# return(do.call(rbind, prices))
	}
	# otherwise historical data
	to<-as.Date(to)
	if(is.na(from)) {
		units<-match.arg(units)
		durationMethod<-switch( units,
			days = lubridate::days,
			months = lubridate::months,
			years = lubridate::years,
			stop(glue::glue("Unknown time units: {units}"))
			)
		from<-lubridate::add_with_rollback(to, -durationMethod(span))
	} else {
		from<-as.Date(from)
	}
	prices<-lapply(tickers, function(x) {
							price<-tryCatch(suppressWarnings(
									quantmod::getSymbols(x, from=from, to=to+1, auto.assign=FALSE, ...)
									),
								error=function(e) { warning(glue::glue("Ticker '{x}' has no historical data")); return(NULL) }
								)
							if(!is.null(price)) { # ticker from column headers
								colnames(price)<-sub(paste("^",x,"\\.",sep=""),"", colnames(price))
							}
							return(price)
							}
				   )
	names(prices)<-tickers
	return(prices)
}

#' Get current exchange ratio for currencies
#'
#' This function returns the exchange ratio between `fromCurrency` and
#' `toCurrency`. For example, if fromCurrency="HKD" and toCurrency="USD",
#' a returned value would be kind of 0.128
#'
#' @param fromCurrency character vector. The currency to convert from, can accept multiple values
#' @param toCurrency character vector. The currency to convert to, can accept multiple values.
#'
#' @details
#' When one of `fromCurrency` and `toCurrency` has multiple values, but the other
#' has only one, then the length-one value will be replicated to match the length
#' of the longer one.
#'
#' @return A vector with 
get_fx<-function(fromCurrency, toCurrency) {
	stopifnot(!any(is.na(fromCurrency), is.na(toCurrency))) # NA values are not accepted
	if(length(fromCurrency) == 1) {
		fromCurrency<-rep(fromCurrency, length(toCurrency))
	} else if(length(toCurrency) == 1) {
		toCurrency<-rep(toCurrency, length(fromCurrency))
	}
	stopifnot(length(fromCurrency) == length(toCurrency))
	ticker<-paste0(fromCurrency, toCurrency, "=X")
	ticker<-toupper(ticker)
	fx<-quantmod::getQuote(ticker)[,'Last']
	names(fx)<-toupper(paste(fromCurrency, toCurrency, sep="."))
	return(fx)
}

#' Read file
#'
#' Read matrix-like files in different formats
read_file<-function(f, fileExt=c("auto","csv","tsv","xlsx"), commentChar="#", ...) {
	fileExt<-match.arg(fileExt)
	if(fileExt == "auto") {
		fileExt <- tools::file_ext(f)
		if(fileExt == "lnk") { # link file
			fileExt <- tools::file_ext(tools::file_path_sans_ext(f))
		}
	}
	dat<-switch(fileExt,
		csv = read.csv(f, stringsAsFactors=F, comment.char=commentChar, ...),
		tsv = read.delim(f, stringsAsFactors=F, comment.char=commentChar, ...),
		xlsx = readxl::read_excel(f, ...),
		stop(glue::glue("Unknown file format for {f}"))
	)
	return(dat)
}


#' Read bank portfolio
#' 
#' Read the portofolio files downloaded from each bank
#' and return a data.frame with standard columns.
#'
read_bank_portfolio<-function(f, accountName=NULL, bank=c("chase","cs","ib"), commentChar="#") {
    if(is.null(accountName)) {
        accountName<-tools::file_path_sans_ext(basename(f))
    }
    dat<-read_file(f, commentChar=commentChar, check.names=F)
    bank<-match.arg(bank)
    res<-switch(bank,
        chase = zz_process_chase_portfolio(dat),
           cs = zz_process_cs_portfolio(dat),
           ib = zz_process_ib_portfolio(dat),
        stop(glue::glue("Unknown bank: {bank}"))
    )
    # change the cash basis to itself
    cashRows<-which(res$Symbol %in% c("HKD","USD", "CNY"))
    res$CostBasis[cashRows]<-res$Quantity[cashRows]
    res<-cbind(Account=accountName, res)
    return(res)
}

#' Process portfolio from Interactive Broker
#' @keywords internal
zz_process_ib_portfolio<-function(dat) {
    # filter rows first
    dat<-subset(dat, Symbol != "")
    # rename columns
    oldNames<-c("Cost Basis", "FinancialInstrument")
    newNames<-c("CostBasis", "Type")
    colnames(dat)[match(oldNames, colnames(dat))]<-newNames
    selectedCols<-c("Symbol", "Type", "Quantity", "CostBasis", "Currency", "Date")
    stopifnot(all(selectedCols %in% colnames(dat)))
    dat<-dat[,selectedCols]
    # also get the underlying stock for options
    dat$UnderTicker<-dat$Symbol
    dat$OptionType<-NA
    optionRows<-which(dat$Type=="Options")
    if(length(optionRows) > 0) {
        optionSet<-strsplit(dat$Symbol[optionRows], "\\s+")
        tmpTickers<-sapply(optionSet,  `[`, 1)
        tmpOption<-sapply(optionSet,  `[`, 2)
        optionType<-ifelse(grepl("C", tmpOption), "C", ifelse(grepl("P", tmpOption), "P", "U" ))
        dat$UnderTicker[optionRows]<-tmpTickers
        dat$OptionType[optionRows]<-optionType
    }
    return(dat)
}

#' Process portfolio from Charles Schwab
#' @keywords internal
zz_process_cs_portfolio<-function(dat) {
    # rename columns
    oldNames<-c("Cost Basis", "Security Type")
    newNames<-c("CostBasis", "Type")
    colnames(dat)[match(oldNames, colnames(dat))]<-newNames
    # filter rows
    dat<-subset(dat, Type != "" & !is.na(Type))
    # map security type to standard names
    dat$Type<-sapply(dat$Type, function(x) switch(x, "Equity" = "Stocks", Option="Options", "Mutual Fund"="MutualFunds", "Cash and Money Market"="Cash", "Unknown"))
    # format the data for Cash
    dat$Symbol[dat$Type=="Cash"]<-"USD"
    dat$CostBasis[dat$Symbol=="USD"]<-dat[,"Market Value"][dat$Symbol=="USD"]
    dat$CostBasis<-as.numeric(gsub("[,$]", "", dat$CostBasis))
    dat$Quantity[dat$Symbol=="USD"]<-dat$CostBasis[dat$Symbol=="USD"]
    dat$Currency<-"USD" # assume USD only, need update in future if other currencies are there
    # prepare data now
    dat$Date<-format(Sys.Date(), "%m/%d/%Y") # add today's date
    selectedCols<-c("Symbol", "Type", "Quantity", "CostBasis", "Currency", "Date")
    stopifnot(all(selectedCols %in% colnames(dat)))
    dat<-dat[,selectedCols]
    # also get the underlying stock for options
    dat$UnderTicker<-dat$Symbol
    dat$OptionType<-NA
    optionRows<-which(dat$Type=="Options")
    if(length(optionRows) > 0) {
        optionSet<-strsplit(dat$Symbol[optionRows], "\\s+")
        tmpTickers<-sapply(optionSet,  `[`, 1)
        optionType<-sapply(optionSet,  `[`, 4)
        dat$UnderTicker[optionRows]<-tmpTickers
        dat$OptionType[optionRows]<-optionType
    }
    return(dat)
}


#' Process portfolio from JP Morgan Chase
#' @keywords internal
zz_process_chase_portfolio<-function(dat) {
    # filter rows
    dat<-subset(dat, dat[,"As of"] != "")
    # rename columns
    oldNames<-c("Ticker", "Base CCY", "Cost", "As of")
    newNames<-c("Symbol", "Currency", "CostBasis", "Date")
    colnames(dat)[match(oldNames, colnames(dat))]<-newNames
    # get security type and change cash symbols
    dat$Type<-sapply(dat$Symbol, function(x) {
                        switch(x,
                            QACDS="Cash",
                            QDERQ="Cash",
                            ifelse(grepl(" (PUT|CALL) ",x), "Options", "Stocks")
                        )
                    })
    dat$Symbol[dat$Type=="Cash"]<-dat$Currency[dat$Type=="Cash"]
    # prepare data
    selectedCols<-c("Symbol", "Type", "Quantity", "CostBasis", "Currency", "Date")
    stopifnot(all(selectedCols %in% colnames(dat)))
    dat<-dat[,selectedCols]
    # also get the underlying stock for options
    dat$UnderTicker<-dat$Symbol
    dat$OptionType<-NA
    optionRows<-which(dat$Type=="Options")
    if(length(optionRows) > 0) {
        optionSet<-strsplit(dat$Symbol[optionRows], "\\s+")
        tmpTickers<-sapply(optionSet,  `[`, 1)
        optionType<-substr(sapply(optionSet,  `[`, 2), 1,1)
        dat$UnderTicker[optionRows]<-tmpTickers
        dat$OptionType[optionRows]<-optionType
    }
    return(dat)

}

