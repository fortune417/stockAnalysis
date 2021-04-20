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
    "Net Income Common", "netIncome"
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
#'
#' @return an object of FinData
#' @export
read_financials<-function(ticker,
                          src=c("sa","mt"),
                          period=c("annual","quarterly"),
                          srcDir=NULL) {
  if(is.null(srcDir)) {
    src<-match.arg(src)
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
  # combine all data into one
  stopifnot(ncol(bsObj)==ncol(cfObj)
            && ncol(bsObj)==ncol(isObj))
  mat<-rbind(bsObj, cfObj, isObj)
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
  structure(mat,
            class=c(class(mat), "FinData"),
            period=period, ratio=ratioObj
          )
}

#' Read excel file downloaded from stockAnalysis
#'
#' @keywords internal
#'
read_sa_file<-function(f) {
  ext<-tools::file_ext(f)
  if(ext == "") # no extension
  { f<-paste0(f, ".xlsx") }
  stopifnot(file.exists(f))
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
  dat<-dat[,order(-as.numeric(colnames(dat)))]
  return(dat)
}

#' Standardize the variable names
#'
#' @param varNames A vector of variable names
#'
#' @return updated variable names
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
