# functions to calculate and model data

#' Calculate growth rates for selected variables
#'
#' This function calculates the growth rates for
#' the selected variables by `vars` in the given
#' periods by `periods`.
#'
#' @param dat A object holds financial information, including
#'   balance sheet, income statement, and cash flow.
#' @param vars Selected variables
#' @param periods Considered periods in years.
#' @param ... other arguments to [calc_growth_rate()]
#'
#' @return A list with each element corresponding to one variable
#'
calc_growth_rates<-function(dat,
                            vars=c("revenue","epsDiluted", "freeCashFlow"),
                            periods=c(10,5,3,1),
                            ...
) {
  x<-as.numeric(colnames(dat))
  rates<-sapply(vars,
                function(v) {
                  if(! v %in% rownames(dat)) {
                    message(sprintf("Variable '%s' is not found in input data", v))
                    return(rep(NA, length(periods)))
                  }
                  y<-dat[v,]
                  calc_growth_rate(x=x, y=y, periods=periods, ...)
                })
  # convert it into matrix if not
  if(!is.matrix(rates)) {
    rates<-matrix(rates, ncol = length(vars))
    colnames(rates)<-vars
    rownames(rates)<-periods
  }
  return(rates)
}

calc_growth_rate<-function(x,y,periods, model=c("exp", "linear")) {
  model<-match.arg(model)
  df<-data.frame(x,y)
  df<-df[order(df$x),] # order as time increases
  n<-nrow(df)
  rates<-sapply(periods,
                function(p) {
                  if(p >= n) { return(NA) }
                  dat<-df[(n-p):n,]
                  dat<-dat[!is.na(dat$y),] # remove NAs
                  #print(dat$y)
                  rate<-robust_lm(dat, model)
                  return(rate)
                }
  )
  names(rates)<-periods
  return(rates)
}

robust_lm<-function(dat, model) {
  stopifnot(model %in% c("exp", "linear"))
  if(sum(!is.na(dat$y)) < 2) { # no enough number of datapoints
    return(NA)
  }
  if(model == "exp") {
    if(any(dat$y < 0)) { return(NA)}
    dat$y<-dat$y/dat$y[1] # normalize
    dat$y<-log(dat$y) # transform
    dat$x<-dat$x - dat$x[1] # starting time point at 0
    fit<-MASS::rlm(y~0+x, data=dat)
    k<-fit$coefficients
    rate<-exp(k)-1
    return(rate)
  } else { # linear
    fit<-MASS::rlm(y~x, data=dat)
    k<-fit$coefficients
    return(k["x"])
  }
}

#' Discounted Cash Flow model
#'
#' @param profits EPS, FCF, etc, the profits generated each year,
#'   if the length is 1, it will be treated as initial value, and
#'   the values for the following years are computed based on growth
#'   rate(s).
#' @param n The time span (in years) to consider. This is overriden if
#'   the length of `profits` is greater than 1, in which case the number
#'   of years considered is the length of `profits`.
#' @param g Growth rate. It can be a vector with more than one value,
#'   in which case, each of the rates is considered for each time period
#'   by equally dividing the total time span.
#' @param r Discount rate
#' @param payout The fraction of profits paid out as dividend.
#' @param twoStage Logical, if true, the growth rate will cut to
#'   half for the second half of the considered time span. It is
#'   ignored if `g` has more than one value.
#' @param terminalModel Model to compute terminal values. 'multiple'
#'   for simply applying a multiple to the value, 'perpetual' for
#'   perpetual growth rate model.
#' @param multiple The multiple used for computing terminal values if
#'   the model is `multiple`.
#' @param terminalG The growth rate used for computing terminal
#'   values if the model is `perpetual`.
#'
dcf<-function(profits, n=5, g=0.05, r=0.1,
              payout=0,
              twoStage=F,
              terminalModel=c("multiple","perpetual"),
              multiple=10, terminalG=0.05) {
  # compute the accumulated profits.
  if(length(profits) > 1) {
    n<-length(profits)
  }else { # compute profits based on growth
    # get the growth rates for each year
    if(length(g)==1 && twoStage) {
      g<-c(g, g*0.5) # cut half for second half time span
    }
    if(length(g)>1) {
      spans<-divide_span(n, length(g))
      g<-rep(g, times=spans)
      growths<-cumprod(1+g)
    } else {
      growths<-(1+g)^seq_len(n)
    }
    profits<-profits*growths
  }
  discountedProfits<-profits/((1+r)^seq_len(n))
  payouts<-discountedProfits*payout
  terminalProfit<-discountedProfits[n]
  terminalModel<-match.arg(terminalModel)
  terminalValue<-switch(terminalModel,
         multiple = terminalProfit*multiple,
         perpetual = ifelse(r > terminalG,
           terminalProfit*(1+terminalG)/(r-terminalG),
           stop("'Perpetual terminal model' is invalid for r <= g")
          )
         )
  return(sum(payouts) + terminalValue)
}

divide_span<-function(size, n) {
  baseVal<-floor(size/n)
  mod<-size %% n
  up<-T
  spans<-rep(baseVal,n)
  # spread the mod to the spans
  for(i in seq_len(n)) {
    if(mod > 0) {
      if(up) {
        spans[i]<-spans[i]+1
        mod<-mod-1
      }
      up<-!up
    } else {
      break
    }
  }
  return(spans)
}

#' Calculate the intrinsic value of a stock
#'
#' Calculate the intrinsic value and some other metrics
#' of a stock.
#'
#' @inheritParams read_financials
#' @param span The number of years for consideration.
#' @param returnDat Logical. If true, the read data is returned
#'   as attribute 'dat'.
#'
get_stock_value<-function(ticker, src="sa",
                          srcDir=NULL,
                          span=10,
                          returnDat=F) {
  datYear<-read_financials(ticker, src=src, period = "annual", srcDir=srcDir)
  if(!is(datYear, "FinData")) { return(NA) }
  datQuarter<-read_financials(ticker, src=src, period = "quarterly", srcDir=srcDir)
  latestYear<-colnames(datYear)[1]
  latestQuarter<-colnames(datQuarter)[1]
  # Get the growth rates for dcf models
  dcfVars<-c("revenue","epsDiluted")
  dcfRates<-calc_growth_rates(datYear, vars = dcfVars)
  ratiosYear<-attr(datYear, "ratio")
  if("payoutRatio" %in% rownames(ratiosYear)) {
    # use recent 10 years mean payout ratio
    payout<-mean(ratiosYear["payoutRatio",1:min(10, ncol(ratiosYear))], trim=0.1, na.rm=T)
  } else {
    payout<-0
  }
  # get the mean PE ratio in last 10 years
  pe<-mean(ratiosYear["pe",1:min(10, ncol(ratiosYear))], trim=0.1, na.rm=T)
  multiple<-min(10, pe, na.rm=T)
  # message(rates, payout)
  curEPS<-datYear["epsDiluted",1]
  est_vals<-function(rates) {
    if(is.na(curEPS) || curEPS < 0) {
      noVals<-sapply(rates, function(x) NA)
      return(noVals)
    }
    estVals<-sapply(rates, function(x) ifelse(is.na(x), NA,
                                              dcf(
                    curEPS,
                    g=x,
                    n=span,
                    twoStage = T,
                    payout = payout,
                    multiple = multiple)
                  )
              )
    return(estVals)
  }
  estVals<-sapply(seq_len(ncol(dcfRates)),
                  function(i) est_vals(dcfRates[,i]))
  colnames(estVals)<-paste("estPrice", colnames(dcfRates), sep=".")
  # combine all results together
  colnames(dcfRates)<-paste("rate", colnames(dcfRates), sep=".")
  estVals<-c(eps=curEPS, payout=payout,
             year=latestYear, quarter=latestQuarter,
         matrix_to_vec(dcfRates),
         matrix_to_vec(estVals))
  # let's also compute health-related index
  healthIndex<-calc_health_indice(datYear, datQuarter)
  estVals<-c(healthIndex, estVals)
  class(estVals)<-c(class(estVals), "StockValue")
  if(returnDat) {
    attr(estVals, "dat") <-list(datYear, datQuarter)
  }
  return(estVals)
}

#' Get intrinsic values for stocks
#'
#' @inheritParams get_stock_value
get_stocks_values<-function(tickers, ...) {
  if(is.matrix(tickers) || is.data.frame(tickers)) {
    tickers<-tickers[,1]
  }
  stockValues<-lapply(tickers, function(x) {
            message(sprintf("== Analyzing '%s' ==", x));
            get_stock_value(x, ...) }
          )
  # combine
  #tmp1<-lapply(stockValues,
  #             function(x) if(is(x, "StockValue")) { to_string.StockValue(x) } else { NA } )
  names(stockValues)<-tickers
  do.call(rbind, stockValues)->stockValues
  return(stockValues)
}


#' Calculate health index
#'
#' Calculate the metrics indicating the health of a company
#'
#' @param datYear `FinData` storing annual values
#' @param datQuarter `FinData` storing quarterly values, `NA`
#'   if no data available.
#' @return A vector with names

calc_health_indice<-function(datYear, datQuarter) {
  varNamesForRatiosQuarter<-c("totalDebt",
                       "shareholderEquity",
                       "shareCountDiluted",
                       "netCashDebt")
  varNamesForRatiosYear<-c("ebitda",
                           "freeCashFlow",
                           "revenue")
  varNamesForRates<-c("profitMargin",
                      "totalDebt",
                      "shareCountDiluted")
  # get latest values
  values1<-summarize_dat(datQuarter, varNames=varNamesForRatiosQuarter, period=1, method="asis")
  values2<-summarize_dat(datYear, varNames=varNamesForRatiosYear, period=1, method="asis")
  values<-c(values1[1,], values2[1,])
  names(values)<-c(varNamesForRatiosQuarter, varNamesForRatiosYear)
  debtToEquity<-values["totalDebt"]/values["shareholderEquity"]
  debtToFcf<-values["totalDebt"]/values["freeCashFlow"]
  debtToEbitda<-values["totalDebt"]/values["ebitda"]
  res<-c(values["revenue"], values["shareCountDiluted"],
         values["netCashDebt"],
         debtToFcf=debtToFcf,
         debtToEbitda=debtToEbitda,
         debtToEquity=debtToEquity)
  # also compute the average of the last 5 years, use yearly data only
  values1<-summarize_dat(datYear, varNames=varNamesForRatiosQuarter, period=5, method="asis")
  values2<-summarize_dat(datYear, varNames=varNamesForRatiosYear, period=5, method="asis")
  if(nrow(values1) != nrow(values2)) { # caused by missing datQuarter
    missNrows<-nrow(values2)-nrow(values1)
    tmpm<-matrix(NA, nr=missNrows, ncol=ncol(values1))
    values1<-rbind(values1, tmpm)
  }
  values<-cbind(values1, values2)
  debtToEquity<-mean(values[,"totalDebt"]/values[,"shareholderEquity"], na.rm=T)
  debtToFcf   <-mean(values[,"totalDebt"]/values[,"freeCashFlow"], na.rm=T)
  debtToEbitda<-mean(values[,"totalDebt"]/values[,"ebitda"], na.rm=T)
  res<-c(res,
         debtToFcf.avg=debtToFcf,
         debtToEbitda.avg=debtToEbitda,
         debtToEquity.avg=debtToEquity)
  rates<-calc_growth_rates(datYear, vars = varNamesForRates,
                           periods = 5, model="linear")
  res<-c(res, matrix_to_vec(rates))
  names(res)<-sub("\\.totalDebt$", "", names(res))
  return(res)
}

#' Summarize data according specified method
#'
#' @param ... Other arguments to the `method`
#'
#' @return A matrix with columns corresponding `varNames`.
#'
summarize_dat<-function(dat, varNames, period,
                       method="asis", ...) {
  func<-switch (method,
    asis = function(x) x,
    method
  )
  if(!is.function(func)) {
    stop("A function or 'asis' should be provided to the argument `method` of the function summarize_dat()")
  }
  if(is.null(dim(dat))) { # no data available
    mat<-matrix(rep(NA, length(varNames)), ncol=length(varNames))
    colnames(mat)<-varNames
    return(mat)
  }
  endCol<-min(ncol(dat), period)
  recordSize<-0 # the vector length returned by 'func()'
  res<-lapply(varNames, function(x){
    if(x %in% rownames(dat)) {
      record<-func(dat[x,1:endCol], ...)
      recordSize<-length(record)
      return(record)
    } else {
      message(sprintf("Variable '%s' isn't in dat", x))
      return(NA)
    }
  })
  # fill NAs
  if(recordSize > 0) {
    res<-lapply(res, function(x) if(is.na(x)) {rep(NA, recordSize)} else {x} )
  }
  mat<-do.call(cbind, res)
  colnames(mat)<-varNames
  return(mat)
}

