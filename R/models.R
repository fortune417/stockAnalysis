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
  x<-as.numeric(colnames(dat)) # year
  rates<-sapply(vars,
                function(v) {
                  if(! v %in% rownames(dat)) {
                    message(sprintf("Variable '%s' is not found in input data", v))
                    return(rep(NA, length(periods)))
                  }
                  y<-dat[v,] # value
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

robust_lm<-function(dat, model, returnFit=F) {
  stopifnot(model %in% c("exp", "linear"))
  if(sum(!is.na(dat$y)) < 2) { # no enough number of datapoints
    return(NA)
  }
  if(model == "exp") {
	# set negative values to NA
	dat$y[dat$y<=0]<-NA
    if(sum(!is.na(dat$y)) < 2) { return(NA)}
	# find first nonNA index, and subset the data to start from that index
	firstIndex<-which.max(!is.na(dat$y))
	dat<-dat[firstIndex:nrow(dat),]
    dat$y<-dat$y/dat$y[1] # normalize
    dat$y<-log(dat$y) # transform
    dat$x<-dat$x - dat$x[1] # starting time point at 0
    fit<-MASS::rlm(y~0+x, data=dat)
    k<-fit$coefficients
    rate<-exp(k)-1
  } else { # linear
    fit<-MASS::rlm(y~x, data=dat)
    k<-fit$coefficients
	rate<-k["x"]
  }
  if(returnFit) { attr(rate, "fit") <- fit }
  return(rate)
}

#' Discounted Cash Flow model
#'
#' @param profits EPS, FCF, etc, the profits generated each year,
#'   if the length is 1, it will be treated as initial value, and
#'   the values for the following years are computed based on growth
#'   rate(s). It can also be sales per share and the paramter `usePS`
#'   should be TRUE; in this case, the terminal sales per share will
#'   be computed and stock price will be computed as 
#'   terminalSalesPerShare*multiple (assuming multiple is P/S ratio).
#' @param usePS logical. If TRUE, the paramter `profits` refers to
#'	 sales per share and multiple is P/S ratio. Default is FALSE.
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
dcf<-function(profits, usePS=FALSE,
				n=5, g=0.05, r=0.1,
              payout=0,
              twoStage=F,
              terminalModel=c("multiple","perpetual"),
              multiple=10,
			  terminalG=0.05) {
  # compute the accumulated profits.
  if(length(profits) > 1) {
    n<-length(profits)
	#message(glue::glue("The number of periods for dcf is derived for parameter profits"))
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
  if(usePS) {
	terminalSales<-profits[n]
	terminalPrice<-terminalSales*multiple
	dcfValue<-terminalPrice/((1+r)^n) # discount value using desired discount rate
  } else {
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
	dcfValue<-sum(payouts) + terminalValue
  }
  return(dcfValue)
}

dcf0<-function(profits, n=5, g=0.05, r=0.1,
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
#' of a stock using both annual and quarter data from the
#' specified source.
#'
#' @inheritParams read_financials
#' @param span The number of years for consideration.
#' @param userPE User specified min and mean PE ratios for value
#'	calculation, instead of those computed from history data.
#'	Default are NA's, so historical values are used.
#' @param userPS User specified min and mean PS ratios for value
#'	calculations, instead of the one computed from historical data.
#'	Useful when PE ratios are negative.
#' @param returnDat Logical. If true, the read data is returned
#'   as attribute 'dat'.
#'
get_stock_value<-function(ticker, src="sa",
                          srcDir=NULL,
                          span=10,
						  userPE=c(NA,NA),
						  userPS=c(NA,NA),
                          returnDat=F) {
  # read data, both yearly and quarterly
  datYear<-read_financials(ticker, src=src, period = "annual", srcDir=srcDir)
  if(!is(datYear, "FinData")) { return(NA) }
  datQuarter<-read_financials(ticker, src=src, period = "quarterly", srcDir=srcDir)
  latestYear<-colnames(datYear)[1]
  latestQuarter<-ifelse(is.matrix(datQuarter), colnames(datQuarter)[1], NA)
  oldestQuarter<-ifelse(is.matrix(datQuarter), colnames(datQuarter)[ncol(datQuarter)], NA)
  # Get the growth rates for dcf models, at present, use both revenue and eps
  dcfVars<-c("revenue","epsDiluted")
  dcfRates<-calc_growth_rates(datYear, vars = dcfVars, model="exp") # using annual data to compute rates
  # ratiosYear<-attr(datYear, "ratio") # not needed any more, as it has been merged into main data
  if("payoutRatio" %in% rownames(datYear)) {
    # use recent 10 years mean payout ratio
    payout<-mean(datYear["payoutRatio",1:min(10, ncol(datYear))], trim=0.1, na.rm=T)
  } else {
    payout<-0
  }
  # get the PE ratio summary
  PEsummary<-summarize_metrics(datYear, "pe", removeNegative=T)
  minPE<-min(10, PEsummary$pe["min"], na.rm=T) # use mean over full time period
  meanPE<-PEsummary$pe["mean"]
  # message(rates, payout)
  curEPS<-datYear["epsDiluted",1]
  # get the PS ratio summary
  PSsummary<-summarize_metrics(datYear, "ps", removeNegative=T)
  minPS<-PSsummary$ps["min"]
  meanPS<-PSsummary$ps["mean"]
  curPS<-datYear["ps",1]
  shareCountVar<-ifelse("shareCountDiluted" %in% rownames(datYear), "shareCountDiluted", "shareCountBasic")
  curSPS<-ifelse(all(c('revenue', shareCountVar) %in% rownames(datYear)), datYear['revenue',1]/datYear[shareCountVar,1], NA)
  
  # we will compute DCF values for the following combinations:
  #               minPE, meanPE, minPS, meanPS
  # revenueRates   Y,       Y,     Y,     Y
  # EPSRates       Y,       Y,     N,     N
  #
  # define a function to calculate DCF values for a set of rates.
  # using both EPS and SPS (sales per share)
  est_vals<-function(rates, startValue, multiple, usePS=F) {
    if(is.na(startValue) || startValue < 0) {
      noVals<-sapply(rates, function(x) NA)
      return(noVals)
    }
    estVals<-sapply(rates, function(x) ifelse(is.na(x), 
										NA_real_,
                                        dcf(
											startValue,
											usePS=usePS,
											g=x,
											n=span,
											twoStage = T,
											payout = payout,
											multiple = multiple)
										)
              )
    return(estVals)
  }
  # minPE
  estVals_minPE<-sapply(seq_len(ncol(dcfRates)),
                  function(i) est_vals(dcfRates[,i], curEPS, multiple=minPE))
  colnames(estVals_minPE)<-paste("estPrice", colnames(dcfRates), "minPE", sep=".")
  # meanPE
  estVals_meanPE<-sapply(seq_len(ncol(dcfRates)),
                  function(i) est_vals(dcfRates[,i], curEPS, multiple=meanPE))
  colnames(estVals_meanPE)<-paste("estPrice", colnames(dcfRates), "meanPE", sep=".")
  # minPS
  estVals_minPS<-sapply(seq_len(ncol(dcfRates)),
                  function(i) est_vals(dcfRates[,i], curSPS, multiple=minPS, usePS=T))
  colnames(estVals_minPS)<-paste("estPrice", colnames(dcfRates), "minPS", sep=".")
  # meanPS
  estVals_meanPS<-sapply(seq_len(ncol(dcfRates)),
                  function(i) est_vals(dcfRates[,i], curSPS, multiple=meanPS, usePS=T))
  colnames(estVals_meanPS)<-paste("estPrice", colnames(dcfRates), "meanPS", sep=".")

  # combine all results together as a vector
  colnames(dcfRates)<-paste("rate", colnames(dcfRates), sep=".")
  PEsumStr<-PEsummary$pe
  names(PEsumStr)<-paste("pe",  names(PEsumStr), sep=".")
  PSsumStr<-PSsummary$ps
  names(PSsumStr)<-paste("ps",  names(PSsumStr), sep=".")
  estVals<-c(
			PEsumStr,
			PSsumStr,
			eps=curEPS, 
			sps=curSPS,
			payout=payout,
			year.last=latestYear,
			quarter.last=latestQuarter,
			quarter.first=oldestQuarter,
			matrix_to_vec(dcfRates),
			minPE = unname(minPE ),
			meanPE= unname(meanPE),
			minPS = unname(minPS ),
			meanPS= unname(meanPS),
			matrix_to_vec(estVals_minPE),
			matrix_to_vec(estVals_meanPE),
			matrix_to_vec(estVals_minPS),
			matrix_to_vec(estVals_meanPS)
			)
  # let's also compute health-related index
  healthIndex<-calc_health_indice(datYear, datQuarter)
  estVals<-c(healthIndex, estVals) # a vector
  class(estVals)<-c(class(estVals), "StockValue")
  if(returnDat) {
    attr(estVals, "dat") <-list(datYear, datQuarter)
  }
  return(estVals)
}

#' Summarize metrics
#'
#' Summarizing a metric from the given dataset.
summarize_metrics<-function(dat, metrics, removeNegative=T, simplify=F, ...) {
	summary_fun<-function(x) {
		NAres<-rep(NA_real_, 5)
		names(NAres)<-c("mean.10", "mean.5", "min", "max", "mean")
		if(x %in% rownames(dat)) { 
			v<-dat[x,] 
		} else { 
			return(NAres)
		}
		# calculate the 10 year and 5 year average first.
		# only valid if the number of valid values (no-NA) in each group is >50%
		v10<-v[1:min(length(v),10)]
		if(removeNegative) { v10<-v10[v10 > 0] }
		v5<-v[1:min(length(v),5)]
		if(removeNegative) { v5<-v5[v5 > 0] }
		## remove NA values
		v10<-v10[!is.na(v10)]
		v5<-v5[!is.na(v5)]
		mean.10<-ifelse(length(v10) > 5, mean(v10, trim=0.1), NA_real_)
		mean.5<-ifelse(length(v5) > 2, mean(v5, trim=0.1), NA_real_)
		# Then min, max, and all mean
		v<-v[!is.na(v)]
		if(removeNegative) {
			v<-v[v>0]
		}
		if(length(v) > 0) {
			res<-sapply(c(min,max,mean), function(z) z(v))
		} else {
			res<-rep(NA_real_, 3) 
		}
		res<-c(mean.10, mean.5, res)
		names(res)<-c("mean.10", "mean.5", "min", "max", "mean")
		return(res)
	}
	metricSummary<-lapply(metrics, summary_fun)
	names(metricSummary)<-metrics
	if(simplify) {
		metricSummary<-do.call(rbind, metricSummary)
	}
	return(metricSummary)
}

#' Get intrinsic values for multiple stocks
#'
#' This is a wrapper of [get_stock_value()] so that
#' 
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
  shareCountVar<-ifelse("shareCountDiluted" %in% rownames(datYear), "shareCountDiluted", "shareCountBasic")
  varNamesForRatiosQuarter<-c("totalDebt",
                       "shareholderEquity",
                       shareCountVar,
                       "netCashDebt")
  varNamesForRatiosYear<-c("ebitda",
                           "freeCashFlow",
                           "revenue",
						   "pb", "pfcf", "curRatio")
  # for linear
  varNamesForRates1<-c("profitMargin",
					   "ebitMargin")
  # for expontential
  varNamesForRates2<-c("freeCashFlow",
					   "totalDebt",
                      shareCountVar)
  # get latest values (some values only available in yearly data)
  values1<-summarize_dat(datQuarter, varNames=varNamesForRatiosQuarter, period=1, method="asis")
  values2<-summarize_dat(datYear, varNames=varNamesForRatiosYear, period=1, method="asis")
  values<-c(values1[1,], values2[1,])
  names(values)<-c(varNamesForRatiosQuarter, varNamesForRatiosYear)
  debtToEquity<-values["totalDebt"]/values["shareholderEquity"]
  debtToFcf<-values["totalDebt"]/values["freeCashFlow"]
  debtToEbitda<-values["totalDebt"]/values["ebitda"]
  curRatio<-ifelse("curRatio" %in% names(values), values["curRatio"], NA)
  res<-c(values["revenue"], values[shareCountVar],
         values["netCashDebt"],
         debtToFcf=debtToFcf,
         debtToEbitda=debtToEbitda,
         debtToEquity=debtToEquity,
		 curRatio=curRatio)
  names(res)[2]<-"shareCount"
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
  curRatio<-ifelse("curRatio" %in% colnames(values), mean(values[,"curRatio"], na.rm=T), NA)
  res<-c(res,
         debtToFcf.avg=debtToFcf,
         debtToEbitda.avg=debtToEbitda,
         debtToEquity.avg=debtToEquity,
		 curRatio.avg=curRatio
		 )
  rates1<-calc_growth_rates(datYear, vars = varNamesForRates1,
                           periods = 5, model="linear")
  rates2<-calc_growth_rates(datYear, vars = varNamesForRates2,
                           periods = 5, model="exp")
  res<-c(res, matrix_to_vec(rates1),matrix_to_vec(rates2))
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

