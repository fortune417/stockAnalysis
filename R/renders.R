# functions to present the data


#' Convert matrix to a vector
#'
matrix_to_vec<-function(mat, sep=".") {
  colNames<-colnames(mat)
  rowNames<-rownames(mat)
  tmp1<-rep(colNames, each=length(rowNames))
  tmp2<-rep(rowNames, times=length(colNames))
  newNames<-paste(tmp1, tmp2, sep=sep)
  vals<-as.vector(mat)
  names(vals)<-newNames
  return(vals)
}

#' Convert an object to string
#'
to_string.StockValue<-function(obj) {
  rates<-attr(obj, "rates")
  payout<-attr(obj, "payout")
  names(rates)<-paste("dcfRate", names(rates), sep=".")
  return(c(rates, payout=payout, obj))
}

#' Convert an object to string
#'

to_string<-function(x, ...) {
  UseMethod("to_string", x)
}

#' Reduce digits
#'
#' Reduce the number of digits in the numeric columns
#' by keeping the specified number of significant digits
reduce_digits<-function(dat, digits=4) {
	stopifnot(any(is.data.frame(dat), is.matrix(dat), is.numeric(dat) ))
	if( is.null(dim(dat)) ) {  return(signif(dat, digits)) }
	# data.frame or matrix
	tmpd<-sapply(seq_len(ncol(dat)), function(i) if(is.numeric(dat[,i])) { dat[,i]<<-signif(dat[,i], digits) } )
	return(dat)
}

#' Find undervalued stocks
#'
#' Reads pre-computed stock metrics on intrinsic values
#' and add latest stock prices to find the stocks undervalued
#'
#' @param metricFile The file containing stock metrics
#' @param metrics A data.frame containing the data of stock metrics,
#'   if this is provided, the parameter `metricFile` will be ignored.

find_undervalued_stocks<-function(metricFile=NULL, metrics=NULL, sep=",", ...) {
	stopifnot(!all(is.null(metricFile), is.null(metrics) ))
	if(is.null(metrics)) {
		metrics<-read_file(metricFile)
	}
	priceColumns<-grep("^estPrice\\.", colnames(metrics), value=T)
	priceColumnsPE<-grep("(\\.(minPE|meanPE))", priceColumns, value=T)
	priceColumnsPS<-grep("(\\.(minPS|meanPS))", priceColumns, value=T)
	rateColumns<-grep("^rate\\.", colnames(metrics), value=T)
	rateRange<-t(apply(metrics[,rateColumns], 1, range, na.rm=T))
	colnames(rateRange)<-c("minRate","maxRate")
	# also get the long-term rate, 10yr or 5yr, eps over revenue
	revenueRates<-apply(metrics[,c("rate.revenue.10", "rate.revenue.5")],1, mean,na.rm=T)
	epsRates<-apply(metrics[,c("rate.epsDiluted.10", "rate.epsDiluted.5")],1, mean,na.rm=T)
	longTermRate<-ifelse(is.na(epsRates), revenueRates, epsRates)
	metrics$maxPE<-metrics$pe.max
	metrics$maxPS<-metrics$ps.max
	otherSelectedCols<-c( "sps", "minPS", "meanPS", "maxPS", "eps", "minPE", "meanPE", "maxPE")
	## put the selected columns at last
	tmp1<-metrics[,! colnames(metrics) %in% otherSelectedCols]
	metrics<-cbind(tmp1,metrics[,otherSelectedCols], rateRange, longTermRate)
	# get min and max estimated prices
	range_x<-function(x, ...) { # define a function to handle the case of all NAs
		if(all(is.na(x))) { return(c(NA_real_, NA_real_)) }
		range(x, ...)
	}
	## for PE-related
	priceRangePE<-t(apply(metrics[,priceColumnsPE], 1, range_x, na.rm=T))
	colnames(priceRangePE)<-c("minPricePE","maxPricePE")
	## for PS-related
	priceRangePS<-t(apply(metrics[,priceColumnsPS], 1, range_x, na.rm=T))
	colnames(priceRangePS)<-c("minPricePS","maxPricePS")
	## For all
	priceRange<-t(apply(metrics[,priceColumns], 1, range_x, na.rm=T))
	colnames(priceRange)<-c("minPrice","maxPrice")
	metrics<-cbind(metrics, priceRangePS, priceRangePE, priceRange)
	# calculate dcf values using user-provided metrics
	userMetrics<-paste(c("multiple.type","multiple.min","multiple.mean","multiple.max","rate.min","rate.mean","rate.max"),"user",sep=".")
	if( all(userMetrics %in% colnames(metrics)) ) {
		message("Computing DCF values using user's specifications")
		userPrices<-apply(metrics, 1, function(x) { # main function
					usePS<-toupper(x["multiple.type.user"]) == "PS"
					startValue<-ifelse(usePS,
													ifelse(is.na(x["sps.user"]), x["sps"], x["sps.user"]),
													ifelse(is.na(x["eps.user"]), x["eps"], x["eps.user"])
													)
				    estVals<-sapply(c("min","mean","max"),
							function(y) {
										rate<-x[paste("rate",y,"user",sep=".")]
										multiple<-x[paste("multiple",y,"user",sep=".")]
										#cat(startValue, rate, multiple, "\n")
										ifelse(is.na(rate),
											NA_real_,
											dcf(
												as.numeric(startValue),
												usePS=usePS,
												g=as.numeric(rate),
												n=10, # use 10 years
												twoStage = T,
												payout = ifelse(is.na(x["payout"]), 0, as.numeric(x["payout"])),
												multiple = as.numeric(multiple)
												)
										)
							}
					)
					names(estVals)<-paste("userPrice", c("min","mean","max"), sep=".")
					estVals
					}
		)
		# put user metrics at ending columns
		metrics<-cbind(metrics[,! colnames(metrics) %in% userMetrics], metrics[,colnames(metrics) %in% userMetrics])
		metrics<-cbind(metrics, t(userPrices))
	}
	# add current price
	prices<-get_stock_price(metrics$ticker)
	stopifnot( all(metrics$ticker == rownames(prices)) )
	## remove existing columns for price if exist
	metrics<-metrics[, ! colnames(metrics) %in% colnames(prices) ]
	metrics<-cbind(metrics, prices)
	## Let's also calculate the ratio between current stock price and intrinsic values
	stockValueCols<-paste("userPrice", c("min","mean","max"), sep=".")
	if(all(stockValueCols %in% colnames(metrics))) {
		priceToVal<-sapply(stockValueCols, function(x) metrics[["Last"]]/metrics[[x]] )
		colnames(priceToVal)<-paste("Last", stockValueCols, sep="/")
		metrics<-cbind(metrics, priceToVal)
	}
	## format the numeric data
	numericCols<-sapply(metrics, is.numeric)
	metrics[,numericCols]<-signif(metrics[,numericCols], 4)
	return(metrics)
}

#' Portfolio value
#'
#' This function reads a portfolio and return a
#' a data.frame with current value based on current stock
#' prices.
#'
portfolio_value<-function(f=NULL, dat=NULL, tickerCol="Symbol", baseCurrency="usd", ...) {
	stopifnot(!all(is.null(f), is.null(dat) ))
	if(is.null(dat)) {
		dat<-read_file(f, ...)
	}
	stopifnot(tickerCol %in% colnames(dat))
	#if("Currency" %in% colnames(dat)) {
	#	dat[["Currency"]]<-NULL # returned prices have the same column
	#}
	# add current price
	# prices<-suppressWarnings(get_stock_price(unique(dat[[tickerCol]])))
	tickers<-unique(dat[[tickerCol]])
	# only keep tickers which are stocks as well as cash
	tickers.us<-grep("^[a-z]+$", tickers, value=T, ignore.case=T)
	tickers.us<-setdiff(tickers.us, "Cash") # remove Cash
	tickers.hk<-grep("^[0-9]+(\\.HK)?$", tickers, value=T, ignore.case=T)
	## convert HK tickers to the format ####.HK
	tickers.hk1<-sub("^([0-9]+)(\\.HK)?$", "\\1", tickers.hk, ignore.case=T)
	tickers.hk1<-sprintf("%04d.HK", as.integer(tickers.hk1))
	prices<-get_stock_price(c(tickers.us,tickers.hk1))
	prices[["Currency"]]<-NULL; # this column exists in input file
	rownames(prices)<-c(tickers.us,tickers.hk)
	dat<-merge(dat,prices, by.x=tickerCol, by.y=0, all.x=T)
	# post processing
	## get values for each ticker using baseCurrency
	tickerCurrency<-na.omit(unique(dat$Currency))
	fxRatio<-get_fx(tickerCurrency, baseCurrency)
	names(fxRatio)<-tickerCurrency
	dat$marketValue<-dat$Last*dat$Quantity
	dat$marketValue[dat[[tickerCol]] == "Cash"]<-dat$Quantity[dat[[tickerCol]] == "Cash"] # add cash
	tmpfx<-fxRatio[dat$Currency]
	dat$marketValue<-round(dat$marketValue*tmpfx, 2)
	return(dat)
}
