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
#' @param x An object to convert to string
#' @param ... Additional arguments
#' @export
to_string<-function(x, ...) {
  UseMethod("to_string", x)
}

#' Convert an object to string (StockValue method)
#'
#' @param x A StockValue object to convert to string
#' @param ... Additional arguments
#' @rdname to_string
#' @export
to_string.StockValue<-function(x, ...) {
  rates<-attr(x, "rates")
  payout<-attr(x, "payout")
  names(rates)<-paste("dcfRate", names(rates), sep=".")
  return(c(rates, payout=payout, x))
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

find_undervalued_stocks<-function(metricFile=NULL, metrics=NULL, sep=",",
                                  discount_threshold=0.8, min_market_cap=100,
                                  additional_criteria=NULL, ...) {
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

	# Additional valuation filters
	if("marketCap" %in% colnames(metrics)) {
		metrics <- metrics[metrics$marketCap >= min_market_cap*1e6, ]  # Filter by market cap
	}

	# Identify undervalued stocks based on price-to-value ratio
	undervalued_stocks <- metrics
	if("Last/userPrice.mean" %in% colnames(undervalued_stocks)) {
		undervalued_stocks <- undervalued_stocks[undervalued_stocks[["Last/userPrice.mean"]] <= discount_threshold, ]
	} else if("Last/userPrice.max" %in% colnames(undervalued_stocks)) {
		undervalued_stocks <- undervalued_stocks[undervalued_stocks[["Last/userPrice.max"]] <= discount_threshold, ]
	}

	## Apply additional criteria if specified
	if(!is.null(additional_criteria)) {
		for(criteria_name in names(additional_criteria)) {
			criteria <- additional_criteria[[criteria_name]]
			if(criteria_name %in% colnames(undervalued_stocks)) {
				if("min" %in% names(criteria)) {
					undervalued_stocks <- undervalued_stocks[undervalued_stocks[[criteria_name]] >= criteria$min, ]
				}
				if("max" %in% names(criteria)) {
					undervalued_stocks <- undervalued_stocks[undervalued_stocks[[criteria_name]] <= criteria$max, ]
				}
			}
		}
	}

	## format the numeric data
	numericCols<-sapply(undervalued_stocks, is.numeric)
	undervalued_stocks[,numericCols]<-signif(undervalued_stocks[,numericCols], 4)

	# Add a valuation score based on multiple factors
	undervalued_stocks <- add_valuation_score(undervalued_stocks)

	return(undervalued_stocks)
}

#' Add a comprehensive valuation score to stock data
#'
#' This function calculates a composite valuation score based on multiple factors
#'
#' @param stock_data Data frame containing stock metrics
#' @return Updated data frame with valuation score
add_valuation_score <- function(stock_data) {
	# Calculate various components for the valuation score
	score_components <- data.frame(
		ticker = stock_data$ticker,
		price_to_value_score = NA,
		financial_health_score = NA,
		growth_score = NA
	)

	# Price to value score (lower is better value - higher score)
	if("Last/userPrice.mean" %in% colnames(stock_data)) {
		price_to_value_ratio <- stock_data[["Last/userPrice.mean"]]
		# Convert ratio to score (0-100 scale, lower ratio = higher score)
		score_components$price_to_value_score <- pmax(0, pmin(100, (1 - price_to_value_ratio) * 100))
	}

	# Financial health score based on available metrics
	health_metrics <- c("debtToEquity", "currentRatio", "roa", "roequity")
	available_health_metrics <- intersect(health_metrics, colnames(stock_data))
	if(length(available_health_metrics) > 0) {
		health_scores <- stock_data[, available_health_metrics, drop=FALSE]

		# Normalize and aggregate health scores
		for(metric in available_health_metrics) {
			if(grepl("debt|ratio", metric, ignore.case=TRUE)) {
				# For debt-related ratios, lower is better
				if(metric == "debtToEquity") {
					health_scores[[metric]] <- pmax(0, pmin(100, (1 - health_scores[[metric]]) * 100))
				} else if(metric == "currentRatio") {
					# Higher current ratio is better
					health_scores[[metric]] <- pmax(0, pmin(100, (health_scores[[metric]] - 0.5) * 100))
				}
			} else {
				# For ROA, ROE, higher is better
				health_scores[[metric]] <- pmax(0, pmin(100, health_scores[[metric]] * 1000))
			}
		}

		score_components$financial_health_score <- rowMeans(health_scores, na.rm=TRUE)
	}

	# Growth score based on growth rates
	growth_metrics <- grep("^rate\\.", colnames(stock_data), value=TRUE)
	if(length(growth_metrics) > 0) {
		growth_data <- stock_data[, growth_metrics, drop=FALSE]
		score_components$growth_score <- rowMeans(growth_data, na.rm=TRUE) * 100  # Convert to 0-100 scale
	}

	# Calculate composite score (weighted average)
	weights <- c(price_to_value=0.4, financial_health=0.35, growth=0.25)

	# Ensure weights sum to 1
	weights <- weights / sum(weights)

	# Calculate weighted score
	weighted_scores <- (
		weights["price_to_value"] * score_components$price_to_value_score +
		weights["financial_health"] * score_components$financial_health_score +
		weights["growth"] * score_components$growth_score
	)

	# Add the score to the original data
	stock_data$valuation_score <- weighted_scores
	score_components$valuation_score <- weighted_scores

	# Add the score components to the original data
	stock_data$price_to_value_score <- score_components$price_to_value_score
	stock_data$financial_health_score <- score_components$financial_health_score
	stock_data$growth_score <- score_components$growth_score

	return(stock_data)
}

#' Rank stocks by valuation metrics
#'
#' This function ranks stocks based on various valuation metrics
#'
#' @param stock_data Data frame containing stock metrics
#' @param rank_by Character string indicating how to rank (e.g., "value", "growth", "financial_health")
#' @return Ranked data frame
rank_stocks_by_valuation <- function(stock_data, rank_by = "value") {
	# Add valuation score if not already present
	if(!"valuation_score" %in% colnames(stock_data)) {
		stock_data <- add_valuation_score(stock_data)
	}

	# Rank based on selected criterion
	switch(rank_by,
		"value" = {
			# Rank by valuation score (higher is better value)
			ranks <- order(stock_data$valuation_score, decreasing = TRUE)
		},
		"financial_health" = {
			# Rank by financial health score
			if("financial_health_score" %in% colnames(stock_data)) {
				ranks <- order(stock_data$financial_health_score, decreasing = TRUE)
			} else {
				stop("Financial health score not available")
			}
		},
		"growth" = {
			# Rank by growth score
			if("growth_score" %in% colnames(stock_data)) {
				ranks <- order(stock_data$growth_score, decreasing = TRUE)
			} else {
				stop("Growth score not available")
			}
		},
		{
			# Default: rank by valuation score
			ranks <- order(stock_data$valuation_score, decreasing = TRUE)
		}
	)

	return(stock_data[ranks, ])
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
