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
#'
#' @return A list with each element corresponding to one variable
#'
calc_growth_rates<-function(dat,
                            vars=c("revenue","epsDiluted", "freeCashFlow"),
                            periods=c(10,5,3,1)
) {
  x<-as.numeric(colnames(dat))
  rates<-sapply(vars,
                function(v) {
                  if(! v %in% rownames(dat)) {
                    message(sprintf("Variable '%s' is not found in input data", v))
                    return()
                  }
                  y<-dat[v,]
                  calc_growth_rate(x=x, y=y, periods=periods)
                })
  if(!is.matrix(rates)) {
    rates<-as.matrix(rates)
    colnames(rates)<-vars
  }
  return(rates)
}

calc_growth_rate<-function(x,y,periods) {
  df<-data.frame(x,y)
  df<-df[order(df$x),] # order as time increases
  n<-nrow(df)
  rates<-sapply(periods,
                function(p) {
                  if(p >= n) { return(NA) }
                  dat<-df[(n-p):n,]
                  dat<-dat[!is.na(dat$y),] # remove NAs
                  #print(dat$y)
                  if(any(dat$y < 0)) { return(NA)}
                  dat$y<-dat$y/dat$y[1] # normalize
                  dat$y<-log(dat$y) # transform
                  if(sum(!is.na(dat$y)) < 2) { # no enough number of datapoints
                    return(NA)
                  }
                  dat$x<-dat$x - dat$x[1] # starting time point at 0
                  model<-MASS::rlm(y~0+x, data=dat)
                  k<-model$coefficients
                  rate<-exp(k)-1
                  return(rate)
                }
  )
  names(rates)<-periods
  return(rates)
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
#' @inheritParams read_financials
#' @param span The number of years for consideration.
#' @param returnDat Logical. If true, the read data is returned
#'   as attribute 'dat'.
#'
get_stock_value<-function(ticker, src="sa",
                          srcDir=NULL,
                          span=10,
                          returnDat=F) {
  dat<-read_financials(ticker, src=src, period = "annual", srcDir=srcDir)
  # calculate growth rates using the averages from eps and revenue
  rates<-calc_growth_rates(dat, vars = c("revenue","epsDiluted"))
  rates<-apply(rates, 1, mean, na.rm=T)
  ratios<-attr(dat, "ratio")
  if("payoutRatio" %in% rownames(ratios)) {
    # use recent 10 years mean payout ratio
    payout<-mean(ratios["payoutRatio",1:min(10, ncol(ratios))], trim=0.1, na.rm=T)
  } else {
    payout<-0
  }
  # get the mean PE ratio in last 10 years
  pe<-mean(ratios["pe",1:min(10, ncol(ratios))], trim=0.1, na.rm=T)
  multiple<-min(10, pe, na.rm=T)
  # message(rates, payout)
  curEPS<-dat["epsDiluted",1]
  if(is.na(curEPS) || curEPS < 0) {
    noVals<-sapply(rates, function(x) NA)
    return(noVals)
  }
  estVals<-sapply(rates, function(x) dcf(
                    curEPS,
                    g=x,
                    n=span,
                    twoStage = T,
                    payout = payout,
                    multiple = multiple
                  )
                  )
  if(returnDat) {
    attr(estVals, "dat") <-dat
  }
  names(estVals)<-paste("estPrice", names(estVals), sep=".")
  attr(estVals, "rates")<-rates
  attr(estVals, "payout")<-payout
  class(estVals)<-c(class(estVals), "StockValue")
  return(estVals)
}

#' Convert an object to string
#'
to_string.StockValue<-function(obj) {
  rates<-attr(obj, "rates")
  payout<-attr(obj, "payout")
  names(rates)<-paste("rate", names(rates), sep=".")
  return(c(rates, payout=payout, obj))
}

#' Convert an object to string
#'

to_string<-function(x, ...) {
  UseMethod("to_string", x)
}



