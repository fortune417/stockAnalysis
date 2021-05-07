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


