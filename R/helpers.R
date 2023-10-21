

#' @importFrom stats coef resid coef sd qqline qqnorm AIC BIC
#' @importFrom utils modifyList tail
#' @importFrom graphics abline barplot text
NULL


is.number <- function(x){
  is.numeric(x) && length(x) == 1
}
is.positive.number <- function(x){
  is.number(x) && x > 0
}
is.negative.number <- function(x){
  is.number(x) && x < 0
}
is.zero.or.positive.number <- function(x){
  is.number(x) && x >= 0
}
is.zero.or.negative.number <- function(x){
  is.numeric(x) && length(x) == 1 && x <= 0
}

sprintf0 <- function(numFormat, x) {
  if (length(x) == 0) {
    return(ifelse(x==0,"0",sprintf(numFormat, x)))
  } else {
    return(sapply(x, function(d)ifelse(d==0,0,sprintf(numFormat, d))))
  }
}


