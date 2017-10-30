#' Print TAF Table
#'
#' Print a TAF table, controlling how \code{NA} values are shown.
#'
#' @param x a data frame in TAF format.
#' @param quote whether all values should be quoted.
#' @param na.print string to use for missing values. Alternatively, a logical
#'        where \code{TRUE} is interpreted as \code{"NA"} and \code{FALSE} is
#'        interpreted as \code{""}.
#' @param right whether strings should be right aligned.
#' @param row.names whether to include row names.
#' @param \dots passed to \code{print.default}.
#'
#' @seealso
#' \code{\link{print.default}} is the underlying function used to print a
#' matrix.
#'
#' \code{\link{write.taf}} writes a TAF table to a file.
#'
#' \code{\link{icesTAF-package}} gives an overview of the package.
#'
#' @examples
#' x <- data.frame(Year=c(2000,NA), SSB=c(123456,800), Fbar=c(1.234,1.5))
#'
#' print.taf(x)
#' print.taf(x, na="-")
#' print.taf(x, na=TRUE)
#'
#' @export print.taf

print.taf <- function(x, quote=FALSE, na.print="", right=TRUE, row.names=FALSE,
                      ...)
{
  if(is.logical(na.print))
    na.print <- if(na.print) "NA" else ""
  m <- trimws(as.matrix(format(x)))
  m[is.na(x)] <- NA
  if(!row.names)
    rownames(m) <- rep("", nrow(m))
  print(m, quote=quote, na.print=na.print, right=right, ...)
}
