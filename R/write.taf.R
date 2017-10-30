#' Write TAF Table to File
#'
#' Write a TAF table to a file.
#'
#' @param x a data frame in TAF format.
#' @param file a filename.
#' @param quote whether to quote strings.
#' @param sep column separator string.
#' @param na string to use for missing values. Alternatively, a logical where
#'        \code{TRUE} is interpreted as \code{"NA"} and \code{FALSE} is
#'        interpreted as \code{""}.
#' @param row.names whether to include row names.
#' @param fileEncoding character encoding for output file.
#' @param \dots passed to \code{write.table}.
#'
#' @seealso
#' \code{\link{write.table}} is the underlying function used to write a table to
#' a file.
#'
#' \code{\link{read.taf}} reads a TAF table from a file into a data frame.
#'
#' \code{\link{print.taf}} prints a TAF table.
#'
#' \code{\link{icesTAF-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' write.taf(catage.taf, "catage.csv")
#' catage <- read.taf("catage.csv")
#'
#' file.remove("catage.csv")
#' }
#'
#' x <- data.frame(Year=c(2000,NA), SSB=c(123456,800), Fbar=c(1.234,1.5))
#' write.taf(x)
#' write.taf(x, na="-")
#' write.taf(x, na=FALSE)
#' write.taf(x, na=FALSE, sep="\t")
#'
#' @importFrom utils write.table
#'
#' @export

write.taf <- function(x, file="", quote=FALSE, sep=",", na="NA",
                      row.names=FALSE, fileEncoding="UTF-8", ...)
{
  if(is.logical(na))
    na <- if(na) "NA" else ""
  m <- trimws(as.matrix(format(x)))
  m[is.na(x)] <- NA
  write.table(m, file=file, quote=quote, sep=sep, na=na, row.names=row.names,
              fileEncoding=fileEncoding, ...)
  if(file != "")
    unix2dos(file)
}
