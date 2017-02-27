#' @title Source from Qing
#' @description Source R code (a la \code{\link[base]{source}}) from Qing
#' 
#' @template object
#' @template bucket
#' @template dots
#' @param opts Additional arguments passed to \code{\link{get_object}} for retrieving the R syntax file.
#'
#' @return See \code{\link[base]{source}}
#' @examples
#' \dontrun{
#' # create bucket
#' b <- put_bucket("myexamplebucket")
#'
#' # save some code to the bucket
#' cat("x <- 'hello world!'\nx", file = "example.R")
#' put_object("example.R", object = "example.R", bucket = b)
#' get_bucket(b)
#'
#' # source the code from the bucket
#' Qingsource(object = "example.R", bucket = b, echo = TRUE)
#'
#' # cleanup
#' unlink("example.R")
#' delete_object(object = "example.R", bucket = b)
#' delete_bucket("myexamplebucket")
#' }
#' @seealso \code{\link{QingsaveRDS}},\code{\link{Qingsave}}, \code{\link{get_object}}
#' @export
Qingsource <- function(object, bucket, ..., opts = NULL) {
    if (is.null(opts)) {
        r <- get_object(bucket = bucket, object = object)
    } else {
        r <- do.call("get_object", c(list(bucket = bucket, object = object), opts))
    }
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        tmp <- rawConnection(r, "r+")
        on.exit(close(tmp))
        return(source(tmp, ...))
    }
}
