#' @title List Buckets
#' @description List buckets for authenticated user
#' @details \code{bucketlist} performs a GET operation on the base Qing endpoint 
#' and returns a list of all buckets owned by the authenticated 
#' sender of the request. If authentication is successful, this function provides a list of 
#' buckets available to the authenticated user. In this way, it can serve as 
#' a \dQuote{hello world!} function, to confirm that one's authentication 
#' credentials are working correctly.
#' 
#' @template dots
#'
#' @return a list of buckets.  if passed with default settings \dQuote{parse_response = TRUE}
#' the response will list the name and creationdate of all buckets owned by the request
#' sender.
#' 
#' 
#' @keywords service
#' 
#' @export
bucketlist <- function(...) {
    r <- QingHTTP(verb = "GET", ...)
    #errors and unparsed
    if (inherits(r, "qingstor_error") | inherits(r, "response")) {
        return(r)
    #parsed
    } else {
        structure(r$Buckets, class = "qing_bucketlist")
    }
}
