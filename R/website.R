#' @rdname website
#' @title Bucket Website configuration
#' @description Get/Put/Delete the website configuration for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return For \code{put_website} and \code{get_website}, a list containing the website configuration, if one has been set.
#' For \code{delete_website}: \code{TRUE} if successful, \code{FALSE} otherwise.
#' An \code{qingstor_error} object may be returned if the request failed.
#' @export
delete_website <- function(bucket, ...){
    r <- QingHTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?website",
                parse_response = FALSE,
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        return(r)
    }
}

#' @rdname website
#' @export
put_website <- function(bucket, ...){
    r <- QingHTTP(verb = "PUT", 
                bucket = bucket,
                path = "?website",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        structure(r, class = "qing_bucket")
    }
}

#' @rdname website
#' @export
get_website <- function(bucket, ...){
    r <- QingHTTP(verb = "GET", 
                bucket = bucket,
                path = "?website",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        return(r)
    }
}
