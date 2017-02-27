#' @rdname tagging
#' @title Bucket tagging
#' @description Get/delete the tag set for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return A list containing the tag set, if one has been set.
#' For \code{delete_tagging}: \code{TRUE} if successful, \code{FALSE} otherwise. An \code{qingstor_error} object may be returned if the request failed.
#' @references
#' @export
get_tagging <- function(bucket, ...){
    r <- QingHTTP(verb = "GET", 
                bucket = bucket,
                path = "?tagging",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        return(r)
    }
}

#' @rdname tagging
#' @export
put_tagging <- function(bucket, ...){
    r <- QingHTTP(verb = "PUT", 
                bucket = bucket,
                path = "?tagging",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        structure(r, class = "qing_bucket")
    }
}

#' @rdname tagging
#' @export
delete_tagging <- function(bucket, ...){
    r <- QingHTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?tagging",
                parse_response = FALSE,
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        return(r)
    }
}
