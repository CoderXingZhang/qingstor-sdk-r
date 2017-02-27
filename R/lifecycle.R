#' @rdname lifecycle
#' @title Lifecycle
#' @description Get/Put/Delete the lifecycle configuration information for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return For \code{get_lifecycle}: a list with lifecycle configuration, if it has been configured.
#' For \code{delete_lifecycle}: \code{TRUE} if successful, \code{FALSE} otherwise. An \code{qingstor_error} object may be returned if the request failed.
#' @references 
#' @export
get_lifecycle <- function(bucket, ...){
    r <- QingHTTP(verb = "GET", 
                bucket = bucket,
                path = '?lifecycle',
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        r
    }
}

#' @rdname lifecycle
#' @export
put_lifecycle <- function(bucket, ...){
    r <- QingHTTP(verb = "PUT", 
                bucket = bucket,
                action = "?lifecycle",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        structure(r, class = "qing_bucket")
    }
}

#' @rdname lifecycle
#' @export
delete_lifecycle <- function(bucket, ...){
    r <- QingHTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?lifecycle",
                parse_response = FALSE,
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        
    }
}
