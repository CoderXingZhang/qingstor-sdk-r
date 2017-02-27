#' @rdname cors
#' @title CORS
#' @description Get/Put/Delete the cross origin resource sharing configuration information for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return For \code{get_cors}: A list with cors configuration and rules. For \code{delete_cors}: \code{TRUE} if successful, \code{FALSE} otherwise.
#' @references
#' @export
get_cors <- function(bucket, ...){
    r <- QingHTTP(verb = "GET", 
                bucket = bucket,
                path = '/?cors',
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        return(r)
    }
}

#' @rdname cors
#' @export
put_cors <- function(bucket, ...){
    r <- QingHTTP(verb = "PUT", 
                bucket = bucket,
                path = "?cors",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        structure(r, class = "qing_bucket")
    }
}

#' @rdname cors
#' @export
delete_cors <- function(bucket, ...){
    r <- QingHTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?cors",
                parse_response = FALSE,
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        return(r)
    }
}
