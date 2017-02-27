#' @rdname acceleration
#' @title Bucket Acceleration
#' @description Get/Put acceleration settings or retrieve acceleration status of a bucket.
#'
#' @template bucket
#' @param status Character string specifying whether acceleration should be \dQuote{Enabled} or \dQuote{Suspended}.
#' @template dots
#'
#' @return For \code{get_acceleration}: If acceleration has never been enabled or suspend, the value is \code{NULL}. Otherwise, the status is returned (either \dQuote{Enabled} or \dQuote{Suspended}). For \code{put_acceleration}: If acceleration has never been enabled or suspend, the value is \code{NULL}.
#'' @examples
#' \dontrun{
#' b <- bucketlist()
#' get_acceleration(b[[1]])
#' put_acceleration(b[[1]], "Enabled")
#' get_acceleration(b[[1]])
#' put_acceleration(b[[1]], "Suspended")
#' }
#' @export
get_acceleration <- function(bucket, ...){
    r <- QingHTTP(verb = "GET", 
                bucket = bucket,
                path = "?accelerate",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        attributes(r) <- NULL
        if (identical(r, list())) {
            return(NULL)
        } else {
            return(r)
        }
    }
}

#' @rdname acceleration
#' @export
put_acceleration <- function(bucket, status = c("Enabled", "Suspended"), ...){
    b <- paste0()
    r <- QingHTTP(verb = "PUT", 
                bucket = bucket,
                path = "?accelerate",
                request_body = b,
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        attributes(r) <- NULL
        if (identical(r, list())) {
            return(NULL)
        } else {
            return(r)
        }
    }
}
