#' @rdname notifications
#' @title Notifications
#' @description Get/put the notification configuration for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return A list containing the notification configuration, if one has been set.
#' @references 
#' @export
get_notification <- function(bucket, ...){
    r <- QingHTTP(verb = "GET", 
                bucket = bucket,
                path = "?notification",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        return(r)
    }
}

#' @rdname notifications
#' @export
put_notification <- function(bucket, ...){
    r <- QingHTTP(verb = "PUT", 
                bucket = bucket,
                path = "?notification",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        structure(r, class = "qing_bucket")
    }
}
