#' @rdname requestpayment
#' @title requestPayment
#' @description Get/Put the requestPayment subresource for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return A list containing the requestPayment information, if set.
#' @export
get_requestpayment <- function(bucket, ...){
    r <- QingHTTP(verb = "GET", 
                bucket = bucket,
                path = "?requestPayment",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        r
    }
}

#' @rdname requestpayment
#' @export
put_requestpayment <- function(bucket, ...){
    r <- QingHTTP(verb = "PUT", 
                bucket = bucket,
                path = "?requestPayment",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        structure(r, class = "qing_bucket")
    }
}
