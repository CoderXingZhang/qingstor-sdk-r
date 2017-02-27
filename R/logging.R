get_logging <- function(bucket, ...){
    r <- QingHTTP(verb = "GET", 
                bucket = bucket,
                path = "?logging",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        r
    }
}

put_logging <- function(bucket, ...){
    r <- QingHTTP(verb = "PUT", 
                bucket = bucket,
                path = "?logging",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        structure(r, class = "qing_bucket")
    }
}
