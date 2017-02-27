#' @rdname versions
#' @title Bucket versions
#' @description Get/Put versioning settings or retrieve versions of bucket objects.
#' @details \code{get_versioning} returns the versioning status of a bucket; \code{put_versioning} sets the versioning status. \code{get_versions} returns information about bucket versions.
#'
#' @template bucket
#' @param status Character string specifying whether versioning should be
#' \dQuote{Enabled} or \dQuote{Suspended}.
#' @template dots
#'
#' @return For \code{get_versioning}: If versioning has never been enabled or suspend, the value is \code{NULL}. Otherwise, the status is returned (either \dQuote{Enabled} or \dQuote{Suspended}). For \code{put_versioning}: If versioning has never been enabled or suspend, the value is \code{NULL}. Otherwise, the status is returned (either \dQuote{Enabled} or \dQuote{Suspended}).
#' For \code{get_versions}: A list.
#' @references 
#' @export
get_versions <- function(bucket, ...){
    r <- QingHTTP(verb = "GET", 
                bucket = bucket,
                path = "?versions",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        return(r)
    }
}

#' @rdname versions
#' @export
get_versioning <- function(bucket, ...){
    r <- QingHTTP(verb = "GET", 
                bucket = bucket,
                path = "?versioning",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        if (identical(r, list())) {
            return(NULL)
        } else {
            return(r$Status)
        }
    }
}

#' @rdname versions
#' @export
put_versioning <- function(bucket, status = c("Enabled", "Suspended"), ...){
    b <- paste0() # note this does not currently allow MFA Delete
    r <- QingHTTP(verb = "PUT", 
                bucket = bucket,
                path = "?versioning",
                request_body = b,
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        if (identical(r, list())) {
            return(NULL)
        } else {
            return(r$Status)
        }
    }
}
