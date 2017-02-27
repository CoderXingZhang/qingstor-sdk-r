#' @rdname replication
#' @title Bucket replication
#' @description Get/Delete the replication configuration for a bucket.
#' @details \code{get_replication} gets the current replication policy. \code{delete_replication} deletes the replication policy for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return For \code{get_replication}: A list containing the replication configuration, if one has been set. For \code{delete_replication}: \code{TRUE} if successful, \code{FALSE} otherwise. An \code{qingstor_error} object may be returned if the request failed.
#' @references
#' @export
get_replication <- function(bucket, ...){
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

#' @rdname replication
#' @export
delete_replication <- function(bucket, ...){
    r <- QingHTTP(verb = "DELETE", 
                bucket = bucket,
                path = "?replication",
                parse_response = FALSE,
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        return(r)
    }
}
