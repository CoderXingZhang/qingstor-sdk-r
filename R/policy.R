#' @rdname policy
#' @title Bucket policies
#' @description Get/Put/Delete the bucket access policy for a bucket.
#' @template bucket
#' @param policy A character string containing a bucket policy.
#' @template dots
#' @return For \code{get_policy}: A character string containing the JSON representation of the policy, if one has been set. For \code{delete_policy} and \code{put_policy}: \code{TRUE} if successful, \code{FALSE} otherwise. An \code{qingstor_error} object may be returned if the request failed.
#' @references 
#' @export
get_bucket_policy <- function(bucket, ...){
    r <- QingHTTP(verb = "GET", 
                bucket = bucket,
                query = alist(policy = ),
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        content(r, "text", encoding = "UTF-8")
    }
}

#' @rdname policy
#' @export
put_bucket_policy <- function(bucket, policy, ...){
    r <- QingHTTP(verb = "PUT", 
                bucket = bucket,
                query = alist(policy = ),
                request_body = policy,
                encode = "raw",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        TRUE
    }
}

#' @rdname policy
#' @export
delete_bucket_policy <- function(bucket, ...){
    r <- QingHTTP(verb = "DELETE", 
                bucket = bucket,
                query = alist(policy = ),
                parse_response = FALSE,
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        TRUE
    }
}
