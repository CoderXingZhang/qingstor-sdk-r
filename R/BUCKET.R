#' @title Get bucket
#' @description List the contents of an Qingstor bucket
#' 
#' @template bucket
#' @param prefix Character string that limits the response to keys that begin 
#' with the specified prefix
#' @param folder must, now set to "/"
#' @param parse_response logical, should we attempt to parse the response?
#' @examples
#' # list the object in Qingstor bucket
#' 
#' get_bucket(bucket = 'bucket_name',folder = "/")
#' prefix is support ,example:
#' get_bucket(bucket = 'bucket_name',folder = "/",prefix = 'prefix_name')
#'
#' @return A dataFrame of objects infomation in the bucket.
#'
#' @export
get_bucket <- function(bucket, 
                       prefix = NULL, 
                       delimiter = NULL,
                       max = NULL,
                       marker = NULL, 
                       parse_response = TRUE,
                       ...) {
   
    query <- list(prefix = prefix, delimiter = delimiter, "max-keys" = max, marker = marker)
    r <- QingHTTP(verb = "GET", bucket = bucket, query = query, parse_response = parse_response, ...)

    if (!parse_response) {
      out <- r
    } else if (inherits(r, "qingstor_error")) {
      out <- r
    } else {
    #    for (i in which(names(r) == "Contents")) {
     #     r[[i]][["Bucket"]] <- get_bucketname(bucket)
      #    r[[i]][["Size"]] <- as.numeric(r[[i]][["Size"]])
     #     attr(r[[i]], "class") <- "qing_object"
     #   }
     #   att <- r[names(r) != "Contents"]
     #   r[names(r) != "Contents"] <- NULL
     #   out <- structure(r, class = "qing_bucket")
     #   attributes(out) <- c(attributes(out), att)
     out <- r$keys$key
    }
    out
}

#' @title Delete Bucket
#' @description Deletes an Qing bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return \code{TRUE} if successful, \code{FALSE} otherwise. 
#' An \code{qingstor_error} object may be returned if the request failed.
#' @export
delete_bucket <- function(bucket, ...){
    r <- QingHTTP(verb = "DELETE", 
                bucket = bucket,
                parse_response = FALSE,
                ...)
    if (inherits(r, "qingstor_error")) {
      return(r)
    } else {
      return(r)
    }
}


#' @title Bucket location
#' @description Get the qingstor region location of bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return A character string containing the region, if one has been set.
#' @export
get_location <- function(bucket, ...){
    r <- QingHTTP(verb = "GET", 
                bucket = bucket,
                path = "?location",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        r
    }
}


#' @title Multipart uploads
#' @description Get a list of multipart uploads for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return A list containing the multipart upload information.
#' @export
get_uploads <- function(bucket, ...){
    r <- QingHTTP(verb = "GET", 
                bucket = bucket,
                path = "?uploads",
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        return(r)
    }
}



#' @title Bucket exists?
#' @description Check whether a bucket exists and is accessible with the current authentication keys.
#' @template bucket
#' @template dots
#'
#' @return \code{TRUE} if bucket exists and is accessible, else \code{FALSE}. An \code{qingstor_error} object may be returned if the request failed.
#' @export
bucket_exists <- function(bucket, ...){
    r <- QingHTTP(verb = "HEAD", 
                bucket = bucket,
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        return(r)
    }
}


#' @title Create bucket
#' @description Creates a new Qing bucket.
#' @template bucket
#' @template dots
#'
#' @return \code{TRUE} if successful, qingstor_error details if not.
#' @export
put_bucket <- function(bucket, ...){
    r <- QingHTTP(verb = "PUT", 
                bucket = bucket,
                parse_response = FALSE,
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        TRUE
    }
}

