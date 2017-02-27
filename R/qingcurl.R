#' @rdname qingcurl
#' @title qingcurl
#' @description Create an qingcurl connection
#' @template object
#' @template bucket
#' @param open One of \dQuote{r} or \dQuote{rb}.
#' @param region A character string containing the QingStor region. Ignored if region can be inferred from \code{bucket}.
#' If missing, defaults to \dQuote{us-east-1}.
#' @param key A character string containing an Qingstor Access Key ID. 
#' If missing, defaults to value stored in environment variable \dQuote{Qstor_ACCESS_KEY_ID}.
#' @param secret A character string containing an QingStor Secret Access Key. 
#' If missing, defaults to value stored in environment variable \dQuote{Qstor_SECRET_ACCESS_KEY}.
#' @details This function creates a \code{\link[curl]{curl}} connection to be used for asynchronous reading from an Qing object.
#' @return If \code{file = NULL}, a raw object. Otherwise, a character string containing the file name that the object is saved to.
#' @seealso \code{\link{get_object}}, \code{\link{save_object}}, \code{\link[curl]{curl}}
#' @importFrom httr build_url
#' @importFrom curl curl new_handle handle_setheaders handle_reset
qingcurl <- function(object, bucket, open = "rb",                
                   accelerate = FALSE,
                   region = Sys.getenv("Qstor_DEFAULT_REGION", "pek3a"), 
                   key = Sys.getenv("Qstor_ACCESS_KEY_ID"), 
                   secret = Sys.getenv("Qstor_SECRET_ACCESS_KEY")) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } 
    object <- get_objectkey(object)
    bucketname <- get_bucketname(bucket)
    bucketregion <- get_region(bucket)
    if (!is.null(bucketregion)) {
        region <- bucketregion
    }
    if (region == "") {
        region <- "pek3a"
    }
    url <- setup_qingstor_url(bucketname, region, path = "", accelerate)
    p <- httr::parse_url(url)
    
    current <- Sys.time()
    d_timestamp <- format(current, "%Y%m%dT%H%M%SZ", tz = "UTC")
    canonical_headers <- c(list(host = p$hostname,
                              `Date` = d_timestamp))
    if (key == "") {
        headers <- list(`Date` = d_timestamp)
    } else {
        Sig <- aws.signature::signature_v4_auth(
               datetime = d_timestamp,
               region = region,
               service = "Qing",
               verb = "GET",
               action = if (p$path == "") "/" else paste0("/", p$path),
               query_args = NULL,
               canonical_headers = canonical_headers,
               request_body = "",
               key = key, secret = secret)
        headers <- list()
        headers$`Date` <- d_timestamp
        headers$`x-amz-content-sha256` <- Sig$BodyHash
        headers$Authorization <- Sig$SignatureHeader
    }
    
    handle <- new_handle()
    on.exit(curl::handle_reset(handle))
    handle_setheaders(handle, .list = c(headers, list("User-Agent" = "r/QingStor.Qing")))
    return(curl(build_url(p), open = open, handle = handle))
}
