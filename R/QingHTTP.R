#' @title Qing HTTP Requests
#' @description This is the workhorse function for executing API requests for Qing.
#' @details This is mostly an internal function for executing API requests. In almost all cases, users do not need to access this directly.
#' @param verb A character string containing an HTTP verb, defaulting to \dQuote{GET}.
#' @param bucket A character string with the name of the bucket, or an object of class \dQuote{qing_bucket}. If the latter and a region can be inferred from the bucket object attributes, then that region is used instead of \code{region}.
#' @param path A character string with the name of the object to put in the bucket 
#' (sometimes called the object or 'key name' in the QingStor documentation.)
#' @param query any queries, passed as a named list 
#' @param headers a list of request headers for the REST call.   
#' @param request_body character string of request body data.
#' @param accelerate A logical indicating whether to use QingStor transfer acceleration, which can produce significant speed improvements for cross-country transfers. Acceleration only works with buckets that do not have dots in bucket name.
#' @param region A character string containing the QingStor region. Ignored if region can be inferred from \code{bucket}. If missing, defaults to \dQuote{us-east-1}.
#' @param key A character string containing an QingStor Access Key ID. If missing, defaults to value stored in environment variable \dQuote{Qstor_ACCESS_KEY_ID}.
#' @param secret A character string containing an QingStor Secret Access Key. If missing, defaults to value stored in environment variable \dQuote{Qstor_SECRET_ACCESS_KEY}.
#' @param parse_response return the response as is, or parse and return as a list? Default is TRUE.
#' @param ... Additional arguments passed to an HTTP request function. such as \code{\link[httr]{GET}}.
#' @return the Qing response, or the relevant error.
#' @importFrom httr GET POST PUT HEAD DELETE VERB upload_file parse_url add_headers
#' @importFrom httr http_error http_status warn_for_status content headers
#' @importFrom xml2 read_xml as_list
#' @importFrom utils URLencode
#' @import aws.signature
#' @export
QingHTTP <- function(verb = "GET",
                   bucket = "", 
                   path = "", 
                   folder = "",
                   query = NULL,
                   headers = list(), 
                   request_body = "",
                   accelerate = FALSE,
                   region = Sys.getenv("Qstor_DEFAULT_REGION", "pek3a"), 
                   key = Sys.getenv("Qstor_ACCESS_KEY_ID"), 
                   secret = Sys.getenv("Qstor_SECRET_ACCESS_KEY"), 
                   session_token = Sys.getenv("Qstor_SESSION_TOKEN"),
                   parse_response = TRUE, 
                   ...) {
    
    bucketname <- get_bucketname(bucket)
    bucketregion <- get_region(bucket)
    if (!is.null(bucketregion)) {
        region <- bucketregion
    }
    if (region == "") {
        region <- "pek3a"
    }

    #region



     if (!is.null(folder) && folder != "") 
     {
       path <- paste0("/",folder,path)
       if( folder == "/")
       {
       path <- paste0(folder)
       }
     }

    
      print(path)

    encodedPath <- if (path == "") "/" else {
        paste(sapply(
            strsplit(path, '/')[[1]],
            function(i) URLencode(i, TRUE),
            USE.NAMES = FALSE
        ), collapse = '/')
    }

    url <- setup_qingstor_url(bucketname, region, encodedPath, accelerate)

    #url

    p <- parse_url(url)

    #p

    Sys.setlocale("LC_TIME", "English")
    tempdate <- format(Sys.time() - 3600*8,'%a, %d %b %Y %H:%M:%S')
    d_timestamp <- paste(tempdate,"GMT")


    # current <- Sys.time()
    # d_timestamp <- format(current, "%Y%m%dT%H%M%SZ", tz = "GMT")
    action <- if (p$path == "") "/" else paste0("/", p$path)
    canonical_headers <- c(list(host = p$hostname,
                                `x-amz-date` = d_timestamp), headers)

    if (is.null(query) && !is.null(p$query)) {
        query <- p[["query"]]
    }
    if (all(sapply(query, is.null))) {
        query <- NULL
    }


# set http request header 
    if (key == "") {
        headers[["Date"]] <- d_timestamp
        Sig <- list()
        H <- do.call(add_headers, headers)
    } else {

    headers[["Date"]] <- d_timestamp
    headers[["Host"]] <-paste0("pek3a.qingstor.com") 
    

    Canonicalized_Resource <- paste0("/",bucket,path)

    if (!is.null(request_body) && request_body != "") 
    {
      content_type <- mime::guess_type(request_body)
      string_to_sign <- paste0(verb,"\n\n", content_type,"\n" ,d_timestamp,"\n",Canonicalized_Resource)
    
    }else{
      string_to_sign <- paste0(verb,"\n\n\n" ,d_timestamp,"\n",Canonicalized_Resource)
    }
     
    print(string_to_sign)

    accesskey <- openssl::base64_encode(openssl::sha256(charToRaw(string_to_sign),key=secret))
    SignatureHeader <- paste0("QS ",key, ":",accesskey) 

    headers[["Authorization"]] <-   SignatureHeader
    H <- do.call(add_headers, headers)

    }






    if (verb == "GET") {
     #   url
      r <- GET(url, H, query = query, ...)

    } else if (verb == "HEAD") {
      r <- HEAD(url, H, query = query, ...)
      s <- http_status(r)
      if (tolower(s$category) == "success") {
          out <- TRUE
          attributes(out) <- c(attributes(out), headers(r))
          return(out)
      } else {
          message(s$message)
          out <- FALSE
          attributes(out) <- c(attributes(out), headers(r))
          return(out)
      }
    } else if (verb == "DELETE") {
      r <- DELETE(url, H, query = query, ...)
      s <- http_status(r)
      if (tolower(s$category) == "success") {
          out <- TRUE
          attributes(out) <- c(attributes(out), headers(r))
          return(out)
      } else {
          message(s$message)
          out <- FALSE
          attributes(out) <- c(attributes(out), headers(r))
          return(out)
      }
    } else if (verb == "POST") {
      r <- POST(url, H, query = query, ...)
    } else if (verb == "PUT") {
      if (is.character(request_body) && request_body == "") {
        r <- PUT(url, H, query = query, ...)
      } else if (is.character(request_body) && file.exists(request_body)) {
        r <- PUT(url, H, body = upload_file(request_body), query = query, ...)
      } else {
        r <- PUT(url, H, body = request_body, query = query, ...)
      }
    } else if (verb == "OPTIONS") {
      r <- VERB("OPTIONS", url, H, query = query, ...)
    }
    
 
    if (isTRUE(parse_response)) {
    #  print(r)
      out <- parse_qingstor_response(r)
    
    } else {
      out <- r
    }
    attributes(out) <- c(attributes(out), headers(r))
    out

   # print(out$keys)
}

parse_qingstor_response <- function(r, verbose = getOption("verbose")){
    ctype <- headers(r)[["content-type"]]
    if (is.null(ctype) || ctype == "application/json"){
        content <- content(r, as = "text", encoding = "UTF-8")
        if (content != "") {
           # response_contents <- as_list(read_xml(content))
           #response <- flatten_list(response_contents)
           response <- fromJSON(content)
        } else {
            response <- NULL
        }
    } else {
        response <- r
    }
    if (http_error(r) | (http_status(r)[["category"]] == "Redirection")) {
        warn_for_status(r)
        h <- headers(r)
        out <- structure(response, headers = h, class = "qingstor_error")
      #  attr(out, "request_canonical") <- Sig$CanonicalRequest
      #  attr(out, "request_string_to_sign") <- Sig$StringToSign
      #  attr(out, "request_signature") <- Sig$SignatureHeader
    } else {
        out <- response
    }
    
    return(out)
}

setup_qingstor_url <- function(bucketname, region, path, accelerate) {
    if (bucketname == "") {
        if (region == "pek3a") {
            url <- paste0("https://pek3a.qingstor.com")
        } else {
            url <- paste0("https://", region, ".qingstor.com")
        }
    } else {
        if (isTRUE(accelerate)) {
            if (grepl("\\.", bucketname)) {
                stop("To use accelerate, bucket name must not contain dots (.)")
            }
        } else {
            if (region == "pek3a") {
                url <- paste0("https://", "pek3a.qingstor.com/",bucketname)
            } else {
                url <- paste0("https://", bucketname, ".", region, ".qingstor.com")
            }
        }
    }
    url <- if (grepl('^[\\/].*', path)) { paste0(url, path) } else { paste(url, path, sep = "/") }
    return(url)
}
