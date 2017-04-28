#' @title Put object
#' @description Puts an object into an Qingstor bucket
#'
#' @param file A character string containing the filename (or full path) of 
#' the file you want to upload to Qingstor.
#' @param folder A character string specify the folder name in the bucket. Optional
#' @template bucket
#' @param object A character string containing the name the object should 
#' have in Qingstor (i.e., its "object key"). If missing, the filename is used.
#' @param headers List of request headers for the REST call.   
#' @examples
#' # put an object in Qingstor bucket
#' tempPath <- file.path("path","to","folder","xxxx.csv")
#' put_object(file = tempPath , object = basename(tempPath),folder = 'folder_name', bucket = 'bucket_name')
#'
#' @return If successful, \code{TRUE}, otherwise an qingstor_error object.
#' @seealso \code{\link{delete_object}}
#' @export
put_object <- function(file, object,bucket, headers = list(), ...) {
    if (missing(object)) {
        object <- basename(file)
    } else {
        object <- get_objectkey(object)
    }
    r <- QingHTTP(verb = "PUT", 
                bucket = bucket,
                path = paste0('/', object),
                headers = c(headers, list(
                  `Content-Length` = ifelse(is.character(file) && file.exists(file), 
                                                       file.size(file), length(file))
                  )), 
                request_body = file,
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        TRUE
    }
}

post_object <- function(object, bucket, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } 
    object <- get_objectkey(object)
    r <- QingHTTP(verb = "POST", 
                bucket = bucket,
                path = paste0("/", object),
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        structure(r, class = "qing_object")
    }
}


#' @title Delete object
#' @description Deletes one objects from an Qingstor bucket.
#'
#' @template object
#' @template bucket
#' @examples
#' # delete an object in Qingstor bucket
#' 
#' delete_object(object = 'object_name', bucket = 'bucket_name')
#' the object_name could be a name under a folder like 'some_folder/object_name'
#'
#' @return \code{TRUE} if successful, otherwise an object of class Qingstor_error details if not.
#' @seealso \code{\link{put_object}}
#' @importFrom digest digest
#' @importFrom base64enc base64encode
#' @importFrom jsonlite fromJSON 
#' @export
delete_object <- function(object, bucket, quiet = TRUE, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    regionname <- get_region(bucket)
    object <- get_objectkey(object)
    if (length(object) == 1) {
        r <- QingHTTP(verb = "DELETE", 
                    bucket = bucket,
                    path = paste0("/", object),
                    ...)
        if (inherits(r, "qingstor_error")) {
            return(r)
        } else {
            return(r)
          #  return(TRUE)
        }
    } else {
        xml <- read_xml(paste0('<?xml version="1.0" encoding="UTF-8"?><Delete><Quiet>', tolower(quiet),'</Quiet></Delete>'))
        for (i in seq_along(object)) {
            xml2::xml_add_child(xml, xml2::read_xml(paste0("<Object><Key>", get_objectkey(object[[i]]), "</Key></Object>")))
        }
        tmpfile <- tempfile()
        on.exit(unlink(tmpfile))
        xml2::write_xml(xml, tmpfile)
        md <- base64enc::base64encode(digest::digest(file = tmpfile, raw = TRUE))
        r <- QingHTTP(verb = "POST", 
                    bucket = bucket,
                    path = "?delete",
                    body = tmpfile,
                    headers = list(`Content-Length` = file.size(tmpfile), 
                                   `Content-MD5` = md), 
                    ...)
        if (inherits(r, "qingstor_error")) {
            return(r)
        } else {
            return(TRUE)
        }
    }
    return(r)
}

# OPTIONS

opts_object <- function(object, bucket, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } 
    object <- get_objectkey(object)
    r <- QingHTTP(verb = "OPTIONS", 
                bucket = bucket,
                path = paste0("/", object),
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        return(r)
    }
}
