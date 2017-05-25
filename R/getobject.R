#' @rdname getobject
#' @title Get object
#' @description Retrieve an object from an Qingstor bucket
#' @template object
#' @template bucket
#' @param headers List of request headers for the REST call.
#' @details \code{get_object} retrieves an object into memory.
#' @examples
#' # get an object in memory
#' 
#' get_object(object = 'object_name', bucket = 'bucket_name')
#' the object_name could be a name under a folder like 'some_folder/object_name'
#' @return DataFrame if the object is a csv file.
#' @seealso \code{\link{get_bucket}}
#' @export
get_object <- function(object, bucket, headers = list(), parse_response = FALSE, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } 
    object <- get_objectkey(object)
    r <- QingHTTP(verb = "GET", 
                bucket = bucket,
                path = paste0("/", object),
                headers = headers,
                parse_response = parse_response,
                ...)

    url <- setup_qingstor_url(bucket,region = Sys.getenv("Qstor_DEFAULT_REGION", "pek3a"),"/", accelerate = FALSE)
    url <- paste0(url,path = paste0(object))

    #print(url)

    if(grepl('\\.csv',object)){
         res <- data.table::fread(url,showProgress = FALSE)
    }else if(grepl('\\.xls',object) || grepl('\\.xlsx',object)){
        filenames <- strsplit(object,'/')
        filename <- filenames[[1]][2]   
        download.file(url,filename,mode = "wb")
        res <- read.xlsx(filename,sheetIndex = 1)
    }





    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
      #  cont <- httr::content(r, as = "raw")
      # attributes(cont) <- httr::headers(r)
        return(res)
    }
}

# rdname getobject
# export
save_object <- function(object, bucket, file, headers = list(), ...) {
    if (missing(file)) {
        stop('argument "file" is missing, with no default')
    }
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } 
    object <- get_objectkey(object)
    r <- QingHTTP(verb = "GET", 
                bucket = bucket,
                path = paste0("/", object),
                headers = headers,
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        writeBin(httr::content(r, as = "raw"), con = file)
        return(file)
    }
}

#rdname getobject
#export
head_object <- function(object, bucket, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } 
    object <- get_objectkey(object)
    r <- QingHTTP(verb = "HEAD", 
                bucket = bucket,
                path = paste0("/", object),
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        structure(r, class = "HEAD")
    }
}

#' @title Get object torrent
#' @description Retrieves a Bencoded dictionary (BitTorrent) for an object from an Qing bucket.
#' 
#' @template object
#' @template bucket
#' @template dots
#'
#' @return Something.
#' @export
get_torrent <- function(object, bucket, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } 
    object <- get_objectkey(object)
    r <- QingHTTP(verb = "GET", 
                bucket = bucket,
                path = paste0("/", object, "?torrent"),
                ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        return(r)
    }
}
