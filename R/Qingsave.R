#' @title save/load
#' @description Save/load R object(s) to/from Qing
#' 
#' @param ... For \code{Qingsave}, one or more R objects to be saved via \code{\link[base]{save}} and uploaded to Qing. For \code{Qingload}, see \code{opts}.
#' @template bucket
#' @param object For \code{Qingsave}, a character string of the name of the object you want to save to. For \code{Qingload}, a character string of the name of the object you want to load from Qing.
#' @param opts Additional arguments passed to \code{\link{QingHTTP}}.
#' @param envir An R environment to load objects into. Default is the \code{parent.frame()} from which the function is called.
#'
#' @return For \code{Qingsave}, a logical, invisibly, otherwise an error object. For \code{Qingload}, \code{NULL} invisibly, otherwise an error object.
#' @examples
#' \dontrun{
#' # create bucket
#' b <- put_bucket("myexamplebucket")
#'
#' # save a dataset to the bucket
#' Qingsave(mtcars, iris, object = "somedata.Rdata", bucket = b)
#' get_bucket(b)
#'
#' # load the data from bucket
#' e <- new.env()
#' Qingload(object = "somedata.Rdata", bucket = b, envir = e)
#' ls(e)
#'
#' # cleanup
#' rm(e)
#' delete_object(object = "somedata.Rdata", bucket = "myexamplebucket")
#' delete_bucket("myexamplebucket")
#' }
#' @seealso \code{\link{QingsaveRDS}},\code{\link{QingreadRDS}}
#' @export
Qingsave <- function(..., object, bucket, opts = NULL) {
    tmp <- rawConnection(raw(0), "r+")
    on.exit(close(tmp))
    save(..., file = tmp)
    if (is.null(opts)) {
        r <- put_object(file = rawConnectionValue(tmp), bucket = bucket, object = object)
    } else {
        r <- do.call("put_object", c(list(file = rawConnectionValue(tmp), bucket = bucket, object = object), opts))
    }
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        return(invisible(r))
    }
}

#' @rdname Qingsave
#' @export
Qingsave_image <- function(object, bucket, opts = NULL) {
    tmp <- rawConnection(raw(0), "r+")
    on.exit(close(tmp))
    save.image(file = tmp)
    if (is.null(opts)) {
        r <- put_object(file = rawConnectionValue(tmp), bucket = bucket, object = object)
    } else {
        r <- do.call("put_object", c(list(file = rawConnectionValue(tmp), bucket = bucket, object = object), opts))
    }
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        return(invisible(r))
    }
}

#' @rdname Qingsave
#' @export
Qingload <- function(object, bucket, envir = parent.frame(), ...) {
    r <- get_object(bucket = bucket, object = object, parse_response = FALSE, ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        tmp <- rawConnection(r, "r")
        on.exit(close(tmp))
        load(tmp, envir = envir)
        return(invisible())
    }
}
