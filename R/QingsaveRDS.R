#' @title saveRDS/readRDS
#' @description Serialization interface to read/write R objects to Qing
#' @author Steven Akins <skawesome@gmail.com>
#' 
#' @param x For \code{QingsaveRDS}, a single R object to be saved via \code{\link[base]{saveRDS}} and uploaded to Qing. \code{x} is analogous to the \code{object} argument in \code{saveRDS}.
#' @template bucket
#' @template object
#' @template dots
#'
#' @return For \code{QingsaveRDS}, a logical. For \code{QingreadRDS}, an R object.
#' @examples
#' \dontrun{
#' # create bucket
#' b <- put_bucket("myexamplebucket")
#'
#' # save a single object to Qing
#' QingsaveRDS(x = mtcars, bucket = "myexamplebucket", object = "mtcars.rds")
#'
#' # restore it under a different name
#' mtcars2 <- QingreadRDS(object = "mtcars.rds", bucket = "myexamplebucket")
#' identical(mtcars, mtcars2)
#' 
#' # cleanup
#' delete_object(object = "mtcars.rds", bucket = "myexamplebucket")
#' delete_bucket("myexamplebucket")
#' }
#' @seealso \code{\link{Qingsave}},\code{\link{Qingload}}
#' @export
QingsaveRDS <- function(x, bucket, object = paste0(as.character(substitute(x)), ".rds"), ...) {
    b <- memCompress(from = serialize(x, connection = NULL), type = 'gzip')
    r <- put_object(file = b, bucket = bucket, object = object, ...)
    if (inherits(r, "qingstor_error")) {
        return(r)
    } else {
        return(invisible(r))
    }
}

#' @rdname QingsaveRDS
#' @export
QingreadRDS <- function(bucket, object, ...) {
    r <- get_object(bucket = bucket, object = object, ...)
    if (typeof(r) == 'raw') {
        return(unserialize(memDecompress(from = as.vector(r), type = 'gzip')))
    } else {
        return(r)
    }
}
