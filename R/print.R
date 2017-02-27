#' @export
print.qing_bucketlist <- function(x, ...) {
    out <- do.call("rbind.data.frame", x)
    rownames(out) <- 1:nrow(out)
    print(out, right = FALSE, row.names = FALSE, ...)
    invisible(x)
}

#' @export
print.qing_bucket <- function(x, ...){
    cat("Bucket:", attributes(x)[["Name"]], "\n\n")
    print(x[names(x) == "Contents"], ...)
    invisible(x)
}

#' @export
print.qing_object <- function(x, ...){
    cat("Key:           ", x$Key, "\n")
    cat("LastModified:  ", x$LastModified, "\n")
    cat("ETag:          ", x$ETag, "\n")
    cat("Size (B):      ", x$Size, "\n")
    cat("Owner:         ", x$Owner$DisplayName, "\n")
    cat("Storage class: ", x$StorageClass, "\n")
    invisible(x)
}

#' @export
print.qingstor_error <- function(x, ...){
    message("QINGSTOR API Error Encountered. Details below:")
    print(str(x))
    invisible(x)
}
