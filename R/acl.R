get_acl <- function(object, bucket, ...) {
    if (!missing(bucket)) {
        r <- QingHTTP(verb = "GET", 
                    bucket = bucket,
                    path = "?acl",
                    ...)
        
        if (inherits(r, "qingstor_error")) {
            return(r)
        } else {
            structure(r, class = "qing_bucket")
        }
    } else if (!missing(object)) {
        object <- get_objectkey(object)
        r <- QingHTTP(verb = "GET", 
                    path = "/object?acl",
                    ...)
        if (inherits(r, "qingstor_error")) {
            return(r)
        } else {
            structure(r, class = "qing_object")
        }
    }
}

putobject_acl <- function(object, bucket, ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    if (missing(object)) {
        r <- QingHTTP(verb = "PUT", 
                    bucket = bucket,
                    path = "?acl",
                    ...)
        if (inherits(r, "qingstor_error")) {
            return(r)
        } else {
            structure(r, class = "qing_bucket")
        }
    } else {
        if (inherits(object, "qing_object"))
            object <- object$Key
        r <- QingHTTP(verb = "PUT", 
                    bucket = bucket,
                    path = paste0("/", object),
                    ...)
        if (inherits(r, "qingstor_error")) {
            return(r)
        } else {
            structure(r, class = "qing_object")
        }
    }
}
