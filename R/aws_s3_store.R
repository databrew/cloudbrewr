#' Function to store object to S3
#' @description Store any object to S3 (limited to <=5GB per object)
#' @param filename your filename
#' @param bucket s3 bucket
#' @param key s3 object key
#' @param ... additional parameter passed to s3 put_object
#' @export
aws_s3_store <- function(filename, bucket, key, ...){
  tryCatch({
    s3obj <- paws::s3()
    if(namespace_bucket){
      bucket <- aws_namespace(bucket)
    }
    s3obj$put_object(Body = filename, Bucket = bucket, Key = key, ...)
    message(glue::glue("[CLOUDBREWR_LOGS]: File is uploaded with in bucket:{bucket}; key:{key}"))
  }, error = function(e){
    stop(e$message)
  })
}
