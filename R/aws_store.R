#' Function to store file to S3 storage
aws_store <- function(filename, bucket, key, multipart = FALSE, ...){
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











