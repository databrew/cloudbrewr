#' Function to create S3 bucket
#' @description create S3 bucket via API
#' @param bucket s3 bucket
#' @param namespace_bucket namespace bucket for prod/dev
#' @param ... additional parameter passed to s3 create_bucket endpoint
#' @export
aws_s3_create_bucket <- function(bucket, namespace_bucket = TRUE, ...){
  tryCatch({
    if(namespace_bucket){
      bucket <- aws_namespace(bucket)
    }
    s3obj <- paws::s3()
    s3obj$create_bucket(Bucket = bucket, ...)
    profile_name <- Sys.getenv('AWS_PROFILE')
    message(glue::glue("[CLOUDBREWR_LOGS]: {bucket} is created in {profile_name}"))
  }, error = function(e){
    stop(e$message)
  })
}
