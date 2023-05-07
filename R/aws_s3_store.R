#' Function to store object to S3
#' @description Store any object to S3 (limited to <=5GB per object)
#' @param filename your filename
#' @param bucket s3 bucket
#' @param key s3 object key
#' @param namespace_bucket boolean on whether to namespace bucket based on prod/dev environment
#' @param ... additional parameter passed to s3 put_object
#' @export
aws_s3_store <- function(filename, bucket, key, namespace_bucket = TRUE, ...){
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

#' Bulk Store Object in DataBrew S3
#' @param bucket s3 bucket to store data
#' @param prefix (optional) folder prefix to store data
#' @param namespace_bucket boolean to create namespace bucket, set to FALSE to override bucket namespace
#' @param target_dir folder to sync with aws
#'
#' @importFrom magrittr %>%
#'
#' @return list of object s3 metadata and file location
#' @export
aws_s3_bulk_store <- function(bucket,
                              prefix = NULL,
                              namespace_bucket = TRUE,
                              target_dir = NULL,
                              ...){
  # authenticate to s3
  s3obj <- paws::s3()

  # name space bucket
  if(namespace_bucket){
    bucket <- aws_namespace(bucket)
  }

  # target dir argument
  if(!is.null(prefix)){
    bucket_args <- glue::glue('{target_dir} s3://{bucket}{prefix}')
  }else{
    bucket_args <- glue::glue('{target_dir} s3://{bucket}')
  }

  # running aws s3 sync
  tryCatch({
    logger::log_info('Running bulk download')
    system2(command = "aws", args = c('s3 sync',  bucket_args))
  }, error = function(e){
    logger::log_error(e$message)
    stop(e$message)
  })
}

