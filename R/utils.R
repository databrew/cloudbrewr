#' Function to get prod/dev stage environment variables,
#' @description Retrieve environment variables used for S3 bucket namespace and profile name to your environment variables.
#' @param pipeline_stage choose production/develop stage
call_cloudbrewr_stage_env_variables <- function(account_id, profile_name, bucket_prefix){
  Sys.setenv(BUCKET_PREFIX = bucket_prefix)
  Sys.setenv(PROFILE_NAME = profile_name)
  Sys.setenv(ACCOUNT_ID = account_id)

  aws_env <-   list(
    account_id = account_id,
    profile_name = profile_name,
    bucket_prefix = bucket_prefix
  )

  return(aws_env)
}


#' Function to namespace bucket
#' @description This function is used to namespace bucket based on pipelines stages
#' @importFrom magrittr %>%
#' @param bucket databrew s3 bucket
#' @return namspaced buckeet
aws_namespace <- function(bucket){
  bucket_with_namespace <- glue::glue("{Sys.getenv('BUCKET_PREFIX')}", bucket)
  return(bucket_with_namespace)
}

#' Function to check file cache
#' @description This function is used to check downloaded files in cache
#' @param etag S3 etag - this is unique identifier of a file
#' @import cachem
#' @return boolean
check_cloudbrewr_cache <- function(etag, cache = "~/.cloudbrewr_cache"){
  d <- pull_cache(cache)
  value <- d$get(etag)
  return(!cachem::is.key_missing(value))
}

#' Function to pull from cache
#' @description This function is used to instantiate cache filesystem
#' @param cache desired cache location, set to cloudbrewr_cache
#' @import cachem
#' @return cachem object
pull_cache <- function(cache = "~/.cloudbrewr_cache"){
  d <- cachem::cache_disk(dir = cache)
  return(d)
}
