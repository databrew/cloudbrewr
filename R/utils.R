#' Function to get prod/dev stage environment variables,
#' use this to retrieve environment variables used for
#' S3 bucket namespace and profile name to your environment variables.
#' @param stage values `prod`, `dev`
call_cloudbrewr_stage_env_variables <- function(stage){
  if(stage == 'dev'){
    list(
      account_id = '381386504386',
      profile_name = 'databrew-dev',
      bucket_prefix = 'databrew-testing-')
  }else if(stage == 'prod'){
    list(
      account_id = '354598940118',
      profile_name = 'databrew-prod',
      bucket_prefix = ''
    )
  }else{
    stop("[CLOUDBREWR_LOGS]: Please set stage parameter as 'prod' or 'dev'")
  }
}


#' Function to namespace bucket based on prod/dev environment
#' @importFrom magrittr %>%
#'
#' @param bucket databrew s3 bucket
#'
#' @return namspaced buckeet
aws_namespace <- function(bucket){
  sts <- paws::sts()
  aws_caller <- sts$get_caller_identity()
  aws_env <- call_cloudbrewr_stage_env_variables(stage = Sys.getenv('STAGE'))
  bucket_with_namespace <- glue::glue("{aws_env$bucket_prefix}", bucket)
  return(bucket_with_namespace)
}
