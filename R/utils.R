#' Function to get prod/dev stage environment variables,
#' @description
#' Retrieve environment variables used for
#' S3 bucket namespace and profile name to
#' your environment variables.
#' @param pipeline_stage choose production/develop stage
call_cloudbrewr_stage_env_variables <- function(pipeline_stage){
  if(pipeline_stage == 'develop'){
    list(
      account_id = '381386504386',
      profile_name = 'databrew-dev',
      bucket_prefix = 'databrew-testing-')
  }else if(pipeline_stage == 'production'){
    list(
      account_id = '354598940118',
      profile_name = 'databrew-prod',
      bucket_prefix = ''
    )
  }else{
    stop("[CLOUDBREWR_LOGS]: Please set stage parameter as 'production' or 'develop'")
  }
}


#' Function to namespace bucket
#' @description This function is used to namespace bucket based on pipelines stages
#' @importFrom magrittr %>%
#' @param bucket databrew s3 bucket
#' @return namspaced buckeet
aws_namespace <- function(bucket){
  sts <- paws::sts()
  aws_caller <- sts$get_caller_identity()
  aws_env <- call_cloudbrewr_stage_env_variables(pipeline_stage = Sys.getenv('PIPELINE_STAGE'))
  bucket_with_namespace <- glue::glue("{aws_env$bucket_prefix}", bucket)
  return(bucket_with_namespace)
}
