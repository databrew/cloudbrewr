call_cloudbrewr_env_params <- function(){
  list(
    dev = list(
      account_id = '381386504386',
      profile_name = 'databrew-dev',
      bucket_prefix = 'databrew-testing-'),
    prod = list(
      account_id = '354598940118',
      profile_name = 'databrew-prod',
      bucket_prefix = ''
    )
  )
}


aws_namespace <- function(bucket){
  sts <- paws::sts()
  aws_caller <- sts$get_caller_identity()
  aws_env <- call_cloudbrewr_env_params()

  # get dev_prod_environment by calling account info
  current_env <- aws_env %>%
    purrr::map(~.x$account_id == aws_caller$Account) %>%
    unlist() %>%
    which() %>%
    names(.)

  bucket_with_namespace <- glue::glue(
    "{aws_env[[current_env]]$bucket_prefix}",
    bucket)

  return(bucket_with_namespace)

}
