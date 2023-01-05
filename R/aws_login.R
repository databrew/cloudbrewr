library(glue)
library(dplyr)

aws_sso_authenticate <- function(profile_name){
  tryCatch({
    aws_args <- glue::glue("sso login --profile {profile_name}")
    system2(
      command = "aws",
      args = aws_args)
    Sys.setenv(AWS_PROFILE  = profile_name)
    message_content <- glue(
      '[CLOUDBREWR_LOGS]: Welcome to DataBrew AWS!','\n',
      'You are logged in to {profile_name}')
    message(message_content)
  }, error = function(e){
    message(e$message)
  })
}


aws_configure <- function(env) {
  config_defaults <-  list(
    sso_start_url = 'https://databrewllc.awsapps.com/start',
    sso_region = 'us-east-1',
    sso_role_name = 'cloudbrewr-aws-role',
    region = 'us-east-1',
    output = 'json')

  tryCatch({
    # create dev sso account in aws config
    config_defaults$sso_account_id <- env$dev$account_id
    purrr::map(names(config_defaults), function(key){
      aws_arg_input <- glue::glue(
        'configure set {key} {config_defaults[[key]]} ',
        '--profile {env$dev$profile_name}')
      # create new profile
      system2(command = "aws", args = c(aws_arg_input))
    })

    # create prod sso account in aws config
    config_defaults$sso_account_id <- env$prod$account_id
    purrr::map(names(config_defaults), function(key){
      aws_arg_input <- glue::glue(
        'configure set {key} {config_defaults[[key]]} ',
        '--profile {env$prod$profile_name}')
      # create new profile
      system2(command = "aws", args = c(aws_arg_input))
    })
    message("[CLOUDBREWR_LOGS]: config written to ~/.aws/config")
  }, error = function(e){
    message(e$error)
  })
}



#' Function to login to aws
#' use this on interactive session
#' to work on local testing
#' NOTE: USE THIS FOR LOCAL TESTING ONLY
#' @param prod prod/dev choose AWS account
#' @export
aws_sso_login <- function(prod = TRUE) {
  # get prod/dev account
  aws_env <- call_cloudbrewr_env_params()
  # configure sso in aws config
  aws_configure <- aws_configure(env = aws_env)
  # set aws profiles
  if(prod){
    aws_sso_authenticate(profile_name = aws_env$prod$profile_name)
  }else{
    aws_sso_authenticate(profile_name = aws_env$dev$profile_name)
  }
}


