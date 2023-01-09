#' Function to initiate SSO authentication
#' @description Instantiate SSO connection using internet browser
#' @param profile_name the profile name set to prod/dev
#' @importFrom glue glue
aws_sso_authenticate <- function(profile_name){
  tryCatch({
    aws_args <- paste0(glue::glue("sso login --profile {profile_name}"))
    system2(
      command = "aws",
      args = aws_args)
  }, error = function(e){
    message(e$message)
  })
}


#' Function to create SSO configuration
#' @description Create ~/.aws/config with requirements to SSO to DataBrew AWS Accounts
#' @param env environment variables
aws_configure <- function(env) {
  config_defaults <-  list(
    sso_start_url = 'https://databrewllc.awsapps.com/start',
    sso_region = 'us-east-1',
    sso_role_name = 'cloudbrewr-aws-role',
    region = 'us-east-1',
    output = 'json')

  tryCatch({
    # create dev sso account in aws config
    config_defaults$sso_account_id <- env$account_id
    purrr::map(names(config_defaults), function(key){
      aws_arg_input <- glue::glue(
        'configure set {key} {config_defaults[[key]]} ',
        '--profile {env$profile_name}')
      # create new profile
      system2(command = "aws", args = c(aws_arg_input))
    })
    message("[CLOUDBREWR_LOGS]: config written to ~/.aws/config")
  }, error = function(e){
    message(e$error)
  })
}

#' Function to check AWS environment variables
#' @description To send user message on completion of AWS Environment Variables
check_aws_environment_variables <- function(){
  message('Checking exported environment variables')
  portal_url <- 'https://databrewllc.awsapps.com/start#/'
  if(Sys.getenv("AWS_ACCESS_KEY_ID") == "" |
     is.null(Sys.getenv("AWS_ACCESS_KEY_ID"))){
    stop(glue::glue('[CLOUDBREWR_LOGS]: Please export Access Keys from {portal_url}'))
  } else if (Sys.getenv("AWS_SECRET_ACCESS_KEY") == "" |
             is.null(Sys.getenv("AWS_SECRET_ACCESS_KEY"))){
    stop(glue::glue('[CLOUDBREWR_LOGS]: Please export Secret Access Keys from {portal_url}'))
  } else if (Sys.getenv("AWS_SESSION_TOKEN") == "" |
             is.null(Sys.getenv("AWS_SESSION_TOKEN"))){
    stop(glue::glue('[CLOUDBREWR_LOGS]: Please export Session Token from {portal_url}'))
  } else{
    message('[CLOUDBREWR_LOGS]: Authenticating using AWS Environment Variable')
  }
}

#' Check aws access via STS
#' @return metadata of STS caller identity
check_aws_access <-  function(){
  tryCatch({
    sts <- paws::sts()
    identity <- sts$get_caller_identity()
    msg_content <- glue::glue(
      '[CLOUDBREWR_LOGS]: Welcome to AWS @DataBrew!',
      '\n',
      'You are logged in to {profile_name}',
      profile_name = Sys.getenv('AWS_PROFILE'))
    message(msg_content)
    return(identity)
  }, error = function(e){
    stop(e$message)
  })
}


#' Utility Function to login to DataBrew AWS.
#' If user is in an interactive session, generate all the required credentials to run SSO.
#' If user is running in Terminal / bash / workflow / VM prompt to export temporary credentials
#' @param pipeline_stage (optional) choose production/develop stage
#' @return metadata of AWS STS authentication (Account, Role)
#' @export
aws_login <- function(pipeline_stage = 'production') {
  # get prod/dev account
  aws_env <- call_cloudbrewr_stage_env_variables(pipeline_stage = pipeline_stage)

  # run SSO if you are running interactive RStudio
  if(interactive()){
    # configure sso in aws config
    aws_configure <- aws_configure(env = aws_env)
    aws_sso_authenticate(profile_name = aws_env$profile_name)

  # else run with exported environment variables
  }else{
    # check aws environment variables availability
    check_aws_environment_variables()
  }
  # check if you have access, return role, account metadata if succeed
  Sys.setenv(PIPELINE_STAGE = pipeline_stage)
  Sys.setenv(AWS_PROFILE  = aws_env$profile_name)

  # check access
  check_aws_access()
}


