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


#' Check aws access via STS
#' @return metadata of STS caller identity
check_aws_access <-  function(){
  tryCatch({
    sts <- paws::sts()
    identity <- sts$get_caller_identity()
    return(identity)
  }, error = function(e){
    stop(e$message)
  })
}

#' Utility Function to login to DataBrew AWS.
#' @description
#' If user is in an interactive session, generate all the required credentials to run SSO.
#' If user is running in Terminal / bash / workflow / VM prompt to export temporary credentials
#' @param pipeline_stage (optional) choose production/develop stage
#' @return metadata of AWS STS authentication (Account, Role)
#' @export
aws_login <- function(pipeline_stage = 'production',
                      region = NULL,
                      access_key = NULL,
                      secret_access_key = NULL,
                      session_token = NULL) {



  # get prod/dev account
  Sys.setenv(PIPELINE_STAGE = pipeline_stage)
  aws_env <- call_cloudbrewr_stage_env_variables(pipeline_stage = pipeline_stage)

  # skip login if aws account is desired
  is_login <- FALSE
  creds <- tryCatch({
    check_aws_access()
  }, error = function(e){
    message('not logged in proceed')
  })

  if(!is.null(creds)){
    if(creds$Account == aws_env$account_id){
      is_login = TRUE
    }
  }

  if(!is_login){
    # run SSO if you are running interactive RStudio, this option will be bypassed
    # if you are running it as an RScript/EC2/Cron/Docker (use env variable export or IAM role for best practice)
    if(interactive()){
      # login via given access keys from SSO portal
      if(!is.null(access_key)
         & !is.null(secret_access_key)
         & !is.null(session_token)
         & !is.null(region)){
        Sys.setenv(
          AWS_ACCESS_KEY_ID = access_key,
          AWS_SECRET_ACCESS_KEY = secret_access_key,
          AWS_SESSION_TOKEN = session_token,
          AWS_REGION = region
        )
      }else{
        # do SSO if access key is not given
        # configure sso in aws config
        aws_configure <- aws_configure(env = aws_env)
        Sys.setenv(AWS_PROFILE  = aws_env$profile_name)
        aws_sso_authenticate(profile_name = aws_env$profile_name)
      }
    }

    # feedback to user when logged in
    msg_content <- glue::glue(
      '[CLOUDBREWR_LOGS]: Welcome to AWS @DataBrew!',
      '\n',
      'You are logged in to {profile_name}',
      profile_name = aws_env$profile_name)
    message(msg_content)
  }
}


