#' Function to initiate SSO authentication
#' @description Instantiate SSO connection using internet browser
#' @param profile_name the profile name set to prod/dev
#' @importFrom glue glue
aws_sso_authenticate <- function(profile_name, account_id){
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
aws_configure <- function(profile_name,
                          role_name,
                          account_region,
                          account_id,
                          sso_start_url,
                          sso_region) {
  config_defaults <-  list(
    sso_start_url = sso_start_url,
    sso_region = sso_region,
    sso_role_name = role_name,
    region = account_region,
    output = 'json')

  tryCatch({
    # create dev sso account in aws config
    config_defaults$sso_account_id <- account_id
    purrr::map(names(config_defaults), function(key){
      aws_arg_input <- glue::glue(
        'configure set {key} {config_defaults[[key]]} ',
        '--profile {profile_name}')
      # create new profile
      system2(command = "aws", args = c(aws_arg_input))
    })
    logger::log_info(glue::glue("{profile_name} written to ~/.aws/config"))
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
#' @param account_id AWS account id
#' @param role_name set role name based on SSO portal, this will be different based on different users but default values will be set to cloudbrewr-aws-role
#' @param profile_name set profile name to save your SSO configuration locally, this will be different based on different users
#' @param account_region default to us-east-1
#' @param access_key retrieve from portal for access key access
#' @param secret_access_key retrieve from portal for secret access key
#' @param session_token retrieve fro portal
#' @param pipeline_stage (DataBrew internal use for CI/CD) choose production/develop stage
#' @return metadata of AWS STS authentication (Account, Role)
#' @export
aws_login <- function(role_name,
                      profile_name,
                      pipeline_stage='production',
                      account_id=NULL,
                      account_region=NULL,
                      access_key = NULL,
                      secret_access_key = NULL,
                      session_token = NULL,
                      sso_start_url = NULL,
                      sso_region=NULL,
                      bucket_prefix = '') {

  # instantiate pipeline stage
  Sys.setenv(PIPELINE_STAGE = pipeline_stage)
  if(pipeline_stage == 'develop') {
    aws_env <- call_cloudbrewr_stage_env_variables(account_id = '381386504386',
                                                   profile_name = 'databrew-dev',
                                                   bucket_prefix = 'databrew-testing-')
    sso_start_url <- 'https://databrewllc.awsapps.com/start'
    account_region <- sso_region <- 'us-east-1'
    account_id <- aws_env$account_id
    profile_name <- aws_env$profile_name
    role_name <- 'AdministratorAccess'
  }else if(pipeline_stage == 'production') {
    aws_env <- call_cloudbrewr_stage_env_variables(account_id = '354598940118',
                                                   profile_name = 'databrew-prod',
                                                   bucket_prefix = '')
    sso_start_url <- 'https://databrewllc.awsapps.com/start'
    account_region <- sso_region <- 'us-east-1'
    role_name <- role_name
    profile_name <- profile_name
    account_id <- aws_env$account_id
  }else{
    aws_env <- call_cloudbrewr_stage_env_variables(account_id = account_id,
                                                   profile_name = profile_name,
                                                   bucket_prefix = bucket_prefix)
    role_name <- role_name
    profile_name <- profile_name
    account_id <- aws_env$account_id
  }

  # skip login if aws account is desired
  is_login <- FALSE
  creds <- tryCatch({
    check_aws_access()
  }, error = function(e){
    logger::log_info('User not logged in: Attempting Login...')
  })

  if(!is.null(creds)){
    if(creds$Account == account_id){
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
         & !is.null(account_region)){
        logger::log_info('Attempting Access Key Login')
        Sys.setenv(
          AWS_ACCESS_KEY_ID = access_key,
          AWS_SECRET_ACCESS_KEY = secret_access_key,
          AWS_SESSION_TOKEN = session_token,
          AWS_REGION = account_region
        )
      }else{
        # do SSO if access key is not given
        # configure sso in aws config
        logger::log_info('Attempting SSO Login')
        aws_configure <- aws_configure(profile_name = profile_name,
                                       role_name =  role_name,
                                       account_id = account_id,
                                       account_region = account_region,
                                       sso_start_url = sso_start_url,
                                       sso_region = sso_region)
        Sys.setenv(AWS_PROFILE = profile_name)
        aws_sso_authenticate(profile_name = profile_name,
                             account_id = account_id)
      }
    }
  }
}
