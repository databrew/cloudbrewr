# CloudBrewR

@Author: [atediarjo\@gmail.com](mailto:atediarjo@gmail.com){.email}

Internal Tooling for DataBrew for Cloud ETL.

The purpose of this R package is to enable user to be able to retrieve and store research data at scale in DataBrew AWS Environment.

## Prerequisites

-   To enable `cloudbrewr` to run on your machine, make sure that you have AWS CLI installed. [Installation Link](https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html)

-   SSO Access - please reach out to DataBrew team ([atediarjo\@gmail.com](mailto:atediarjo@gmail.com){.email}, [joebrew\@gmail.com](mailto:joebrew@gmail.com){.email})

## Installation

Installation can be done through Github installation:

``` r
devtools::install_github('databrew/cloudbrewr')
```

## AWS Authentication

There are two procedures in AWS Session Authentication

- Interactive: RStudio, RStudio Shell
- Non-Interactive: Bash, Terminal (Not the one in RStudio)

### Non-Interactive Session Login
When running your script through non-interactive session (terminal/bash/virtual-machines) please manually export the access keys from the `Command line or programmatic access` in the [SSO Portal](https://databrewllc.awsapps.com/start/#/)

In bash:
```bash
export AWS_ACCESS_KEY_ID='SOME_ACCESS_KEY'
export AWS_SECRET_ACCESS_KEY='SOME_SECRET_ACCESS_KEY'
export AWS_SESSION_TOKEN='SOME_SESSION_TOKEN'
```

Your RScript:
```r
library(cloudbrewr)
cloudbrewr::aws_login() # no need to define role name if using manual export
```

If you are running in DataBrew AWS Compute Resources (EC2, Lambda, ECS) **Please use IAM Role-Based Access**

### Interactive Session Login

Run this in RStudio:
``` r
library(cloudbrewr)
cloudbrewr::aws_login(
  role_name = 'SSO_ROLE_NAME_FROM_WEB_PORTAL',
  profile_name = 'AWS_PROFILE_NAME_OF_CHOICE'
)
```
-   Pass in your role name this will be arbitrary based on user. For example, `bohemia-box-team-s3-role` for box access or `bohemia-ento-team-s3-role` for ento folder access

-   After you run this code snippet, you will be redirected to our [AWS SSO Portal](https://databrewllc.awsapps.com/start/#/) in your default browser

-   Input username and password that was created from the email invitation

-   Click `Allow` on the following window prompt

-   Voila! Your R session is now connected to AWS

-   This command will create a profile in `~/.aws/config`, where AWS will use it as a reference for future authentication

## Fetching your first object

An example of how you can get an S3 object and read it as a `.csv` file

``` r
object <- cloudbrewr::aws_s3_get_object(
    bucket = 'bohemia-datalake',
    key = 'bohemia-minicensus/clean_minicensus_main.csv',
)

# read based on object metadata
read.csv(object$file_path)
```

## Vignettes

To read more about other features, check out our team Vignettes here in [Github Pages](http://www.databrew.cc/cloudbrewr/)

## Troubleshooting & FAQs

Not able to authenticate?

1.  Check \~/.aws/config and see whether you do not have the same profile name with different AWS Account set up, as this package will append to your config file

2.  Check your AWS access by using the STS service

To run STS in R:

``` r
library(paws)
sts <- paws::sts()
sts$get_caller_identity()
```

Clarify that your account is DataBrew AWS Account `354598940118`

3. For further issues, post to [Github Issues](https://github.com/databrew/cloudbrewr/issues)


Issues with authenticating in Windows?

Although this is an edge-case issue, this issue can happen due to the improper `aws configure` process, where AWS profile information parsed by this package is not written to `.aws/config` file. Thus user will be required to do manual setup by creating both the folder and file in their home directory.

1. In the home directory, create a folder named `.aws` and a create an empty text file name `config` under it
2. Add this SSO settings, based on known parameter
```
[profile my-dev-profile]
sso_start_url = https://my-sso-portal.awsapps.com/start
sso_region = us-east-1
sso_account_id = 123456789011
sso_role_name = readOnly
region = us-west-2
output = json
```
3. Log in as usual


## Contribute

In-Dev by [atediarjo\@gmail.com](mailto:atediarjo@gmail.com){.email}
