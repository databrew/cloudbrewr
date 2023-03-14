# CloudBrewR

@Author: atediarjo@gmail.com

Internal Tooling for DataBrew for Cloud ETL. 

The purpose of this R package is to enable user to be able to retrieve and store research data at scale in DataBrew AWS Environment. 

## Prerequisites

- To enable `cloudbrewr` to run on your machine, make sure that you have AWS CLI installed. [Installation Link](https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html)

- SSO Access - please reach out to DataBrew team (atediarjo@gmail.com, joebrew@gmail.com)

## Installation

Installation can be done through Github installation:

```r
devtools::install_github('databrew/cloudbrewr')
```

## Log in to AWS

```r
library(cloudbrewr)
cloudbrewr::aws_login(
  role_name = 'SSO_ROLE_NAME_FROM_WEB_PORTAL',
  profile_name = 'AWS_PROFILE_NAME_OF_CHOICE'
)
```

- Pass in your role name this will be arbitrary based on user. For example, `bohemia-box-team-s3-role` for box access or `bohemia-ento-team-s3-role` for ento folder access

- After you run this code snippet, you will be redirected to our [AWS SSO Portal](https://databrewllc.awsapps.com/start/#/) in your default browser

- Input username and password that was created from the email invitation

- Click `Allow` on the following window prompt

- Voila! Your R session is now connected to AWS

- This command will create a profile in `~/.aws/config`, where AWS will use it as a reference for future authentication


## Fetching your first object

An example of how you can get an S3 object and read it as a `.csv` file

```r
object <- cloudbrewr::aws_s3_get_object(
    bucket = 'bohemia-datalake',
    key = 'bohemia-minicensus/clean_minicensus_main.csv',
)

# read based on object metadata
read.csv(object$file_path)
```

## Vignettes

To read more about other features, check out our team Vignettes here in [Github Pages](http://www.databrew.cc/cloudbrewr/)

## Contribute

In-Dev by atediarjo@gmail.com
