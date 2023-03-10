% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aws_login.R
\name{aws_login}
\alias{aws_login}
\title{Utility Function to login to DataBrew AWS.}
\usage{
aws_login(pipeline_stage = "production")
}
\arguments{
\item{pipeline_stage}{(optional) choose production/develop stage}
}
\value{
metadata of AWS STS authentication (Account, Role)
}
\description{
If user is in an interactive session, generate all the required credentials to run SSO.
If user is running in Terminal / bash / workflow / VM prompt to export temporary credentials
}
