% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aws_s3_table.R
\name{aws_s3_get_table}
\alias{aws_s3_get_table}
\title{Function to retrieve s3 object directly into table}
\usage{
aws_s3_get_table(bucket, key, namespace_bucket = TRUE, ...)
}
\arguments{
\item{bucket}{s3 bucket}

\item{key}{s3 object key}

\item{namespace_bucket}{boolean to create namespace bucket, set to FALSE to override bucket namespace}

\item{...}{additional parameter from S3 get object}
}
\value{
tibble data-frame of s3 object
}
\description{
wrapper function to transform s3 into tibble
}