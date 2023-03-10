% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aws_s3_store.R
\name{aws_s3_store}
\alias{aws_s3_store}
\title{Function to store object to S3}
\usage{
aws_s3_store(filename, bucket, key, namespace_bucket = TRUE, ...)
}
\arguments{
\item{filename}{your filename}

\item{bucket}{s3 bucket}

\item{key}{s3 object key}

\item{namespace_bucket}{boolean on whether to namespace bucket based on prod/dev environment}

\item{...}{additional parameter passed to s3 put_object}
}
\description{
Store any object to S3 (limited to <=5GB per object)
}
