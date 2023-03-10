% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aws_s3_get.R
\name{aws_s3_bulk_get}
\alias{aws_s3_bulk_get}
\title{Bulk Get Object in DataBrew S3}
\usage{
aws_s3_bulk_get(
  bucket,
  namespace_bucket = TRUE,
  build_metadata = TRUE,
  output_dir = NULL,
  ...
)
}
\arguments{
\item{bucket}{s3 bucket}

\item{namespace_bucket}{boolean to create namespace bucket, set to FALSE to override bucket namespace}

\item{build_metadata}{boolean to create metadata of bulk object retrieval from S3}

\item{output_dir}{output directory from aws_s3_get parameter}

\item{...}{additional parameter from S3 get object}
}
\value{
list of object s3 metadata and file location
}
\description{
Bulk Get Object in DataBrew S3
}
