#' Get DataBrew S3 object catalog
#' @description Get catalog of files used in S3
#'
#' @inheritParams aws_s3_get_object
#'
#' @importFrom magrittr %>%
#'
#' @return tibble of S3 file catalog
#' @export
aws_s3_get_catalog <- function(bucket,
                               namespace_bucket = TRUE,
                               ...){
  tryCatch({
    s3obj <- paws::s3()
    if(namespace_bucket){
      bucket <- aws_namespace(bucket)
    }
    s3obj$list_objects_v2(Bucket = bucket, ...) %>%
      .$Contents %>%
      purrr::map_dfr(~.x) %>%
      dplyr::mutate(folder = dirname(Key)) %>%
      tidyr::separate(folder,
                      c("project",
                        "pipeline_folder",
                        "pipeline_subfolder"),
                      sep = "/")
  }, error = function(e){
    stop(e$message)
  })
}

#' Get object version history in s3
#' @description Get object version change history
#'
#' @inheritParams aws_s3_get_object
#'
#' @importFrom magrittr %>%
#'
#' @return tibble of S3 object version history
#' @export
aws_s3_get_object_version_history <- function(bucket,
                                              key,
                                              namespace_bucket = TRUE,
                                              ...){
  tryCatch({
    # authenticate to s3
    s3obj <- paws::s3()
    if(namespace_bucket){
      bucket <- aws_namespace(bucket)
    }
    list_obj_history <- s3obj$list_object_versions(
      Bucket = bucket,
      Prefix = key, ...) %>%
      .$Versions
    # get metadata
    version_history <- list_obj_history %>%
      purrr::map_dfr(~.x) %>%
      dplyr::distinct(VersionId, .keep_all = TRUE)
    return(version_history)
  }, error = function(e){
    stop(e$message)
  })
}

#' Get Object in DataBrew S3
#' @param bucket s3 bucket
#' @param key s3 object key
#' @param namespace_bucket boolean to create namespace bucket, set to FALSE to override bucket namespace
#' @param ... additional parameter from S3 get object
#'
#' @importFrom magrittr %>%
#'
#' @return list of object s3 metadata and file location
#' @export
aws_s3_get_object <- function(bucket,
                              key,
                              namespace_bucket = TRUE,
                              ...){
  tryCatch({
    # authenticate to s3
    s3obj <- paws::s3()
    if(namespace_bucket){
      bucket <- aws_namespace(bucket)
    }

    # get metadata
    metadata <- s3obj$get_object(
      Bucket = bucket, Key = key, ...)

    # output metadata
    output <- list(
      bucket = bucket,
      object_key = key,
      last_modified = metadata$LastModified,
      content_length = metadata$ContentLength,
      version_id = metadata$VersionId,
      content_type = metadata$ContentType,
      etag = metadata$ETag
    )

    # save to tempfile
    file_path <- tempfile(
      basename(dirname(key)),
      fileext = paste0(".", tools::file_ext(key)))
    s3obj$download_file(
      Bucket= bucket,
      Key = key,
      Filename = file_path,
      ...)

    # add filepath
    output$file_path <- file_path
  }, error = function(e){
    stop(e$message)
  })
  return(output)
}
