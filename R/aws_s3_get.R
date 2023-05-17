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
#' @param bucket s3 bucket name
#' @param key s3 object key / s3 URI
#' @param output_dir output directory from aws_s3_get parameter, this will be the destination if write_cache is set to `TRUE`
#' @param namespace_bucket boolean to create namespace bucket, set to FALSE to override bucket namespace
#' @param check_cache boolean, set to `TRUE` if you would like to check cache before download
#' @param write_cache boolean, set to `TRUE` if you would like to write cache after download
#' @param ... additional parameter from S3 get object
#'
#' @importFrom magrittr %>%
#'
#' @return list of object s3 metadata and file location
#' @export
aws_s3_get_object <- function(bucket,
                              key,
                              output_dir = '~/.cloudbrewr_cache',
                              namespace_bucket = TRUE,
                              check_cache = FALSE,
                              write_cache = FALSE,
                              ...){
  tryCatch({
    # authenticate to s3
    s3obj <- paws::s3()
    d <- pull_cache(cache = output_dir)

    # namespace bucket
    if(namespace_bucket){
      bucket <- aws_namespace(bucket)
    }

    # create dir if not exist
    if(!dir.exists(output_dir)){
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    }

    is_cached <- FALSE
    # check if use cache
    if(check_cache){
      msg <- glue::glue('[CLOUDBREWR_LOGS]: Checking cache for {key}')
      message(msg)
      header <- s3obj$head_object(Bucket = bucket, Key = key, ...)
      etag <- stringr::str_replace_all(header$ETag, "[[:punct:]]", "")

      # if cache is already available
      if(check_cloudbrewr_cache(etag, cache = output_dir)){
        message('[CLOUDBREWR_LOGS]: Cache hit, skipping download...')
        output <- list(
          bucket = bucket,
          object_key = key,
          content_length = header$ContentLength,
          content_type = header$ContentType,
          version_id = header$VersionId,
          last_modified = header$LastModified,
          etag = etag,
          file_path = d$get(etag))
        return(output)
      }
      msg <- glue::glue('[CLOUDBREWR_LOGS]: {key} is not found, downloading...')
      message(msg)
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
      etag = stringr::str_replace_all(metadata$ETag, "[[:punct:]]", "")
    )

    # if output dir is null save to cache
    if(is.null(output_dir)){
      file_path <- file.path(
        "~/.cloudbrewr_cache",
        basename(key)
      )
    # save to desired directory
    }else{
      file_path <- file.path(
        output_dir,
        basename(key)
      )
    }

    unlink(file_path)
    # download file using filepath
    s3obj$download_file(
      Bucket= bucket,
      Key = key,
      Filename = file_path,
      ...)
    # add filepath
    output$file_path <- file_path

    if(write_cache){
      d <- pull_cache(cache = output_dir)
      d$set(output$etag, output$file_path)
    }

  }, error = function(e){
    stop(e$message)
  })
  return(output)
}


#' Bulk Get Object in DataBrew S3
#' @param bucket s3 bucket
#' @param prefix folder prefix
#' @param namespace_bucket boolean to create namespace bucket, set to FALSE to override bucket namespace
#' @param output_dir output directory from aws_s3_get parameter, this will be the destination if write_cache is set to `TRUE`
#' @param ... additional parameter from S3 get object
#'
#' @importFrom magrittr %>%
#'
#' @return list of object s3 metadata and file location
#' @export
aws_s3_bulk_get <- function(bucket,
                            prefix = NULL,
                            namespace_bucket = TRUE,
                            output_dir = '~/.cloudbrewr_cache',
                            ...){
    # authenticate to s3
    s3obj <- paws::s3()

    # name space bucket
    if(namespace_bucket){
      bucket <- aws_namespace(bucket)
    }

    # create dir if not exist
    if(!dir.exists(output_dir)){
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    }

    if(!is.null(prefix)){
      bucket_args <- glue::glue('s3://{bucket}{prefix} {output_dir}')
    }else{
      bucket_args <- glue::glue('s3://{bucket} {output_dir}')
    }

    # running aws s3 sync
    tryCatch({
      logger::log_info('Running bulk download')
      system2(command = "aws", args = c('s3 sync',  bucket_args))
    }, error = function(e){
      logger::log_error(e$message)
      stop(e$message)
    })
}

#' Get Metadata Headers
#' @param bucket s3 bucket
#' @param key s3 object key
#' @param namespace_bucket boolean to create namespace bucket, set to FALSE to override bucket namespace
#' @param ... additional parameter from S3 get object
#'
#' @importFrom magrittr %>%
#'
#' @return list of object s3 metadata and file location
#' @export
aws_s3_get_header <- function(bucket,
                              key,
                              namespace_bucket = TRUE){
  tryCatch({
    # authenticate to s3
    s3obj <- paws::s3()
    if(namespace_bucket){
      bucket <- aws_namespace(bucket)
    }
    s3obj$head_object(
      Bucket = bucket,
      Key = key) %>%
      .$Metadata %>%
      tibble::as_tibble() %>%
      dplyr::mutate(key = key,
                    bucket = bucket)

  }, error = function(e){
    stop(e$message)
  })

}
