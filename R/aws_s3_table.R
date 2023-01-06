#' Function to retrieve S3 folder-partitioned table into time-series data
#' @description Wrapper to bind folder-partitioned table into time-series, indexed by run_date
#' @param bucket s3 bucket
#' @param key object key
#' @param namespace_bucket boolean for bucket namespacing, set to FALSE to override namespace
#' @param ... argument on s3 api on list_objects_v2
#' @param date_range date range min/max of date partition
#' @return tibble dataframe with run-date index
#' @export
aws_s3_get_table_ts <-  function(bucket,
                                 key,
                                 namespace_bucket = TRUE,
                                 date_range = NULL,
                                 ...){
  tryCatch({
    # authenticate to s3
    s3obj <- paws::s3()
    if(namespace_bucket){
      bucket <- aws_namespace(bucket)
    }

    # get time series
    ts_obj <- s3obj$list_objects_v2(
      Bucket = bucket,
      Prefix = key, ...) %>%
      .$Contents %>%
      purrr::map_dfr(~.x) %>%
      dplyr::mutate(run_date = basename(dirname(Key))) %>%
      tidyr::separate(run_date, c("tmp", "run_date"), "=") %>%
      dplyr::mutate(run_date = lubridate::as_date(run_date)) %>%
      distinct(run_date, .keep_all = TRUE)

    # filter
    if(!is.null(date_range)){
      ts_obj <- ts_obj %>%
        dplyr::filter(run_date >= date_range[1],
                      run_date <= date_range[2])
    }
    ts_obj %>%
      tidyr::drop_na(run_date) %>%
      dplyr::mutate(
        data = purrr::map(Key, function(iter_key){
          objs <- aws_s3_get_table(bucket, iter_key)
          })
        ) %>%
      dplyr::select(data) %>%
      tidyr::unnest(data)
    # return(hist_data)
  }, error = function(e){
    stop(e$message)
  })
}

#' Function to retrieve s3 object directly into table
#' @description wrapper function to transform s3 into tibble
#' @inheritParams aws_s3_get_object
#' @importFrom magrittr %>%
#' @importFrom data.table fread
#' @return tibble data-frame of s3 object
#' @export
aws_s3_get_table <- function(bucket,
                             key,
                             namespace_bucket = TRUE,
                             ...){
  object_metadata <- aws_s3_get_object(bucket, key, ...)
  table <- fread(object_metadata$file_path) %>%
    tibble::as_tibble()
  return(table)
}
