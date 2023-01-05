#' Wrapper function to retrieving object into time-series dataa
#' @param bucket s3 bucket
#' @param key object key
#' @param namespace_bucket boolean for bucket namespacing, set to FALSE to override namespace
#' @param date_range:  date range min/max of date partition
#'
#' @import data.table
#'
#' @return tibble dataframe with run-date index
#' @export
aws_get_table_ts <-  function(bucket,
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
          objs <- aws_get_object(bucket, iter_key)
          fread(objs$file_path)})) %>%
      dplyr::select(data) %>%
      tidyr::unnest(data)
    # return(hist_data)
  }, error = function(e){
    stop(e$message)
  })
}

#' Wrapper function to retrieving object into table format data
#' @inheritParams aws_get_object
#' @import data.table
#'
#' @return tibble data-frame
#' @export
aws_get_table <- function(bucket,
                          key,
                          namespace_bucket = TRUE,
                          ...){
  object_metadata <- aws_get_object(bucket, key, ...)
  table <- fread(object_metadata$file_path)
  return(table)
}
