#' #' this is still in development
#'
#' #' Multipart upload function to split parts
#' split_equal_parts <- function(data, num_groups){
#'   data %>%
#'     group_by((row_number()-1) %/% (n()/num_groups)) %>%
#'     nest() %>%
#'     pull(data)
#' }
#'
#' aws_multipart_upload <- function(filename, bucket, key, partsize = 10e7){
#'   file_size <- file.size(filename)
#'   num_chunks <- as.integer(file_size / partsize)
#'   file_connection <- file(filename)
#'   open(con = file_connection, open = 'rb')
#'   body <- readBin(file_connection,
#'                   what = 'character',
#'                   n = 5)
#'
#'   part_numbers_list <- seq(1:length(body))
#'
#'   key <- 'test_multipart/a.csv'
#'   multipart_upload <- s3obj$create_multipart_upload(
#'     Bucket = bucket,
#'     ContentType = mime::guess_type(key),
#'     Key=key
#'   )
#'   parts <- purrr::map2(body, part_numbers_list, function(chunk_body, part_number){
#'     uploadPart <- s3obj$upload_part(
#'       Body = chunk_body,
#'       Bucket = bucket,
#'       Key = key,
#'       PartNumber = part_number,
#'       UploadId = multipart_upload$UploadId
#'     )
#'     list(ETag = uploadPart$ETag,
#'          PartNumber = part_number)
#'   })
#'   complete_multipart <- s3obj$complete_multipart_upload(
#'     Bucket = bucket,
#'     Key = key,
#'     UploadId = multipart_upload$UploadId,
#'     MultipartUpload = list(
#'       Parts = list(
#'         list(ETag = "\"865c0c0b4ab0e063e5caa3387c1a8741\"",
#'              PartNumber = 1)
#'       )
#'     )
#'   )
#' }
