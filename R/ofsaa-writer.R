#' write OFSAA data object into file
#'
#' @export
#'
#' @param data_object object produced by \link{ofsaa_data_object} function
#' @param file filename to store data
write_ofsaa <- function(data_object, file) {
  writeLines(text = data_object, con = file)
}


