#' generate vector of lines to be stores in a file
#'
#' @export
#'
#' @param header header object created by \link{ofsaa_header} function
#' @param data data object created by \link{ofsaa_data} function
#' @param footer footer object created by \link{ofsaa_footer} function
ofsaa_data_object <- function(header, data, footer) {

  col_sep <- ";"
  dec_del <- ","

  serializeHeader <- function() {
    paste(
      "0",
      header$fileset,
      header$csa_id,
      header$csa_name,
      header$version,
      header$business_date,
      header$business_time,
      "",
      sep = col_sep
    )
  }

  serializeData <- function() {
    readydata <- data %>%
      mutate(REC_TYPE = 2) %>% rename(`1` = REC_TYPE)
    captured_output <- capture.output(
      write.table(
        x = readydata, col.names = T, sep = col_sep, na = "", dec = dec_del,
        quote = F, row.names = F, fileEncoding = "UTF-8")
    )
    paste0(captured_output, col_sep)
  }

  serializeFooter <- function() {
    paste(
      "9",
      footer$nrow,
      "",
      sep = col_sep)
  }

  c(serializeHeader(), serializeData(), serializeFooter())

}

#' Create OFSAA header object
#'
#' Function creates header object which then can be serialized (written as file)
#'
#' @export
#'
#' @param fileset File Set code
#' @param csa_id CSA Table ID (like 1018, 1006, etc)
#' @param csa_name SCA Table Name (like STG_LOAN_CONTRACTS)
#' @param version version
#' @param business_date business date in YYYYMMDD text format
#' @param business_time in HH24miss format
ofsaa_header <- function(fileset, csa_id, csa_name, version, business_date, business_time) {
  list(
    fileset = fileset,
    csa_id = csa_id,
    csa_name = csa_name,
    version = version,
    business_date = business_date,
    business_time = business_time
  )
}

#' Create OFSAA data object
#'
#' Expand and arrange columns into specific order. Function takes dataframe and desired list of columns and returns dataframe
#' with this columns arranged accordingly if column present and original
#' dataframe and missing in columns than it will be dropped. If column is
#' missing in original dataframe and present in columns than new column with NA
#' values will be generated.
#'
#' @export
#'
#' @param dataframe dataframe to be arranged
#' @param columns vector of column names that defines what columns should
#'   present in the result dataframe and in which order should they appear
ofsaa_data <- function(dataframe, columns = NULL) {
  if (is.null(columns)) {
    dataframe
  } else {
    cols <- dataframe %>% colnames

    new_cols <- columns[!(columns %in% cols)]
    addColumn <- function(data, column) {
      data[column] <- NA
      data
    }
    expanded <-
      new_cols %>%
      Reduce(x = ., f = addColumn, init = dataframe) %>%
      subset(select = columns)
  }

}


#' Create OFSAA footer object
#'
#' Function creates footer object which then can be serialized (written as file)
#'
#' @export
#'
#' @param nrow number of rows to be reported in footer (actually number of rows
#'   in body dataframe)
ofsaa_footer <- function(nrow) {
  list(nrow = nrow)
}
