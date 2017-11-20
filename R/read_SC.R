
#' Read Raw ballot data (EL155)
#'
#' Read raw SC EL155 form
#'
#' @param path  path to raw EL155 file
#' @param path name of county of the file


read_EL155 <- function(path = "build/input/SC_2010/Allendale/EL155", cname = "Allendale") {
  raw <-  read_file(path)
}
