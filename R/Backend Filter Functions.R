#' Character Column Interpreter
#'
#' @param data
#' @param prov
#' @param district
#' @param region
#' @param element
#'
#' @return
#'
#'@import dplyr
#' @examples
general_chr_filter = function(data, prov = FALSE, district = FALSE, region = FALSE, element){
  logicals = c(prov, district, region)
  fields = c("province", "district", "region")
  col = c(fields[logicals], element)
  output = data %>% select(col) %>%
    drop_na() %>% # Dropping rows containing NAs
    group_by_(.dots = col) %>% summarize(Count = n())
  return(output)
}

#' Numeric Column Interpreter
#'
#' @param data
#' @param prov
#' @param district
#' @param region
#' @param element
#'
#' @export
#'
#' @examples
general_num_filter = function(data, prov = FALSE, district = FALSE, region = FALSE, element){
  logicals = c(prov, district, region)
  fields = c("province", "district", "region")
  col = c(fields[logicals], element)
  output = data %>% select(col) %>%
    drop_na() %>% # Dropping rows containing NAs
    group_by_(.dots = col[-length(col)])
  return(output)
}
