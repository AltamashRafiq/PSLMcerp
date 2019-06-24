#' Type Pull - Developer Use Only
#'
#' @param input
#' @param col
#'
#' @return
#' @export
#'
#' @examples
type_pull = function(input, col){
  if(col == "Type"){
    output = codes() %>%
      filter(File == input) %>% na.omit() %>%
      pull("Type") %>% unique()
  }
  else if (col == "Full"){
    output = codes() %>%
      filter(Type == input) %>% pull("Full")
  }
  return(output)
}

#' Real Data Input - Default Set
#'
#' @return
#' @export
#'
#'@import PSLMdata
#' @examples
data_real = function(){
  return(list(PSLMdata::sec_c, PSLMdata::sec_b, PSLMdata::sec_d,
              PSLMdata::sec_e, PSLMdata::sec_f1, PSLMdata::sec_f2,
              PSLMdata::sec_g, PSLMdata::sec_h, PSLMdata::sec_i, PSLMdata::sec_j))
}

#' Input Desired PSLM Coding
#'
#' @return
#' @export
#'
#' @examples
codes = function(){
  return(PSLMdata::pslm_codes)
}

#' Data Input - For Use in Multiple Functions
#'
#' @return
#' @export
#'
#' @examples
data_input = function(){
  return(c("Education", "Household", "Health", "Employment", "Land/Animals", "Assets/Possessions", "Housing", "Vaccination & Diarrhoea", "Married Women", "Public Benefits/Services"))
}

#' Data Chooser Function - Developer Use Only
#'
#' @param data_query
#'
#' @return
#' @export
#'
#' @examples
data_chooser = function(data_query){
  return(data_real()[[which(data_input() %in% data_query)]])
}

#' Filter Chooser Function - Developer Use Only
#'
#' @param data
#' @param prov
#' @param dist
#' @param region
#' @param element
#'
#' @return
#' @export
#'
#' @examples
filter_chooser = function(data, prov = FALSE, dist = FALSE, region = FALSE, element){
  t = typeof(data[[element]])
  if(t == "character"){
    output = general_chr_filter(data=data, prov = prov, dist = dist, region = region, element)
  }
  else{
    output = general_num_filter(data=data, prov = prov, dist = dist, region = region, element)
  }
  return(output)
}

#' Info Parser Function - Developer Use Only
#'
#' @param req_info
#'
#' @return
#' @export
#'
#' @examples
info_parser = function(req_info){
  output = codes() %>%
    filter(Full == req_info) %>%
    pull("Code")
  return(output)
}

#' Summary Maker Function - Developer Use Only
#'
#' @param main_df
#' @param column_of_choice
#' @param zero_omit
#'
#' @return
#' @export
#'
#'@import tidyr
#' @examples
summary_maker = function(main_df, column_of_choice, zero_omit = TRUE){
  legend = typeof(main_df[[column_of_choice]])
  if (legend == "character"){
    summary = main_df %>% spread(key = column_of_choice, value = "Count")
  }
  else{
    if(zero_omit == TRUE){
      main_df = main_df %>% filter(get(column_of_choice) != 0)
    }
    summary = main_df %>%
      group_by_(.dots = names(main_df)[-ncol(main_df)]) %>%
      summarize("Count" = n(),
                "Mean" = mean(get(column_of_choice)),
                "Minimum Value" = min(get(column_of_choice)),
                "Tenth Percentile" = quantile(get(column_of_choice), 0.10),
                "First Quartile" = quantile(get(column_of_choice), 0.25),
                "Median" = median(get(column_of_choice)),
                "Third Quartile" = quantile(get(column_of_choice), 0.75),
                "Ninetieth Percentile" = quantile(get(column_of_choice), 0.90),
                "Maximum Value" = max(get(column_of_choice)))
  }
  return(summary)
}
