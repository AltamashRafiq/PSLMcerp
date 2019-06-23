#' Tab 2 User Interface
#'
#' @param data_input
#'
#' @return
#' @export
#'
#' @examples
tab2 = function(data_input){
  output = fluidPage(
    sidebarPanel(
      selectInput("data_type_t2", label = "Data Type", choices = data_input),
      selectInput("gen_query_t2", label = "General Query", choices = c()),
      selectInput("req_info_t2", label = "Request Information", choices = c()),
      radioButtons("select_zero_t2", "Include Zero Values", c("No" = TRUE, "Yes" = FALSE), inline=T),
      downloadButton("download_sum", "Summary Download"), h3(" "),
      downloadButton("download_raw", "Raw Data Download")
    ),
    mainPanel(
      h2("Summary Statistics"),
      tableOutput("table_sum")
    )
  )
  return(output)
}
