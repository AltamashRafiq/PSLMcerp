#' Tab 1 User Interface
#'
#' @param data_input
#'
#' @return
#' @export
#'
#' @examples
tab1 = function(data_input){
  output = fluidPage(
    sidebarPanel(
      selectInput("data_type", label = "Data Type", choices = data_input),
      selectInput("gen_query", label = "General Query", choices = c()),
      selectInput("req_info", label = "Request Information", choices = c()),
      radioButtons("select_zero", "Include Zero Values",
                   c("No" = TRUE, "Yes" = FALSE), inline=T),
      actionButton("action", "Button"),
      actionButton("action2", "Button2", class = "btn-primary")
    ),
    mainPanel(
      highchartOutput("plotter")
    )
  )
  return(output)
}
