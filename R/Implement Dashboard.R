#' Open PSLM Dashboard
#'
#' @param tab1
#' @param tab2
#'
#' @return
#' @export
#'
#'@import shiny
#'@import shinythemes
#' @examples
implement_dashboard = function(tab1, tab2){
  shinyApp(
    ui = navbarPage("PSLM 2014-15 Dashboard",
                    theme = shinythemes::shinytheme("spacelab"),
                    tabPanel("Plot", tab1),
                    navbarMenu("More",
                               tabPanel("Summary Statistics", tab2),
                               tabPanel("Table", "Table tab contents..."))
    ),
    server = custom_server(input, output, session)
  )
}
