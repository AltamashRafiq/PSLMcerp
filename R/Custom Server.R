#' Custom Server for Shiny Dashboard
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
custom_server = function(input, output, session) {
  ####################### TAB 1 #######################
  #### Setting Sidebar Select Inputs ####
  observeEvent(input$data_type,{
    updateSelectInput(session, 'gen_query', choices = type_pull(input$data_type, "Type"))})
  observeEvent(input$gen_query,{
    updateSelectInput(session, 'req_info', choices = type_pull(input$gen_query, "Full"))})

  #### Main Plot ####
  df = eventReactive(input$req_info, {
    filter_chooser(data=data_chooser(input$data_type), prov = TRUE, dist = FALSE, region = FALSE, info_parser(input$req_info))
  })

  output$plotter = renderHighchart({
    t = typeof(df()[[info_parser(input$req_info)]])
    if(t == "character"){funky_plot_chr(df(), info_parser(input$req_info))}
    else{funky_plot_dbl(df(), info_parser(input$req_info),
                        zero_omit = input$select_zero)}
  })

  ####################### TAB 2 #######################
  #### Setting Sidebar Select Inputs ####
  observeEvent(input$data_type_t2,{
    updateSelectInput(session, 'gen_query_t2', choices = type_pull(input$data_type_t2, "Type"))})
  observeEvent(input$gen_query_t2,{
    updateSelectInput(session, 'req_info_t2', choices = type_pull(input$gen_query_t2, "Full"))})

  #### Main Table ####
  df_t2 = eventReactive(input$req_info_t2, {
    filter_chooser(data=data_chooser(input$data_type_t2), prov = TRUE, dist = FALSE, region = FALSE, info_parser(input$req_info_t2))
  })

  #### Summary Statistics ####
  output$table_sum = renderTable({summary_maker(df_t2(), info_parser(input$req_info_t2), zero_omit = input$select_zero_t2)})
  output$download_sum <- downloadHandler(
    filename = function() {
      paste(input$data_type_t2, "_", info_parser(input$req_info_t2), "_",
            input$req_info_t2, "_", "summary", ".csv", sep = "")},
    content = function(file) {
      write.csv(summary_maker(df_t2(), info_parser(input$req_info_t2)),
                file, row.names = FALSE)})
  #### Raw Data ####
  output$download_raw <- downloadHandler(
    filename = function() {
      paste(input$data_type_t2, "_", info_parser(input$req_info_t2), "_",
            input$req_info_t2, "_", "raw", ".csv", sep = "")},
    content = function(file) {
      write.csv(df_t2(), file, row.names = FALSE)})
}
