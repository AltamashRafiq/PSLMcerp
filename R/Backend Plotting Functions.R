#' Highcharts Plot for Numeric Columns
#'
#' @param df
#' @param column_of_interest
#' @param zero_omit
#' @param out
#'
#' @return
#' @export
#'
#'@import highcharter
#'@importFrom glue glue
#'@importFrom htmlwidgets JS
#' @examples
funky_plot_dbl = function(df, column_of_interest, zero_omit = TRUE, out = FALSE){
  my_palette = c('#31688EFF', '#DC143C', '#35B779FF', '#FF8F00') %>% sample()
  if (zero_omit == TRUE){
    df = df %>% filter(get(column_of_interest) != 0)
  }
  basic = hcboxplot(x = df[[column_of_interest]], var = df[["province"]], outliers = out) %>%
    hc_chart(type = "column") %>%
    #hc_tooltip(pointFormat = 'Maximum: {point.high}<br/>Upper Quartile: {point.q3}<br/>Median: {point.median}<br/>Lower Quartile: {point.q1}<br/>Minimum: {point.low}<br/>Observations: {point.obs}') %>%
    hc_chart(type = "column", events = list(
      load = JS(glue::glue("function() {{
                           var chart = this;
                           chart.series[0].points[0].update({{color: '{my_palette[1]}'}})
                           chart.series[0].points[1].update({{color: '{my_palette[2]}'}})
                           chart.series[0].points[2].update({{color: '{my_palette[3]}'}})
                           chart.series[0].points[3].update({{color: '{my_palette[4]}'}})}")
      )))
  return(basic)
}

#' Highcharts Plot for Character Columns
#'
#' @param df
#' @param column_of_interest
#'
#' @return
#' @export
#'
#' @examples
funky_plot_chr = function(df, column_of_interest){
  interest = df[[column_of_interest]] %>% unique()
  if (length(interest) <= 30){
    df = df %>% spread(key = column_of_interest, value = "Count")
    my_list = list()
    for (i in seq_along(interest)){
      my_list[[i]] = list(name=interest[i], data = df[[interest[i]]])
    }
    basic = highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "MyGraph") %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = TRUE),
        stacking = "normal",
        enableMouseTracking = TRUE)) %>%
      hc_xAxis(categories = paste(df[["province"]], df[["region"]],
                                  sep=" ")) %>%
      hc_add_series_list(my_list)
  }
  else{
    basic = "Currently Nothing Here"
    # basic = hchart(df, "column",
    #                hcaes(x = "Province", y = "Count",
    #                      group = column_of_interest))
  }
  return(basic)
}
