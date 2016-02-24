library(dplyr)
library(plotly)
# A function that takes a dataset as a parameter and returns a visualization chart
chart_two <- function(data) {
  
  # Creating a summary table of the totals for the different types of people
  summary_tab <- data %>% 
    summarise("Number of Mac Users" = sum(
                data$What.orperating.system.do.you.typically.use. == "Mac"),
              "Number of Windows users" = sum(
                data$What.orperating.system.do.you.typically.use. == "Windows"), 
              "Number of Windows Users interested in Informatics" = sum((data$What.orperating.system.do.you.typically.use. == "Windows")) &
                (data$Are.you.interested.in.applying.to.the.Informatics.major. == "Yes"))
              "Number of Mac Users interested in Informatics" = sum((data$What.orperating.system.do.you.typically.use. == "Mac") & 
                (data$Are.you.interested.in.applying.to.the.Informatics.major. == "Yes"))
  
  # Set up axis names
  xaxis <- list(title = 'Type of Person')
  yaxis <- list(title = 'Number of People')
  
  # Create a bar graph 
  plot_ly(summary_tab, type = 'bar', x = names(summary_tab), 
          y = as.numeric(summary_tab[1,])) %>% 
    layout(title = 'Are PC users or Mac users more interested in Informatics?', xaxis=xaxis, yaxis=yaxis)
  
}