# A function that takes a dataset as a parameter and returns a visualization chart
chart_two <- function(dataset) {
  
  # Creating a summary table of the totals for the different types of people
  summary_tab <- dataset %>% 
    summarise("Number of Mac Users" = sum(operating_system == "Mac"),
              "Number of Windows users" = sum(operating_system == "Windows"), 
              "Number of Mac Users interested in Info" = 
                sum((operating_system == "Mac") & (info_interest == "Yes")),
              "Number of Windows Users interested in Info" = 
                sum((operating_system == "Windows") & (info_interest == "Yes"))
    )
              
  # Set up axis names and fonts
  xaxis <- list(title = 'Type of Person', tickangle = 8, tickfont = list(size = 9))
  yaxis <- list(title = 'Number of People')
  
  # Create a bar graph 
  plot_ly(summary_tab, type = 'bar', x = names(summary_tab), 
          y = as.numeric(summary_tab[1,])) %>% 
    layout(title = 'Are PC users or Mac users more interested in Informatics?', xaxis=xaxis, yaxis=yaxis)
}