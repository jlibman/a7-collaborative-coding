# A function that takes a dataset as a parameter and returns a visualization chart
chart_one <- function(data) {
  
  # Creating a summary table of the totals for the different types of people
  summary_tab <- data %>% 
    summarise("A Dog Person" = sum(data$Do.you.consider.yourself. == "A dog person..."), 
              "A Cat Person" = sum(data$Do.you.consider.yourself. == "A cat person...."),
              "Both!" = sum(data$Do.you.consider.yourself. == "Both!"), 
              "Neither" = sum(data$Do.you.consider.yourself. == "Neither"))
  
  # Set up axis names
  xaxis <- list(title = 'Type of Person')
  yaxis <- list(title = 'Number of People')
  
  # Create a bar graph 
  plot_ly(summary_tab, type = 'bar', x = names(summary_tab), 
          y = as.numeric(summary_tab[1,])) %>% 
    layout(title = 'What type of person is our class?', xaxis=xaxis, yaxis=yaxis)
  
}