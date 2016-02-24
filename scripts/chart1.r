# A function that takes a dataset as a parameter and returns a visualization chart
chart_one <- function(dataset) {
  
  # Creating a summary table of the totals for the different types of people
  summary_tab <- data %>% 
    summarise("A Dog Person" = sum(data$dog_or_cat_person == "A dog person..."), 
              "A Cat Person" = sum(data$dog_or_cat_person == "A cat person...."),
              "Both!" = sum(data$dog_or_cat_person == "Both!"), 
              "Neither" = sum(data$dog_or_cat_person == "Neither"))
  
  # Set up axis names
  xaxis <- list(title = 'Type of Person')
  yaxis <- list(title = 'Number of People')
  
  # Create and return a bar graph 
  plot_ly(summary_tab, type = 'bar', x = names(summary_tab), 
          y = as.numeric(summary_tab[1,])) %>% 
    layout(title = 'What type of people are in our class?', xaxis=xaxis, yaxis=yaxis) %>% 
    return()
  
}