# Information about the data
summary <- function(dataset) {
  output <- list()
  output$participants <- nrow(dataset)
  output$freshmen <- nrow(dataset %>% filter(class_standing == "Freshman"))
  output$sophomore <- nrow(dataset %>% filter(class_standing == "Sophomore"))
  output$junior <- nrow(dataset %>% filter(class_standing == "Junior"))
  output$senior <- nrow(dataset %>% filter(class_standing == "Senior"))
  output$info_interest <- nrow(dataset %>% filter(info_interest == "Yes"))
  output$windows <- nrow(dataset %>% filter(operating_system == "Windows"))
  output$mac <- nrow(dataset %>% filter(operating_system == "Mac"))
  output$never_used_term <- nrow(dataset %>% filter(command_line_terminal == "Never used it"))
  output$no_experience_percent <- nrow(dataset %>% filter(command_line_terminal == "Never used it") %>% 
                                         filter(familiarity_git_for_vc == "Never used it") %>% 
                                         filter(familiarity_md == "Never used it") %>% 
                                         filter(familiarity_r == "Never used it")) / output$participants
  output$ave_countries_visited <- mean(dataset$countries_visited)
  hawks_fans <- grepl("yes", dataset$go_hawks, ignore.case = TRUE)
  output$hawks_fans <- sum(hawks_fans == TRUE)
  return (output)
}
