#install.packages(dplyr)
require(dplyr)
# Read in data 
data <- read.csv("https://raw.githubusercontent.com/INFO-498F/a7-collaborative-coding/master/data/intro_survey_data.csv?token=AP1NbC6w-vFziH4uo-eMA9wqFp2X73pjks5W1Mc8wA%3D%3D  ")

# Rename columns
names(data)[names(data)=="What.is.your.current.class.standing."] <- "class_standing"
names(data)[names(data)=="Are.you.interested.in.applying.to.the.Informatics.major."] <- "info_interest"
names(data)[names(data)=="What.operating.system.do.you.typically.use."] <- "operating_system"
names(data)[names(data)=="Using.the.command.line...terminal"] <- "command_line_terminal"
names(data)[names(data)=="What.is.your.familiarity.with..using.git.for.version.control"] <- "familiarity_git_for_vc"
names(data)[names(data)=="What.is.your.familiarity.with..Creating.documents.with.Markdown"] <- "familiarity_md"
names(data)[names(data)=="What.is.your.familiarity.with..Using.the.R.programming.language"] <- "familiarity_r"
names(data)[names(data)=="What.is.your.programming.experience."] <- "programming_exp"
names(data)[names(data)=="How.many.countries.have.you.visited.in.your.life."] <- "countries_visited"
names(data)[names(data)=="Do.you.consider.yourself."] <- "dog_or_cat_person"
names(data)[names(data)=="Are.you.a.Seahawks.fan."] <- "go_hawks"

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
  #output$ave_countries_visited <-
  return (output)
}
