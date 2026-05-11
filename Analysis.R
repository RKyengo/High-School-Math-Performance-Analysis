#Rename the files herefor ease of access
library(readxl)
Mathematics_Dataset <- read_excel("Mathematics Dataset.xlsx")
head(Mathematics_Dataset,sheet="Compiled Data")
Compiled_Data <- read_excel("Mathematics Dataset.xlsx",sheet="Compiled Data")
head(Compiled_Data)
# want to crate charts but I am not sure about column names. so need them cleaned
install.packages("janitor")
library(janitor)
#to clean the namesesp spelling and capital mistakes
Compiled_Data <- Compiled_Data %>% clean_names()
colnames(Compiled_Data)
#heading to creating charts now. ggplot is in tidyverse
library(tidyverse)
#to reshape the data for plots ie make it long
student_long <- Compiled_Data %>%
  select(stream, starts_with("student")) %>% # to only select student factors
  pivot_longer(cols = -stream, names_to = "factor_type", 
               values_to = "score") 
#keep stream but stack other columns 
# To create the actual charts.
# Start the 'ggplot' function
ggplot(data = student_long, aes(x = factor_type, y = score, fill = stream)) +
  
  # Add the bars. 'dodge' puts them side-by-side instead of stacked.
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  
  # Clean up the labels to make them professional
  labs(
    title = "Comparison of Student Factors by Stream",
    subtitle = "Mean Likert scores for Interest, Attitude, and Study Habits",
    x = "Student-Related Factors",
    y = "Average Score (1-5)",
    fill = "Stream Name") +
  
  # Choose a clean look for the background
  theme_dark() +
  
  # Tuning down the colors (Choosing a more professional palette)
  scale_fill_brewer(palette = "Blues") + 
  # For Y-axis nos and breaks
  scale_y_continuous(breaks = 1:5, limits = c(0, 5)) +
  
  # Bonus: Cleaning up the X-axis category names
  scale_x_discrete(labels = c("student_attitude" = "Attitude", 
                              "student_interest" = "Interest", 
                              "student_study_habits" = "Study Habits"))
library(tidyverse)
library(janitor)
library(readxl)
#reload the libaries above and the dataset.
Mathematics_Dataset<-read_excel("Mathematics Dataset.xlsx")
Compiled_Data<-read_excel("Mathematics Dataset.xlsx",sheet="Compiled Data")
colnames(Compiled_Data) # to identify columnnames
Compiled_Data <- Compiled_Data %>% clean_names() 
#added the above after stream was not found in the next code.
#need to reshape the data, ie create extra columns for ease of charting.
teacher_long <- Compiled_Data %>%
  select(stream, starts_with("Teacher")) %>% # to only select teacher-related factors
  pivot_longer(cols = -stream, names_to = "factor_type", 
               values_to = "score") 
#keep stream but stack other columns 

#the actual plot
ggplot(data = teacher_long, aes(x = factor_type, y = score, fill = stream)) +
  
  # Add the bars. 'dodge' puts them side-by-side instead of stacked.
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  
  # Clean up the labels to make them professional
  labs(
    title = "Comparison of Teacher-Related Factors by Stream",
    subtitle = "Mean Likert scores for Teaching Style, Expectations & Support, and Availability & Communication",
    x = "Teacher-Related Factors",
    y = "Average Score (1-5)",
    fill = "Stream Name") +
  
  # Choose a clean look for the background
  theme_dark() +
  
  # Tuning down the colors (Choosing a more professional palette)
  scale_fill_brewer(palette = "Greens") + 
  # For Y-axis nos and breaks
  scale_y_continuous(breaks = 1:5, limits = c(0, 5)) +
  
  # Bonus: Cleaning up the X-axis category names
  scale_x_discrete(labels = c("teacher_teaching_style" = "Style", 
                              "teacher_expectation_and_support" = "Expectation", 
                              "teacher_availability_communication" = "Availability"))
colnames(Compiled_Data)

#Trying a regression model. 
#got some errors so made corrections in excel
Compiled_Data<-read_excel("Mathematics Dataset.xlsx", sheet="Compiled Data")

Compiled_Data <- Compiled_Data %>% clean_names() 
# 1. Build the model
# The '~' means 'is predicted by' and '+' adds factors
math_model <- lm(performance ~ student_interest + student_attitude + 
                   student_study_habits + teacher_teaching_style + 
                   teacher_expectation_and_support + 
                   teacher_availability_communication, 
                 data = Compiled_Data)

# 2. See the results
summary(math_model)
# New model including Stream as a category
stream_model <- lm(performance ~ as.factor(stream) + student_interest + 
                     student_attitude + student_study_habits + 
                     teacher_teaching_style + 
                     teacher_expectation_and_support + 
                     teacher_availability_communication, 
                   data = Compiled_Data)

summary(stream_model)
interaction_model <- lm(performance ~ student_attitude * as.factor(stream),
                        data = Compiled_Data)
summary(interaction_model)
#To visualize the interaction
ggplot(Compiled_Data, aes(x = student_attitude, y = performance, color = stream)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "The Attitude Paradox by Stream",
       x = "Attitude Score",
       y = "Math Performance")

#To diagnose the model by ploting the residuals
par(mfrow = c(2, 2)) # Sets up a 2x2 grid
plot(math_model)
install.packages("mediation")
library(mediation)

