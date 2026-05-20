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

# Plot for the overall regression model/ (Overall Cohort Trend- Attitude)

ggplot(Compiled_Data, aes(x = student_attitude, y = performance)) +
  geom_point(color = "#7A8B7B", alpha = 0.5, size = 2) + # Muted sage dot to match your palette
  geom_smooth(method = "lm", se = TRUE, color = "#2D3B32", size = 1.2) + # Forest green trend line
  theme_minimal() +
  labs(title = "The Cohort Performance Trend",
       subtitle = "Cohort-wide relationship before stream stratification",
       x = "Attitude Score",
       y = "Math Performance")
# will probably plot for study habits.
# But side by side with the attitude plot. 
# Reshape data to put both factors into one clean visual block
Compiled_Data %>%
  select(performance, student_attitude, student_study_habits) %>%
  pivot_longer(cols = c(student_attitude, student_study_habits), 
               names_to = "Variable", values_to = "Score") %>%
  mutate(Variable = ifelse(Variable == "student_attitude", "Student Attitude (The Paradox)", "Study Habits (The Solution)")) %>%
  
  ggplot(aes(x = Score, y = performance, color = Variable)) +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  facet_wrap(~Variable, scales = "free_x") +
  
  scale_color_manual(values = c("#A24B43", "#2D3B32")) + # Red-brown for paradox, green for solution
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(size = 12, face = "bold")) +
  labs(title = "The Friction in Learning: Confidence vs. Execution",
       x = "Survey Score (1-5 Scale)",
       y = "Mathematics Performance")

# New model including Stream as a category
stream_model <- lm(performance ~ as.factor(stream) + student_interest + 
                     student_attitude + student_study_habits + 
                     teacher_teaching_style + 
                     teacher_expectation_and_support + 
                     teacher_availability_communication, 
                   data = Compiled_Data)

summary(stream_model)

# Plot for attitude for Model 2- adding stream as a factor
# ---The Controlled Stream Shift (stream_model) ---
# To accurately visualize the parallel lines of a factor model, we use predicted values
Compiled_Data$pred_stream <- predict(stream_model)

ggplot(Compiled_Data, aes(x = student_attitude, y = pred_stream, color = stream)) +
  geom_line(size = 1.2) + 
  theme_minimal() +
  labs(title = "Additive Model: Performance Shifts by Stream",
       subtitle = "Parallel tracking assumes attitude impact is uniform across classrooms",
       x = "Attitude Score",
       y = "Predicted Math Performance (Controlled)")

# Model 3, interaction of stream
interaction_model <- lm(performance ~ student_attitude * as.factor(stream),
                        data = Compiled_Data)

summary(interaction_model)
#To visualize the interaction for both attitude and study habits separately then path them
#install patchwork coz facetwrap might not work here

install.packages("patchwork")
library(patchwork)

library(ggplot2)
#attitude plot
plot_attitude_interaction <- ggplot(Compiled_Data, aes(x = student_attitude, y = performance, color = stream)) +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  theme_minimal() +
  labs(title = "The Attitude Paradox by Stream",
       x = "Attitude Score",
       y = "Math Performance") +
  theme(legend.position = "none") # Hides legend on the left to save space
#study habits plot
plot_habits_interaction <- ggplot(Compiled_Data, aes(x = student_study_habits, y = performance, color = stream)) +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  theme_minimal() +
  labs(title = "The Study Habits Impact by Stream",
       x = "Study Habits Score",
       y = "Math Performance")
# Glue them together side-by-side
plot_attitude_interaction + plot_habits_interaction

## previous one failed, so we try this one.

# Option A: Using gridExtra (standard in most data packages)
if(!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)
library(grid)

# 1. Re-run the Attitude Interaction Plot (Your original code)
plot_attitude_interaction <- ggplot(Compiled_Data, aes(x = student_attitude, y = performance, color = stream)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  theme_minimal() +
  labs(title = "Attitude by Stream",
       x = "Attitude Score (1-5 Scale)",
       y = "Math Performance") +
  theme(legend.position = "none") # Hide left legend to save space

# 2. Re-run the Study Habits Interaction Plot (The companion)
plot_habits_interaction <- ggplot(Compiled_Data, aes(x = student_study_habits, y = performance, color = stream)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  theme_minimal() +
  labs(title = "Study Habits by Stream",
       x = "Study Habits Score (1-5 Scale)",
       y = "") +
  theme(
  axis.text.y = element_blank(), # Removes the numbers (20, 30, 40...)
axis.ticks.y = element_blank())  # Removes the little tick marks

# 3. Use grid.arrange to glue them side-by-side
grid.arrange(
  plot_attitude_interaction, 
  plot_habits_interaction, 
  ncol = 2,
  top = textGrob(
    "The Friction in Learning: The Attitude Paradox vs. Practical Execution",
    gp = gpar(fontface = "bold", fontsize = 12, col = "#333333")
  )
)


#To diagnose the model by ploting the residuals
par(mfrow = c(2, 2)) # Sets up a 2x2 grid
plot(math_model)



