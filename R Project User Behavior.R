# Load required libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(scales)
library(dplyr)


# Function to load and clean data from CSV
data <- read.csv("C:/Users/hamza/Downloads/user_behavior_dataset.csv")

# Rename columns 
data = data %>%
  rename(
    User_ID = 'User.ID',
    Device_Model = 'Device.Model',
    User_ID = 'User.ID',
    Operating_System = 'Operating.System',
    App_Usage_Time = 'App.Usage.Time..min.day.',
    Screen_On_Time = 'Screen.On.Time..hours.day.',
    Battery_Drain = 'Battery.Drain..mAh.day.',
    Number_Apps_Installed = 'Number.of.Apps.Installed',
    Data_Usage = 'Data.Usage..MB.day.',
    Age = 'Age',
    Gender = 'Gender'
  )


# Clean and transform data
data = data %>% 
  mutate(
    is_iOS = ifelse(Operating_System == "iOS", 1, 0),
    Battery_Drain_per_hour = Battery_Drain / 24,  
    # Convert to hourly rate
    App_Usage_Time_hours = App_Usage_Time / 60,   
    # Convert minutes to hours
    Data_Usage_GB = Data_Usage / 1024,         
    # Convert MB to GB
    Age_Group = cut(Age, breaks = c(0, 20, 30, 40, 50, 60, Inf),
                    labels = c("0-20", "21-30", "31-40", "41-50", "51-60", "60+"))
  ) 


# Plot 1: Battery Drain by Operating System

ggplot(data, aes(x = Operating_System, y = Battery_Drain)) +
  geom_boxplot(aes(fill = Operating_System)) +
  geom_jitter(width = 0.2, alpha = 0.2) +
  scale_fill_manual(values = c("#4287f5", "#f54242")) +
  labs(title = "Battery Drain Distribution by Operating System",
       x = "Operating System",
       y = "Battery Drain (mAh/day)") +
  theme_minimal() +
  theme(legend.position = "none")


# Plot 2: Screen Time vs Battery Drain
ggplot(data, aes(x = Screen_On_Time, y = Battery_Drain, color = Operating_System)) +
  geom_point(aes(shape = Device_Model), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("#4287f5", "#f54242")) +
  labs(title = "Screen Time vs Battery Drain",
       x = "Screen Time (hours/day)",
       y = "Battery Drain (mAh/day)") +
  theme_minimal() +
  theme(legend.position = "right")


# Plot 3: App Usage by Age Group and Gender
ggplot(data, aes(x = Age_Group, y = App_Usage_Time_hours, fill = Gender)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "App Usage by Age Group and Gender",
       x = "Age Group",
       y = "App Usage Time (hours/day)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot 4: Data Usage by Device Model
# First calculate averages
device_data <- data %>%
  group_by(Device_Model) %>%
  summarise(
    Avg_Data_Usage = mean(Data_Usage),
    Count = n()
  )

# Create plot 4
ggplot(device_data, aes(x = reorder(Device_Model, -Avg_Data_Usage), y = Avg_Data_Usage)) +
  geom_bar(stat = "identity", aes(fill = Count)) +
  scale_fill_gradient(low = "#69b3a2", high = "#2ecc71") +
  labs(title = "Average Data Usage by Device Model",
       x = "Device Model",
       y = "Average Data Usage (MB/day)",
       fill = "Number of Users") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Statistical Analysis Section

# Summary statistics by Operating System
summary_stats <- data %>%
  group_by(Operating_System) %>%
  summarise(
    Avg_Battery_Drain = mean(Battery_Drain),
    Avg_Screen_Time = mean(Screen_On_Time),
    Avg_App_Usage = mean(App_Usage_Time),
    Avg_Data_Usage = mean(Data_Usage),
    Number_of_Users = n()
  )

# Regression analysis
model <- lm(Battery_Drain ~ Screen_On_Time + App_Usage_Time + Data_Usage + is_iOS + 
              Number_Apps_Installed, data = data)



# Device comparison
device_stats <- data %>%
  group_by(Device_Model) %>%
  summarise(
    Avg_Battery_Drain = mean(Battery_Drain),
    Avg_Screen_Time = mean(Screen_On_Time),
    Avg_Data_Usage = mean(Data_Usage),
    User_Count = n()
  ) %>%
  arrange(desc(Avg_Battery_Drain))

# Save results
write.csv(summary_stats, "analysis_output/summary_statistics.csv")
write.csv(device_stats, "analysis_output/device_comparison.csv")