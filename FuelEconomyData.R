# Uncomment the following line if you need to install the packagea
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("dplyr")

# 1. Load necessary libraries
library(readr)  # For reading and writing CSV files
library(ggplot2)
library(dplyr)

# 2. Read the CSV file
# Specify the path to your CSV file
file_path <- "vehicles.csv"
vehicle <- read_csv(file_path)
df <- vehicle[, c("make", "model","year", "cylinders","trany", "fuelType", 
                  "fuelType1", "range", "rangeCity", "rangeHwy","UCity", "UHighway")]

# Summary statistics for numerical columns
summary(df)

# Summary of categorical columns
table(df$make)
table(df$trany)
table(df$fuelType)

# Average city fuel efficiency
avg_UCity <- mean(df$UCity)
cat("Average City Fuel Efficiency:", avg_UCity, "mpg\n")

# Average highway fuel efficiency
avg_UHighway <- mean(df$UHighway)
cat("Average Highway Fuel Efficiency:", avg_UHighway, "mpg\n")

unique(df$cylinders)
sort(unique(df$year))

# Filtering data , excluding Electric cars
df_filtered <- df %>% filter(fuelType != "Electricity",
                             fuelType != "Premium and Electricity", 
                             year != 2025, 
                             UCity <= 50, 
                             UHighway <= 50)
df_sample <- df_filtered %>% sample_frac(0.005)



# Scatter plot of city vs. highway fuel efficiency
ggplot(df_sample, aes(x = UCity, y = UHighway, color = make)) + 
  geom_point() + 
  labs(title = "City vs. Highway Fuel Efficiency",
       x = "City Fuel Efficiency (mpg)",
       y = "Highway Fuel Efficiency (mpg)",
       color = "Maker") +
    theme_minimal()+
    theme(legend.position = "right",
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8))

# Box plot of city fuel efficiency by transmission type
ggplot(df_filtered, aes(x = trany, y = UCity, fill = trany)) +
  geom_boxplot() +
  labs(title = "City Fuel Efficiency by Transmission Type",
       x = "Transmission Type",
       y = "City Fuel Efficiency (mpg)") +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_blank())

# Density plot of highway fuel efficiency by fuel type using the sampled data
ggplot(df_filtered, aes(x = UHighway, fill = fuelType)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Highway Fuel Efficiency by Fuel Type",
       x = "Highway Fuel Efficiency (mpg)",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Histogram of the year column
ggplot(df_filtered, aes(x = year)) +
  geom_histogram(binwidth = 1, color="yellow") +
  labs(title = "Distribution of Records by Year",
       x = "Year",
       y = "Count") +
  theme_minimal()

# Aggregate data by year to get the average city fuel efficiency
agg_yearly <- df_filtered %>% group_by(year) %>% summarise(avg_UCity = mean(UCity, na.rm = TRUE))

# Line plot of average city fuel efficiency over the years
ggplot(agg_yearly, aes(x = year, y = avg_UCity)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Average City Fuel Efficiency Over the Years",
       x = "Year",
       y = "Average City Fuel Efficiency (mpg)") +
  theme_minimal()

# Box plot of city fuel efficiency by year
ggplot(df_filtered, aes(x = as.factor(year), y = UCity)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "City Fuel Efficiency by Year",
       x = "Year",
       y = "City Fuel Efficiency (mpg)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis text for better readability

#  Group by cylinders and then year, and count the number of records
cylinder_year_counts <- df_filtered %>% group_by(cylinders,year) %>% reframe(count = n())

# Line plot of counts by year for each number of cylinders
ggplot(cylinder_year_counts, aes(x = year, y = count, color = as.factor(cylinders),
                                 group = as.factor(cylinders))) +
  geom_line() +
  geom_point() +
  labs(title = "Cars produced with no. of Cylinders by Year",
       x = "Year",
       y = "Count",
       color = "No.of cylinders") +
  theme_minimal()
