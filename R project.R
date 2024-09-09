#This analysis is performed on Starbucks, a coffee house company with branches in more than 
#24,000 retail stores in 70 countries.
#I will perform data cleaning, manipulation, visualization, answer some questions and generate certain insights


#importing my dataset downloaded from Kaggle
Starbucks <- read.csv("C:/Users/Chika/Desktop/MY MASTERS COURSES/DATA ANALYSIS/Starbuck dataset.csv")
Starbucks

#I will import certain libraries 
library(tidyverse)

#Checking the size of my dataset using the dim function
starbucks_size = dim(Starbucks) 
starbucks_size

length(Starbucks)


#To display only the column names in my dataset
names(Starbucks)

#To print or display just the rows
print(Starbucks[c(1:25000), ])

head(Starbucks)

#Displaying a statistical summary of my dataset
summary(Starbucks)


#DATA CLEANING

#Checking for variables with null values in my dataset
Starbucks %>%
  summarise(across(everything(), ~sum(is.na(.))))


#Data Manipulation
#shaping raw data to discover meaningful patterns and insights for better decision-making
#Sorting my data in ascending order by Store.Number
Starbucks_sorted <- Starbucks %>%
  arrange(Store.Number)
Starbucks_sorted

#Since I will likely not utilize the latitude and longitude column, I will drop both for now
Latest_Starbucks <- Starbucks_sorted %>%
  select(-Longitude, -Latitude)

Latest_Starbucks

#To remove duplicates existing in my dataset
Unique_starbucks <- Latest_Starbucks %>%
  distinct()
Unique_starbucks


#DATA VISALIZATION
#Translating raw data into practical insights via captivating visuals

# Creating a bar plot for the distribution of Starbucks stores by postcode
barplot(table(Unique_starbucks$Postcode), main = "Starbucks Store Distribution by Postcode",color = "pink", xlab = "Postcode", ylab = "Store Count")


#Using graphical representation to illustrate ownership types exclusively based on countries.
ggplot(Unique_starbucks, aes(x = Country, fill = Ownership.Type)) +
  geom_bar(position = "dodge") +
  labs(title = "Ownership Type Based on Country", x = "Country", y = "Frequency") +
  theme_minimal()


#Distribution of cities with Starbucks stores (analyzing the top cities with stores)
# Count the number of unique cities with Starbucks stores
city_counts <- Unique_starbucks %>%
  distinct(City) %>%
  summarise(Count = n())

#Number of countries with Starbucks stores
country_counts <- Unique_starbucks %>%
  distinct(Country) %>%
  summarise(Count = n())

# Plot the graph showing top 15 cities with Starbucks stores
top_cities <- Unique_starbucks %>%
  group_by(City) %>%
  summarise(Store_Count = n()) %>%
  arrange(desc(Store_Count)) %>%
  slice_max(order_by = Store_Count, n = 15)

#top 15 countries with Starbucks store
top_countries <- Unique_starbucks %>%
  group_by(Country) %>%
  summarise(Store_Count = n()) %>%
  arrange(desc(Store_Count)) %>%
  slice_max(order_by = Store_Count, n = 15)


# Create the bar chat plot for top 15 cities
ggplot(top_cities, aes(x = reorder(City, Store_Count), y = Store_Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 15 Cities with Starbucks Stores", x = "City", y = "Number of Stores") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Create the bar chat plot for top 15 countries
ggplot(top_countries, aes(x = reorder(Country, Store_Count), y = Store_Count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Top 15 Countries with Starbucks Stores", x = "Country", y = "Number of Stores") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Representing same visuals in a pie chart displaying top 10 cities only
top_cities2 <- Unique_starbucks %>%
  count(City) %>%
  top_n(10, n) # Change '10' to the number of top cities you want to display

# Plotting the pie chart
ggplot(top_cities2, aes(x = "", y = n, fill = City)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Top Cities with Starbucks Stores", fill = "City") +
  theme_void() +
  theme(legend.position = "bottom") # Adjust legend position as needed


#Correlation Heatmap for StarBucks data
library(ggplot2)
install.packages('corrplot')
library(corrplot)
Starbucks <- read.csv("C:/Users/Chika/Desktop/MY MASTERS COURSES/DATA ANALYSIS/Starbuck dataset.csv")
correlation_matrix <- cor(Starbucks[sapply(Starbucks, is.numeric)])
corrplot(correlation_matrix, method = "color", title = "Correlation Heatmap for Starbucks", 
        tl.col = "blue", addCoef.col = "red")


#Determining which timezone records higher purchasing of coffee across different places
# Plotting the pie chart for the top 10 timezones
Unique_starbucks %>%
  count(Timezone) %>%
  arrange(desc(n)) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = "", y = n, fill = Timezone)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Top 10 Timezones Distribution", fill = "Timezone") +
  theme_void() +
  theme(legend.position = "bottom",
        text = element_text(size = 25))  # Adjust the size value to increase/decrease font size


# Check the structure of the dataset
str(cleaned)

# Run linear regression
model <- lm(Timezone ~ Country, data = cleaned)

# Display summary of the regression
summary(model)





