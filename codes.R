#Environmental setup
# Set API options
options(elsevier_api_key = "your_api_key_here") # Replace with your actual API key

# Load necessary libraries
library(rscopus)      # For accessing Scopus API
library(tidyverse)    # For data manipulation and visualization
library(tm)           # For text mining
library(hrbrthemes)   # For enhanced ggplot2 themes
library(tidytext)     # For text analysis

# Define the search query
query <- 'TITLE("soil health") AND PUBYEAR > 1999 AND PUBYEAR < 2023'

# Execute the search on Scopus
results <- scopus_search(query, count = 25, view = "COMPLETE")

# Extract information from the results
papers <- results$entries
titles <- sapply(papers, function(x) ifelse(is.null(x$`dc:title`), NA, x$`dc:title`))
years <- sapply(papers, function(x) ifelse(is.null(x$`prism:coverDate`), NA, substr(x$`prism:coverDate`, 1, 4)))
authors <- sapply(papers, function(x) ifelse(is.null(x$`dc:creator`), NA, x$`dc:creator`))
abstracts <- sapply(papers, function(x) ifelse(is.null(x$`dc:description`), NA, x$`dc:description`))

# Combine into a data frame
data <- data.frame(Title = titles, Year = years, Authors = authors, Abstract = abstracts, stringsAsFactors = FALSE)

# Remove rows with NA values in the Abstract
data <- na.omit(data)

# Save the results to a CSV file
write.csv(data, "papers_data.csv", row.names = FALSE)

                    # Define new variables based on the presence of specific terms in the Abstract
data <- data %>%
  mutate(
    soil_health_crop_yield = grepl("soil health", Abstract, ignore.case = TRUE) & grepl("crop yield", Abstract, ignore.case = TRUE),
    # Additional variables...
  )

# Summarize the results
summary_df <- data %>%
  count(Category = c("Soil Health + Crop Yield", "Soil Health + Soil Function", "Soil Health + Ecosystem Services", "Soil Health + Economics", "Soil Health + Water Quality", "Soil Health + Greenhouse Gas"),
        wt = c(soil_health_crop_yield, soil_health_soil_function, soil_health_ecosystem_services, soil_health_economics, soil_health_water_quality, soil_health_greenhouse_gas)) %>%
  arrange(desc(n))


 # Create a bar plot for summary data
ggplot(summary_df, aes(x = Category, y = n, fill = Category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Specific Terms in Abstracts",
       subtitle = "Co-occurrence of 'soil health' with key terms",
       y = "Number of Papers")

# Trend analysis plot
yearly_trend <- data %>%
  group_by(Year) %>%
  summarize(count = sum(soil_health_crop_yield, na.rm = TRUE)) %>%
  mutate(cumulative_count = cumsum(count))

ggplot(yearly_trend, aes(x = Year, y = cumulative_count)) +
  geom_line(color = "blue") +
  labs(title = "Trend of 'Soil Health + Crop Yield' Over Years",
       x = "Year",
       y = "Cumulative Count") +
  theme_minimal()
                   
