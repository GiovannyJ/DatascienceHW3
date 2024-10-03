# Load necessary libraries
library(fmsb)
library(readr)

# Load the CSV file and inspect
words_data <- read_csv("words.csv")
head(words_data)

# Check column names
colnames(words_data)

# Categories for words in the CSV (adjusted based on your CSV file)
person_categories <- data.frame(
  person = words_data$Name,  # 'Name' is the first column in your CSV
  
  Politics_and_Leadership = rowSums(words_data[, c("Trump", "Biden", "Kamala", "Harris")], na.rm = TRUE),
  
  Economics_and_Business = rowSums(words_data[, c("Economy, economic", "Business", "Job(s)", "Tax(es)", "Inflation")], na.rm = TRUE),
  
  Society_and_Values = rowSums(words_data[, c("Democacy", "Freedom", "Law", "God", "Love", "Family", "Father", "Mother", "Abortion")], na.rm = TRUE),
  
  Immigration_and_Borders = rowSums(words_data[, c("Border", "Immigrant, immigration", "Invasion", "Illegal Aliens", "Neighbor, neighborhood")], na.rm = TRUE),
  
  International_Affairs_and_Conflicts = rowSums(words_data[, c("War", "Ukraine", "Russia", "Putin", "China, Chinese", "Israel", "Gaza", "Hamas", "Terrorist", "Iran", "Afganistan")], na.rm = TRUE)
)

# Check the new data structure
head(person_categories)

# Find the maximum and minimum for each category to normalize radar chart
max_values <- apply(person_categories[, -1], 2, max)
min_values <- apply(person_categories[, -1], 2, min)

# Add max and min rows to the dataset for radar chart scaling
person_categories_scaled <- rbind(max_values, min_values, person_categories[, -1])

# Function to create radar chart for each person
# Function to create radar chart for each person with custom colors
create_radar_chart <- function(person_data, person_name) {
  
  # Set color based on the person
  if (person_name %in% c("Donald J. Trump", "JD Vance")) {
    polygon_color <- rgb(1, 0, 0, 0.9)  # Red polygon
    fill_color <- rgb(1, 0, 0, 0.5)     # Semi-transparent red fill
  } else {
    polygon_color <- rgb(0, 0, 1, 0.9)  # Blue polygon
    fill_color <- rgb(0, 0, 1, 0.5)     # Semi-transparent blue fill
  }
  par(mar = c(5, 5, 5, 5))
  
  #par(mar = c(15, 15, 15, 15))
  # Create radar chart with the selected color
  radarchart(
    person_data,
    axistype = 1,
    # Custom polygon
    pcol = polygon_color,
    pfcol = fill_color,
    plwd = 4,
    # Custom grid
    cglcol = "grey", 
    cglty = 1, 
    axislabcol = "grey", 
    caxislabels = seq(0, 20, 5), 
    cglwd = 0.8,
    # Custom labels
    vlcex = 0.8,
    title = paste("Radar Chart for", person_name)
  )
}


# Loop through each person to create radar charts
for (i in 3:nrow(person_categories_scaled)) {
  person_name <- person_categories$person[i - 2]  # Skip the max and min rows
  person_data <- rbind(max_values, min_values, person_categories_scaled[i, ])
  
  create_radar_chart(person_data, person_name)
}


