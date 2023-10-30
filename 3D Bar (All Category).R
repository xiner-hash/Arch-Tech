df <- read.csv("Tech.csv", check.names = FALSE)

# Remove rows where "Year" is NA
df <- df[complete.cases(df$Year), ]
df[is.na(df)] <- FALSE

# Sort the data frame based on the 'Year' variable in ascending order
df <- df[order(df$Year), ]

library(networkD3)
library(dplyr)
library(plotly)
library(shiny)
library(ggplot2)
library(tidyr)
library(reshape2)
library(RColorBrewer)
library(corrplot)

colnames(df) <- gsub("Data science", "Data Science", colnames(df))
colnames(df) <- gsub("Data mining", "Data Mining", colnames(df))
colnames(df) <- gsub("Big data", "Big Data", colnames(df))
colnames(df) <- gsub("Open data", "Open Data", colnames(df))
colnames(df) <- gsub("Remote sensing", "Remote Sensing", colnames(df))
colnames(df) <- gsub("3dScanning", "3D Scanning", colnames(df))
colnames(df) <- gsub("3dModelling", "3D Modelling", colnames(df))
colnames(df) <- gsub("AI&related", "AI Related", colnames(df))
colnames(df) <- gsub("Mat_Stat_not_ai_or_dss", "Other Digital Modelling", colnames(df))
colnames(df) <- gsub("SocialNetworks", "Social Networks", colnames(df))
colnames(df) <- gsub("SocialNews", "Social News", colnames(df))
colnames(df) <- gsub("MediaSharing", "Media Sharing", colnames(df))
colnames(df) <- gsub("PointCloud", "Point Cloud", colnames(df))
colnames(df) <- gsub("Machine learning", "Machine Learning", colnames(df))

category_columns <- list(
  Data = c("Data", "Data Science", "Data Mining", "Big Data", "Crowdsourcing", "Open Data"),
  `VR/AR/MR` = c("VR", "AR", "MR"),
  Robotic = c("Robotic", "DronUAV/UAS"),
  GIS = c("GIS"),
  `Remote Sensing` = c("Remote Sensing"),
  `3D Modelling` = c("BIM", "LIM", "Point Cloud", "Rendering", "3D Scanning", "3D Modelling"),
  `AI Related` = c("AI Related", "Deep Learning", "Artificial Intelligence", "Machine Learning", "MAS"),
  `Decision Support` = c("MCDA/AHP", "PSS/DSS"),
  `Other Digital Modelling` = c("SIM", "Other Digital Modelling"),
  `Social Media` = c("Social Media", "Social Networks", "Social News", "Blogging", "Bookmarking", "Media Sharing"),
  `Other ICT` = c("GPS", "GNSS", "ICT", "5G/Beyond", "Blockchain", "Cloud", "IoT", "Devices")
)



# Define the filter_category_columns function
filter_category_columns <- function(df, category) {
  category_columns_selected <- category_columns[[category]]
  df_filtered <- df %>%
    select(Year, all_of(category_columns_selected)) %>%
    mutate(main_variable = rowSums(across(all_of(category_columns_selected)))) %>%
    mutate(main_variable = as.logical(main_variable))
  return(df_filtered)
}

# Apply the filter_category_columns function for each category and store the results
filtered_dfs <- lapply(names(category_columns), function(category) {
  filter_category_columns(df, category)
})

# Merge all the main variables into one big dataframe
final_df <- do.call(cbind, c(list(df$Year), lapply(filtered_dfs, `[[`, "main_variable")))

# Assign appropriate column names
colnames(final_df) <- c("Year", names(category_columns))


final_df <- as.data.frame(final_df)

plot_df <- final_df %>%
  group_by(Year) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(-Year, names_to = "Technology", values_to = "Count")

color_mapping <- c(
  "VR/AR/MR" = "#8338EC",
  "Social Media" = "#288A85",
  "Robotic" = "#2A9A16",
  "Remote Sensing" = "#EB5607",
  "Other ICT" = "#6A040F",
  "Other Digital Modelling" = "#3A86FF",
  "GIS" = "#FF8E0B",
  "Decision Support" = "#FF006E",
  "Data" = "#665811",
  "AI Related" = "#134F08",
  "3D Modelling" = "#160D9B"
)

gg_plot <- ggplot(plot_df, aes(x = Year, y = Count, fill = Technology)) +
  geom_bar(stat = "identity",alpha = 0.88) +
  scale_fill_manual(values = color_mapping) +
  labs(x = "Year", y = "Count") +
  theme_minimal() +
  theme(legend.position = "right")

plotly_plot <- ggplotly(gg_plot)

plotly_plot

