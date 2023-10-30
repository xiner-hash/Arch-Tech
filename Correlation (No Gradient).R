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


ui <- fluidPage(
  titlePanel("Correlation Plot"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput", "Select Year", 
                  min = min(final_df$Year), 
                  max = max(final_df$Year),
                  value = min(final_df$Year),  # Initial value
                  step = 1),
      width = 8
    ),
    
    mainPanel(
      plotOutput("correlationPlot", width = "100%", height = "800px"),
      width = 70
    )
  )
)



server <- function(input, output) {
  
  output$correlationPlot <- renderPlot({
    selected_year <- input$yearInput
    selected_data <- final_df[final_df$Year == selected_year, -1] # Exclude the Year column
    
    correlation_matrix <- cor(selected_data)
    
    # Set self-correlations to NA
    diag(correlation_matrix) <- NA
    
    # Create a mask to hide the lower triangle
    mask <- lower.tri(correlation_matrix)
    correlation_matrix[mask] <- NA
    
    # Plot the upper diagonal without question marks and boundary
    corrplot(
      correlation_matrix,
      method = "circle",
      type = "upper",
      tl.cex = 1,  # Adjust text size
      tl.col = "black",
      tl.srt = 60,   # Adjust text rotation angle
      tl.pos = "lt",
      mar = c(1, 5, 5, 5),   # Adjust the plot margins (left, right, top, bottom)
      col = c("darkgray", "#6A040F"),  # Assign darkred for positive and darkgray for negative correlations
      asp = 1,  # Maintain equal aspect ratio
      cl.cor = "blank",  # Remove labels for NAs
      na.label=" ",
      cl.pos = 'n',
      diag=FALSE
    )
  })
}




shinyApp(ui, server)

