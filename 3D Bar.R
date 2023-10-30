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


# Define the categories

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



filter_category_columns <- function(df, category) {
  category_columns_selected <- category_columns[[category]]
  df_filtered <- df[, c("Year", category_columns_selected)]
  return(df_filtered)
}

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Stacked Bar Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select a category", choices = names(category_columns)),
      br()
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

server <- function(input, output) {
  # Define the initial color palette
  color_palette <- c("#E3F3E0", "#C0CDBBB", "#6F9C67", "#134F08", "#0C3105")
  
  # Create a reactiveVal to store the color palette
  color_palette_reactive <- reactiveVal(color_palette)
  
  # Update the color palette when the category input changes
  observeEvent(input$category, {
    new_palette <- switch(input$category,
                          "AI Related"          = c("#E3F3E0", "#C0CDBBB", "#6F9C67", "#134F08", "#0C3105"),
                          "Data"                = c("#4A400D", "#665811", "#AEA66C", "#E6E3BD", "#F9F8E0"),
                          "Decision Support"    = c("#FF006E", "#Fc6FB3"),
                          "VR/AR/MR"            = c("#5D169A", "#833BEC", "#BB94F6"),
                          "3D Modelling"        = c("#0C0856", "#160D9B", "#4B45B4", "#7F7CCD", "#E8EBFF"),
                          "Other ICT"           = c("#2F0307", "#57040D", "#8F3F46", "#B4797D", "#EBD1CF", "FFEEE9"),
                          "GIS"                 = c("#FF8E0B"),
                          "Remote Sensing"      = c("#EB5607"),
                          "Other Digital Modelling" = c("#3A86FF"),
                          "Social Media"         = c("#288A85"),
                          "Robotic"             = c("#2A9A16"),
                          default = color_palette
    )
    
    color_palette_reactive(new_palette)
  })
  
  filtered_data <- reactive({
    filter_category_columns(df, input$category)
  })
  
  output$plot <- renderPlotly({
    data <- filtered_data()
    
    plot_df <- data %>%
      group_by(Year) %>%
      summarise(across(everything(), sum)) %>%
      pivot_longer(-Year, names_to = "Technology", values_to = "Count") %>%
      arrange(Technology)
    
    color_palette <- color_palette_reactive()  # Retrieve the updated color palette
    
    # Rest of your code...
    # Assign colors to each unique technology
    unique_technologies <- unique(plot_df$Technology)
    technology_colors <- setNames(color_palette[1:length(unique_technologies)], unique_technologies)
    plot_df$Color <- technology_colors[plot_df$Technology]
    
    View(plot_df)
    
    ggplot(plot_df, aes(x = Year, y = Count, fill = Technology, color = Technology)) +
      geom_bar(stat = "identity", alpha = 0.88) +
      scale_fill_manual(values = plot_df$Color) +
      scale_color_manual(values = plot_df$Color) +
      labs(x = "Year", y = "Count") +
      theme_minimal() +
      theme(legend.position = "right")
    
    
  })
}


# Run the Shiny app
shinyApp(ui, server)

