# ---------------- Libraries ----------------
library(shiny)
library(tidyverse)

# ---------------- Data ----------------
data <- read_csv("vitamin_deficiency_cleaned.csv")

# ---------------- UI ----------------
ui <- fluidPage(
  
  titlePanel("Vitamin Deficiency & Health Dashboard"),
  
  tabsetPanel(
    
    tabPanel(
      "Overview",
      fluidRow(
        column(6, plotOutput("agePlot")),
        column(6, plotOutput("genderPlot"))
      )
    ),
    
    tabPanel(
      "Nutrient Analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "vitamin",
            "Select Nutrient",
            choices = c(
              "Vitamin D" = "vitamin_d_percent_rda",
              "Vitamin B12" = "vitamin_b12_percent_rda",
              "Iron" = "iron_percent_rda"
            )
          ),
          selectInput(
            "diet",
            "Filter by Diet",
            choices = c("All", levels(data$diet_type))
          )
        ),
        mainPanel(
          plotOutput("vitaminHist"),
          tableOutput("vitaminSummary")
        )
      )
    ),
    
    tabPanel(
      "Lifestyle & Risk",
      fluidRow(
        column(6, plotOutput("sunVitDPlot")),
        column(6, plotOutput("dietB12Plot"))
      )
    ),
    
    tabPanel(
      "Disease Risk",
      fluidRow(
        column(12, plotOutput("diseaseDefPlot"))
      )
    )
    
  )
)

# ---------------- SERVER ----------------
server <- function(input, output) {
  
  filtered_data <- reactive({
    if (input$diet == "All") {
      data
    } else {
      data %>% filter(diet_type == input$diet)
    }
  })
  
  
  output$genderPlot <- renderPlot({
    data %>%
      count(gender) %>%
      ggplot(aes(gender, n)) +
      geom_col() +
      labs(title = "Gender Distribution", x = "Gender", y = "Count")
  })
  
  output$vitaminHist <- renderPlot({
    ggplot(filtered_data(), aes(.data[[input$vitamin]])) +
      geom_histogram(bins = 25) +
      labs(title = "Nutrient %RDA Distribution", x = "% RDA", y = "Count")
  })
  
  output$vitaminSummary <- renderTable({
    filtered_data() %>%
      summarise(
        Mean_RDA = mean(.data[[input$vitamin]], na.rm = TRUE),
        Deficient_Percent = mean(.data[[input$vitamin]] < 80) * 100
      )
  })
  
  output$sunVitDPlot <- renderPlot({
    ggplot(data, aes(sun_exposure, vitamin_d_percent_rda)) +
      geom_boxplot() +
      labs(title = "Vitamin D by Sun Exposure")
  })
  
  output$dietB12Plot <- renderPlot({
    ggplot(data, aes(diet_type, vitamin_b12_percent_rda)) +
      geom_boxplot() +
      labs(title = "Vitamin B12 by Diet Type")
  })
  
  output$diseaseDefPlot <- renderPlot({
    ggplot(data, aes(vit_d_deficient, fill = disease_diagnosis)) +
      geom_bar(position = "fill") +
      labs(title = "Disease Diagnosis by Vitamin D Deficiency")
  })
  
}

# ---------------- RUN APP ----------------
shinyApp(ui = ui, server = server)

