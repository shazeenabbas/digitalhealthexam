library(shiny)
library(shinyjs)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(shinyWidgets)

# To Load the data
data <- read_csv("adult-depression-lghc-indicator-24_R.csv")
data$Year <- as.factor(data$Year)
data$Percent <- as.numeric(data$Percent)
data$Frequency <- as.numeric(data$Frequency)
data$`Lower 95% CL` <- as.numeric(data$`Lower 95% CL`)
data$`Upper 95% CL` <- as.numeric(data$`Upper 95% CL`)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .light-mode { 
        background-color: #f9f9f9; 
        color: #333333;
        height: 100%;
      }
      .dark-mode { 
        background-color: #1e1e1e; 
        color: #f0f0f0; 
        height: 100%;
      }
      .dark-mode .well { 
        background-color: #2e2e2e; 
        color: #ffffff; 
      }
      .btn-custom {
        font-size: 16px;
        font-weight: bold;
        padding: 10px 20px;
        border-radius: 8px;
        background-color: #007bff;
        color: white;
        border: none;
        transition: all 0.3s ease;
      }
      .btn-custom:hover {
        background-color: #0056b3;
      }
      .card {
        border-radius: 12px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        padding: 20px;
        background: white;
        margin-top: 20px;
      }
      .dark-mode .card {
        background: #2e2e2e;
      }
      .pretty-toggle .btn {
        font-size: 18px !important;
        padding: 12px 24px !important;
      }
      .pretty-toggle .btn i {
        font-size: 24px !important;
      },
      .title {
          color: #fff;
          background-color: #002b49;
          text-align: center;
          padding: 16px;
          display: inline-block;
          font-size: 32px;
          font-weight: bold;
          margin-bottom: 20px;
          margin-top: 10px;
          text-transform: capitalize;
          width: 100%;
      }
      .sideControl {
        
      }
      #page-container {
        height: 95vh;
      }
      .row {
      display: flex;
      justify-content: center;
      align-items: center;
      }
    "))
  ),
  div(
    id = "page-container",
    class = "light-mode",
    titlePanel(
      HTML('<h2 style="color: #fff; background-color: #002b49; text-align: center; padding: 16px; display: inline-block; font-size: 32px; font-weight: bold; margin-bottom: 0; margin-top: 0; text-transform: capitalize; width: 100%;">California Adult Depression Indicator (2012-18)</h2>')
    ),
    sidebarLayout(
      sidebarPanel(
        div(class="sideControl",
            selectInput("strata", "Choose Strata:", 
                        choices = c("Total", "Sex", "Race-Ethnicity", "Education", "Income", "Age")),
            selectInput("metric", "Choose Metric:",
                        choices = c("Percent", "Frequency", "Lower 95% CL", "Upper 95% CL")),
            actionButton("update", "Update", class = "btn-custom")
        )
      ),
      mainPanel(
        div(class = "card", plotOutput("plot"))
      )
    )
  ),
  div(
    id = "dark_mode_container",
    prettyToggle(
      inputId = "dark_mode",
      label_on = "",
      label_off = "",
      icon_on = icon("moon"),
      icon_off = icon("sun"),
      status_on = "success",
      status_off = "danger",
      animation = "rotate"
    )
  )
)

server <- function(input, output, session) {
  observe({
    if (input$dark_mode) {
      shinyjs::addClass(selector = "#page-container", class = "dark-mode")
      shinyjs::removeClass(selector = "#page-container", class = "light-mode")
    } else {
      shinyjs::removeClass(selector = "#page-container", class = "dark-mode")
      shinyjs::addClass(selector = "#page-container", class = "light-mode")
    }
  })
  
  plot_data <- eventReactive(input$update, {
    filtered_data <- data %>% filter(Strata == input$strata)
    
    switch(input$metric,
           "Percent" = {
             ggplot(filtered_data, aes(x = as.numeric(as.character(Year)), y = Percent, color = `Strata Name`, group = `Strata Name`)) +
               geom_line() +
               geom_point() +
               labs(title = paste("Depression Percentage Trend by", input$strata),
                    x = "Year",
                    y = "Percent",
                    color = input$strata)
           },
           "Frequency" = {
             ggplot(filtered_data, aes(x = Year, y = Frequency, fill = `Strata Name`)) +
               geom_bar(stat = "identity", position = "dodge") +
               labs(title = paste("Depression Frequency by", input$strata, "and Year"),
                    x = "Year",
                    y = "Frequency")
           },
           "Lower 95% CL" = {
             filtered_data_long <- filtered_data %>%
               select(Year, `Strata Name`, `Lower 95% CL`) %>%
               pivot_wider(names_from = Year, values_from = `Lower 95% CL`) %>%
               pivot_longer(cols = -`Strata Name`, names_to = "Year", values_to = "Lower_95_CL")
             
             ggplot(filtered_data_long, aes(x = Year, y = `Strata Name`, fill = Lower_95_CL)) +
               geom_tile() +
               scale_fill_gradient(low = "white", high = "red", na.value = "grey90") +
               labs(title = paste("Depression Lower 95% CL by", input$strata, "and Year"),
                    x = "Year",
                    y = input$strata,
                    fill = "Lower 95% CL")
           },
           "Upper 95% CL" = {
             ggplot(filtered_data, aes(x = Year, y = `Upper 95% CL`, size = Percent, color = `Strata Name`)) +
               geom_point(alpha = 0.7) +
               scale_size(range = c(3, 15)) +
               labs(title = paste("Depression Upper 95% CL by", input$strata, "and Year"),
                    x = "Year",
                    y = "Upper 95% CL",
                    size = "Percent",
                    color = input$strata)
           }
    )
  })
  
  output$plot <- renderPlot({
    plot_data() +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = ifelse(input$dark_mode, "#fff", "white"), color = NA),
        panel.background = element_rect(fill = ifelse(input$dark_mode, "#fff", "white"))
      )
  })
}

shinyApp(ui = ui, server = server)