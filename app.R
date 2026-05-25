# Uruchomienie: shiny::runApp("app.R")

library(devtools)
load_all(".")

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)

sales_data <- load_sales_data()
clean_data <- clean_sales_ts(sales_data)
manager <- create_management_summary(clean_data)

ui <- fluidPage(
  tags$head(tags$style(HTML("body { background-color: #f5f7fb; } .hero { background: linear-gradient(135deg, #223A5E, #5B7BB8); color: white; padding: 24px; border-radius: 18px; margin-bottom: 18px; } .card { background: white; border-radius: 16px; padding: 18px; margin-bottom: 18px; box-shadow: 0 3px 12px rgba(0,0,0,0.08); } .metric { font-size: 28px; font-weight: 700; color: #223A5E; } .label { color: #666; font-size: 13px; }"))),
  div(class = "hero", h1("salesTSKinga"), h4("Panel analizy szeregów czasowych sprzedaży")),
  sidebarLayout(
    sidebarPanel(class = "card", h4("Filtry"), selectInput("store", "Sklep:", choices = sort(unique(clean_data$store_nbr)), selected = 1), selectInput("category", "Kategoria:", choices = sort(unique(clean_data$family)), selected = "GROCERY I"), helpText("Panel pokazuje prosty interfejs do funkcji pakietu.")),
    mainPanel(
      fluidRow(column(4, div(class = "card", div(class = "label", "Najlepszy sklep"), div(class = "metric", textOutput("best_store")))), column(4, div(class = "card", div(class = "label", "Największa kategoria"), div(class = "metric", textOutput("best_category")))), column(4, div(class = "card", div(class = "label", "Liczba obserwacji"), div(class = "metric", textOutput("n_rows"))))),
      div(class = "card", h3("Trend sprzedaży"), plotOutput("trend_plot", height = "380px")),
      div(class = "card", h3("Podsumowanie menedżerskie"), p(textOutput("manager_text"))),
      div(class = "card", h3("Ranking sklepów"), DTOutput("store_table")),
      div(class = "card", h3("Ranking kategorii"), DTOutput("category_table"))
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({ clean_data |> filter(store_nbr == input$store, family == input$category) })
  output$best_store <- renderText({ manager$store_ranking$store_nbr[1] })
  output$best_category <- renderText({ manager$category_ranking$family[1] })
  output$n_rows <- renderText({ scales::comma(nrow(filtered_data())) })
  output$trend_plot <- renderPlot({ plot_sales_trends(clean_data, store = input$store, category = input$category) })
  output$manager_text <- renderText({ manager$text })
  output$store_table <- renderDT({ manager$store_ranking |> head(15) |> datatable(options = list(pageLength = 5), rownames = FALSE) })
  output$category_table <- renderDT({ manager$category_ranking |> head(15) |> datatable(options = list(pageLength = 5), rownames = FALSE) })
}

shinyApp(ui, server)
