library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)

# reading in files to be used 
happiness = read_csv(file = "./data/happiness.csv") |>
  mutate(Country = ifelse(Country == "United States", "USA", Country)) |>
  rename(region = Country)

world_happiness = happiness
  
ind_happiness = happiness |>
  pivot_longer(
    cols = Score:Generosity,
    names_to = "Measurement",
    values_to = "Value"
  )

# Define UI for application
ui <- navbarPage(
    title = "World Happiness Visualization",
    tabPanel(
      title = "World Map",
      # Sidebar with a slider input for year selection
      sidebarLayout(
        sidebarPanel(
          sliderInput(inputId = "year",
                      label = "Select a Year:",
                      min = min(happiness$Year),
                      max = max(happiness$Year),
                      value = min(happiness$Year),
                      sep = ""),
          helpText("1. Click on the map"),
          helpText("2. Hover over a country on the map to see the Happiness Score"),
          br(), br(), br(),
          helpText("Note: the graph takes a couple seconds to render")
        ),
        
        # Show a plot of the generated map
        mainPanel(
          plotlyOutput("worldPlot")
        )
      ), 
    ),
    
    tabPanel(
      title = "Other Plots",
      sidebarLayout(
        # sidebar with select input for a country and checkboxes for factors
        sidebarPanel(
          selectInput(inputId = "country",
                      label = "Select Country:",
                      choices = sort(unique(ind_happiness$region)),
                      selected = "USA"),
          checkboxGroupInput(inputId = "factors",
                             label = "Choose Factors to View: ",
                             choices = sort(unique(ind_happiness$Measurement))
                             )
        ),
        # shows a graph of the selected factors for a speciic country
        mainPanel(
          plotOutput("lineGraph"),
          br(), br(), br(), br(),
          plotOutput("correlation")
        )
      ),
    ),

    tabPanel(
      title = "Tables",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "table",
                      label = "Select Dataset to View:",
                      choices = c("World Map Data", "Other Graphs Data"),
                      selected = "World Map Data")
        ),
        mainPanel(
          dataTableOutput("tableView")
        )
      ), 
    ),
    
    tabPanel(title = "About",
             includeMarkdown("about.rmd"))
)

# Define server logic 
server <- function(input, output, session) {
  # World Map 
    pick_year = reactive({
      world_happiness |>
        filter(Year == input$year)
    })
    
    observeEvent(
      eventExpr = input$year,
      handlerExpr = {
        updateSliderInput(session = session, 
                          inputId = "year",
                          value = input$year)
      }
    )
  
  # Graphs
    specific_country = reactive({
      ind_happiness |>
        filter(region == input$country)
    })

    observeEvent(
      eventExpr = input$country,
      handlerExpr = {
        updateSelectInput(inputId = "country",
                          choices = sort(unique(ind_happiness$region)),
                          selected = input$country)
      }
    )
    
    choose_factors = reactive({
      ind_happiness |>
        filter(Measurement %in% input$factors)
    })
    
    
    observeEvent(
      eventExpr = input$reset, 
      handlerExpr = {
      updateSelectInput(session = session,
                        inputId = "country", 
                        selected = "USA")
      updateCheckboxGroupInput(session = session,
                               inputId = "factors", 
                               choices = sort(unique(ind_happiness$Measurement))
                               )
    })

  
  # outputs
    output$worldPlot <- renderPlotly({
      ggplotly(
        pick_year() |>
          right_join(map_data("world"), by = "region") |>
          ggplot(aes(long, lat, group = group), fig(10, 10)) +
          geom_polygon(aes(fill = Score), color = "white") +
          scale_fill_viridis_c(option = "E", na.value = "light gray") +
          theme_void() +
          ggtitle("World Happiness Distribution By Year") +
          theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
      )
    })
    
    output$lineGraph <- renderPlot({
      specific_country() |>
        filter(Measurement %in% input$factors) |>
        ggplot(aes(x = Year, y = Value, group = Measurement, color = Measurement)) +
        geom_line() +
        ggtitle("Happiness Factors Over the Years") +
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    })
    
    output$correlation <- renderPlot({
      corrplot(calculateCorrelations(happiness, input$country), 
               type = "upper",
               rder = "hclust", 
               tl.col = "black", 
               tl.srt = 45, 
               title = "Correlation Matrix",
               mar=c(0,0,1,0))
    })
    
    output$tableView <- renderDataTable({
      if (input$table == "World Map Data") {
        pick_year()
      } else {
        specific_country()
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
