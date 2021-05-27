#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(tidyverse)
library(timetk)
library(plotly)
library(readxl)
library(janitor)
library(lubridate)

hpi <- read_excel("../HPI_PO_monthly_hist.xls", skip = 3)

hpi_wrangled <- hpi %>% 
    clean_names() %>% 
    slice(-1) %>%   # remove empty row
    rename(date = month) %>%
    mutate_if(is.numeric, round, digits = 2) %>%  # round all numeric columns to 2 digits
    # arrange() from dplyr to sort rows
    arrange(date)

hpi_tidy <- 
    hpi_wrangled %>% 
    # pivot_longer makes data long, or tidy
    pivot_longer(-date, names_to = "division", values_to = "hpi") %>% 
    group_by(division)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = bs_theme(bootswatch = "cyborg"),
    
    # Application title
    titlePanel("FHFA House Price Index"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxInput(inputId = "sa",
                          label = "Use Seasonally Adjusted data?"),
            checkboxInput(inputId = "usa",
                          label = "Show USA index?"),
            selectInput(inputId = "division",
                        label = "US Census Division:",
                        choices = c("east_north_central", "east_south_central", "middle_atlantic", "mountain", "new_england", "pacific", "south_atlantic", "west_north_central", "west_south_central"),
                        selected = c("south_atlantic", "usa"),
                        selectize = TRUE,
                        multiple = TRUE),
            dateRangeInput(inputId = "date",
                           label = "Start and End Dates:",
                           start = min(hpi_tidy$date),
                           end = max(hpi_tidy$date))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("HPI by Division over Time",
                                 plotOutput("hpiPlot")),
                        tabPanel("Anomaly Detection",
                                 plotlyOutput("anomalyPlot")),
                        tabPanel("Details",
                                 p("The data used in this app comes from the ", a("Federal Housing Finance Agency.", href="https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index.aspx")),
                                 p("This application was built with ", img(src="https://raw.githubusercontent.com/rstudio/hex-stickers/master/PNG/shiny.png", height = "40px"), ".")))
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    hpi_filtered <-reactive({
        if (input$sa) {
            sa_or_nsa <- "_sa"
        } else {
            sa_or_nsa <- "_nsa"
        }
        
        if (input$usa) {
            division_filter <- c(input$division, "usa")
        } else {
            division_filter <- input$division
        }
        
        hpi_tidy %>% 
            filter(str_detect(division, sa_or_nsa),
                   str_detect(division, division_filter)) %>% 
            filter_by_time(date, input$date[1], input$date[2])
    }) 
    
    
    output$hpiPlot <- renderPlot({
        hpi_filtered() %>% 
            ungroup() %>% 
            plot_time_series(date, hpi, .color_var = division, .smooth = FALSE, .interactive = FALSE)
    })
    
    output$anomalyPlot <- renderPlotly({
        hpi_filtered() %>% 
            group_by(division) %>% 
            plot_anomaly_diagnostics(date, hpi, .interactive = TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
