#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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
ui <- fluidPage(

    # Application title
    titlePanel("FHFA House Price Index"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "division",
                        label = "US Census Division:",
                        choices = c("east_north_central_sa", "east_south_central_sa", "middle_atlantic_sa", "mountain_sa", "new_england_sa", "pacific_sa", "south_atlantic_sa", "west_north_central_sa", "west_south_central_sa", "usa_sa"),
                        selected = c("south_atlantic_sa", "usa_sa"),
                        selectize = TRUE,
                        multiple = TRUE
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("hpiPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$hpiPlot <- renderPlot({
        hpi_plot <- hpi_tidy %>% 
            filter(division %in% input$division)
        
        plot_hpi <- ggplot(hpi_plot, aes(x = date, y = hpi, color = division)) +
            geom_line()
        
        plot_hpi
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
