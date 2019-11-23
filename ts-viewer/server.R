#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$select.source <- renderUI({
        sources = dates$data_source_id %>% setNames(dates$data_source_name)
        selectInput(
            "source", "Select data source"
            , choices = sources
            , width = "100%"
        )
    })
    
    dates.by.source <- reactive({
        req(input$source)
        dates %>% filter(data_source_id == input$source)
    })
    
    output$select.type <- renderUI({
        types <- dates.by.source()$data_type_id %>% setNames(dates.by.source()$data_type_name)
        selectInput(
            "type", "Select data type"
            , types
            , width = "100%"
        )
    })
    
    dates.by.type <- reactive({
        req(input$type)
        dates.by.source() %>% filter(data_type_id == input$type)
    })
    
    output$select.date.range <- renderUI({
        sliderInput(
            "date.range", "Select date / time to display"
            , min = dates.by.type()$min_date
            , max = dates.by.type()$max_date
            , timezone = "EET"
            , animate = TRUE
            , value = c(dates.by.type()$max_date - days(1), dates.by.type()$max_date)
        )
    })
    
    observations <- reactive({
        req(input$date.range)
        url <- paste0(API_BASE, "data/", input$source, "/", input$type, "/", input$date.range[2], "/", input$date.range[1])
        message(url)
        result <- GET(url) %>%
            content("text") %>%
            fromJSON() %>%
            .$Data %>%
            mutate(
                entity_created = ymd_hms(entity_created)
            )
        result
        })
    
    output$tsPlot <- renderPlot({
        ggplot(observations(), aes(x = entity_created, y = value)) +
            geom_point()
    })
})
