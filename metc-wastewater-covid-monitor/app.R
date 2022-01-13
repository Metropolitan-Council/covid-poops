# data:
combined_data <- read.csv('combined_data.csv') %>% mutate(date = as.Date(date))
load_data <- read.csv('clean_load_data.csv') %>% mutate(date = as.Date(date))
variant_data <- read.csv('clean_variant_data.csv') %>% mutate(date = as.Date(date))


library(shiny)
library(tidyverse)
library(plotly)
library(councilR)
library(DT)


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Wastewater Treatment COVID-19 Monitoring Dashboard"),
    h3('Some general text here'),
    # Show a plot of the generated distribution
    tabsetPanel(
        tabPanel(
            "COVID-19 Variants",
            h3('Plot title'),
            h4('Plot subtitle'),
            plotlyOutput("variantPlot")
        ),
        tabPanel(
            "COVID-19 Load",
            h3('Plot title'),
            h4('Plot subtitle'),
            plotlyOutput("loadPlot")
        ),
        tabPanel(
            "Load vs. Reported Cases",
            h3('Plot title'),
            h4('Plot subtitle'),
            plotlyOutput("casesVload")
        ),
        tabPanel("Download Data",
                 DTOutput("loadData"))
    )
)

# Server
server <- function(input, output) {

    output$loadPlot <- renderPlotly({
        load_plot <- 
            ggplot(load_data, aes(x = date, y = copies_day_person_M_mn))+ 
            geom_point() + 
            geom_errorbar(aes(ymin = copies_day_person_M_mn - copies_day_person_M_se, ymax = copies_day_person_M_mn + copies_day_person_M_se)) + 
            geom_line(linetype = 'dashed', color = 'gray50') + 
            councilR::council_theme()
        ggplotly(load_plot)
    })
    
    output$loadData <- renderDT({
        load_data %>%
            DT::datatable(extensions = 'Buttons', 
                          options = list(dom = 'Btrip',
                                         buttons = c('copy', 'excel', 'csv'), 
                                         searching = FALSE, 
                                         lengthMenu = FALSE)) %>%
            DT::formatRound('copies_day_person_M_mn', 2) %>%
            DT::formatRound('copies_day_person_M_mn', 2)
    })
    
    
    output$variantPlot <- renderPlotly({
        variant_plot<-
            variant_data %>%
            filter(!variant == "Other") %>%
            ggplot(aes(x = date, y = frequency, color = variant)) + 
            geom_line() + 
            councilR::council_theme()
        
        ggplotly(variant_plot)  
    })
    
    
    output$casesVload <- renderPlotly({
        cases_vs_load_plot<-
            combined_data %>%
            ggplot(aes(x = covid_cases_7day, y = copies_day_person_M_mn)) + 
            geom_point() + 
            geom_smooth(method = 'lm', se = F, color = 'blue')+
            councilR::council_theme()
        
        ggplotly(cases_vs_load_plot)  
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
