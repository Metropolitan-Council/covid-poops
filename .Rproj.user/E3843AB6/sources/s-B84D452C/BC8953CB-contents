# toolbox: 
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(councilR)
library(DT)

# data:
combined_data <- read.csv('combined_data.csv') %>% mutate(date = as.Date(date))
load_data <- read.csv('clean_load_data.csv') %>% mutate(date = as.Date(date))
variant_data <- read.csv('clean_variant_data.csv') %>% mutate(date = as.Date(date))
case_data <- read.csv('case_data.csv') %>% mutate(date = as.Date(date))




# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "font.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "colors.css")
    ),
    # Application title
    h1("Wastewater Treatment COVID-19 Monitoring Dashboard"),
    h3('App Subtitle'),
    # Show a plot of the generated distribution
    div(style = 'width:1200px;', tabsetPanel(
        tabPanel(
            "COVID-19 Variants",
            h4(),
            h3('Tracking COVID-19 Variants in Metro Plant influent'),
            h4('N501Y (Alpha, Beta, and Gamma), L452R (Delta) and K417N (Omicron) mutation frequencies in Metro Plant influent'),
            p("Here is some text to explain what is going on."),
            plotlyOutput("variantPlot", height = "auto"),
            p("Data updated ... Data source ... ")
        ),
        tabPanel(
            "COVID-19 Load",
            h4(),
            h3('Plot title'),
            h4('Plot subtitle'),
            p("Here is some text to explain what is going on."),
            plotlyOutput("loadPlot", height = "auto"),
            p("Data updated ... Data source ... ")
        ),
        tabPanel(
            "Load vs. Reported Cases",
            h4(),
            h3('A strong relationship between SARS-CoV-2 daily load and reported COVID-19 cases'),
            h4('Metro Plant influent SARS-CoV-2 daily load versus new cases in the sewered service area, weekly mean values from 11 April 2021 to present.'),
            p("Here is some text to explain what is going on."),
            plotlyOutput("casesVload", height = "auto"),
            p("Data updated ... Data source ... ")
        ),
        tabPanel("Download Data",
                 h4(),
                 p("Data are divided in three sections: load, the variant frequencies, and the number of cases in the sewered service area."),
                 h3('Load'),
                 p("Data updated ... Data source ... "),
                 DTOutput("loadData"),
                 h3('Variants'),
                 p("Data updated ... Data source ... "),
                 DTOutput("variantData"),
                 h3("Cases"),
                 p("Data updated ... Data source ... "),
                 DTOutput("caseData"))
    ))
)

# Server
server <- function(input, output) {
    output$loadPlot <- renderPlotly({
        
        ay <- list(
            tickfont = list(color = colors$esBlue),
            overlaying = "y",
            side = "right",
            title = list(text = "<b>Viral load in wastewater,</b> M copies/person/day", standoff = 25,
                         font = list(color = colors$esBlue)),
            zerolinewidth = 2,
            zerolinecolor = '#ffff',
            gridcolor = 'ffff'
        )
        
        
        
        load_plot <-
            load_data %>%
            left_join(case_data) %>%
            plot_ly(type = 'scatter', mode = 'lines') %>%
            add_trace(
                x = ~ date,
                y = ~ copies_day_person_M_mn,
                name = 'Viral load',
                size = 1,
                yaxis = "y2",
                fill = 'tozeroy',
                fillcolor = 'rgba(0, 154, 199, .5)',
                line = list(width = 0.5, color = colors$esBlue)
            ) %>%
            add_trace(
                x = ~ date,
                y = ~ covid_cases_7day,
                name = "Cases per 100,000",
                fill = 'tozeroy',
                fillcolor = 'rgba(140, 140, 140, .3)',
                line = list(width = 0.5, color = colors$suppGray)
            ) %>%
            layout(showlegend = F) %>%
            layout(
                margin = list(l = 50, r = 100, b = 50, pad = 10),
                yaxis2 = ay,
                xaxis = list(
                    title = list(text = "Date", standoff = 25),
                    zerolinewidth = 2,
                    gridcolor = 'ffff',
                    zerolinecolor = '#ffff'
                ),
                yaxis = list(
                    title = list(text = "<b>Reported COVID-19 cases,</b> 7-day average", standoff = 25,
                                 font = list(color = colors$suppGray)),
                    zerolinewidth = 2,
                    tickfont = list(color = colors$suppGray),
                    gridcolor = 'ffff',
                    zerolinecolor = '#ffff'
                )
            )
        
        load_plot
    })
    
    
    
    
    output$variantPlot <- renderPlotly({
        variant_plot<-
            variant_data %>%
            filter(!variant == "Other") %>%
            ggplot(aes(x = date, y = frequency, color = variant)) + 
            scale_color_manual(values = c(colors$metrostatsMePurp, colors$esBlue, colors$cdGreen)) + 
            geom_line(alpha = 0.5, lwd = 1.5) + 
            geom_point(size =2.5) + 
            councilR::council_theme()
        
        ggplotly(variant_plot)  
    })
    
    
    output$casesVload <- renderPlotly({
        cases_vs_load_plot<-
            # aggregate data to week:
            combined_data %>%
            mutate(weekof = lubridate::floor_date(date, unit="week", week_start = 7)) %>%
            select(weekof, covid_cases_7day, copies_day_person_M_mn) %>%
            group_by(weekof) %>%
            summarize(across(c(covid_cases_7day, copies_day_person_M_mn), ~mean(., na.rm = T))) %>%
            
            # scatterplot: 
            ggplot(aes(x = covid_cases_7day, y = copies_day_person_M_mn, text = weekof)) + 
            geom_point() + 
            geom_smooth(method = 'lm', se = F, color = colors$esBlue, fill = colors$esBlue)+
            councilR::council_theme()
        
        ggplotly(cases_vs_load_plot, label = "text")  
    })
    
    output$loadData<- renderDT(server = FALSE, {
        load_data %>%
            DT::datatable(extensions = 'Buttons', 
                          options = list(dom = 'Btrip',
                                         buttons = c('copy', 'excel', 'csv'), 
                                         searching = FALSE, 
                                         lengthMenu = FALSE)) %>%
            DT::formatRound('copies_day_person_M_mn', 2) %>%
            DT::formatRound('copies_day_person_M_mn', 2)
    })
    
    
    output$variantData<- renderDT(server = FALSE, {
        variant_data %>%
            DT::datatable(extensions = 'Buttons', 
                          options = list(dom = 'Btrip',
                                         buttons = c('copy', 'excel', 'csv'), 
                                         searching = FALSE, 
                                         lengthMenu = FALSE)) %>%
            DT::formatRound('frequency', 2)
    })
    
    output$caseData<- renderDT(server = FALSE, {
        case_data %>%
            DT::datatable(extensions = 'Buttons', 
                          options = list(dom = 'Btrip',
                                         buttons = c('copy', 'excel', 'csv'), 
                                         searching = FALSE, 
                                         lengthMenu = FALSE)) 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
