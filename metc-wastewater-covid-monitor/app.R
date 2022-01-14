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
    h3('Wastewater may yield clues to COVID-19 infection rates'),
    # Show a plot of the generated distribution
    div(style = 'width:1200px;', tabsetPanel(
        
        tabPanel(
            "Prevalence",
            h4(),
            h4("Tracking COVID-19 Prevalence with Metro Plant Wastewater"),
            p("The number of reported cases of COVID-19 infections in the seven-county metro area corresponds to the prevalence of the virus in wastewater samples at the Metro treatment plant in Saint Paul. The plant serves a large portion of the seven-county metro area. "),
            plotlyOutput("loadPlot", height = "auto"),
            p("The blue line shows the total amount of SARS-CoV-2 viral RNA in wastewater flowing into the Metro Plant, in millions copies of SARS-CoV-2 RNA (N1 and N2 gene) per person served by the wastewater area, per day. The gray line shows the seven-day moving average number of new reported COVID-19 infections in the seven-county Metro area per 100,000 residents. Case data are provided by the Minnesota Department of Health and downloaded from USA Facts (https://usafacts.org).. New cases tend to lag wastewater detection trends by about 6-8 days."),
            p("Data last updated 2022-01-13.")
        ),
        tabPanel(
            "Variants",
            h4(),
            h4('COVID-19 variant tracker'),
            p("As the Delta variant of the SARS-CoV-2 virus declined, the Omicron variant quickly took its place as the dominant variant in wastewater samples at the Metro treatment plant in Saint Paul. The plant serves a large portion of the seven-county metro area. "),
            plotlyOutput("variantPlot", height = "auto"),
            p('Alpha, Beta and Gamma frequencies are inferred from the presence of the N501Y mutation; Delta from the L452R mutation; and Omicron from the K417N mutation. Presence of K417N mutation before November 18 were inferred to be the Beta variant and are omitted from this image.'),
            p("Data last updated 2022-01-13.")
        ),
        # tabPanel(
        #     "Relationship to Cases",
        #     h4(),
        #     h4('Relationship between SARS-CoV-2 daily load and reported COVID-19 cases'),
        #     p('Metro Plant influent SARS-CoV-2 daily load versus new cases in the sewered service area, weekly mean values from 11 April 2021 to present.'),
        #     plotlyOutput("casesVload", height = "auto"),
        #     p("Here is some text to explain what is going on."),
        #     p("Data last updated 2022-01-13.")
        # ),
        tabPanel("Download Data",
                 h4(),
                 p("Data are divided in three sections: prevalence (load), variant frequencies, and the number of cases in the sewered service area."),
                 h3('Prevalence'),
                 p("SARS-CoV-2 prevalence in wastewater influent is determined from multiple samples of wastewater each day. Units are in millions of copies of N1 and N2 genes, per person in the sewage treatment area, per day. RNA counts are determined by MCES and the Universit of Minnesota Genomics Center. Cases are a per-capita (per 100,000 people) 7-day rolling average case rates for the 7-county Metropolitan Council area, provided by the Minnesota Department of Health and downloaded from USA Facts (https://usafacts.org)."),
                 DTOutput("loadData"),
                 h3('Variants'),
                 p("Variant presence and frequency are inferred from the N501Y mutation (Alpha, Beta and Gamma); the L452R mutation (Delta); and the K417N mutation (Omicron). K417N mutations present before November 18, 2020 are assumed to be Beta variants, and are marked as Other in the variant column."),
                 DTOutput("variantData")
                 # h3("Cases"),
                 #  DTOutput("caseData")
                 )
    ))
)

# Server
server <- function(input, output) {
    output$loadPlot <- renderPlotly({
        
        ay <- list(
            tickfont = list(color = colors$esBlue),
            overlaying = "y",
            side = "right",
            title = list(text = "Viral load (M copies per person per day)", standoff = 25,
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
                name = "Cases",
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
                    title = list(text = "COVID-19 cases (per capita, 7-day avg.)", standoff = 25,
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
        pal <- c(colors$cdGreen, colors$esBlue, colors$metrostatsDaPurp)
        pal <- setNames(pal, c("Omicron", "Delta", "Alpha, Beta & Gamma"))
        
        variant_plot <-
            variant_data %>%
            plot_ly() %>%
            add_trace(
                type = 'scatter',
                mode = 'markers',
                x = ~ date,
                y = ~ frequency,
                split = ~ variant,
                color = ~ variant,
                alpha = 0.8,
                colors = pal
            ) %>%
            add_trace(
                type = 'scatter',
                mode = 'lines',
                x = ~ date,
                fill = 'tozeroy',
                y = ~ frequency_7day,
                split = ~ variant,
                color = ~ variant,
                alpha = 0.25,
                line = list(color = "#fffff"),
                colors = pal
            ) %>%
            layout(
                margin = list(
                    l = 50,
                    r = 100,
                    b = 50,
                    pad = 10
                ),
               
                xaxis = list(
                    title = list(text = "Date", standoff = 25),
                    zerolinewidth = 2,
                    gridcolor = 'ffff',
                    zerolinecolor = '#ffff'
                ),
                
                yaxis = list(
                    title = list(
                        text = "<b>Frequency of marker mutations (%)</b>",
                        standoff = 25,
                        font = list(color = colors$suppGray)
                    ),
                    tickformat = "%",
                    zerolinewidth = 2,
                    tickfont = list(color = colors$suppGray),
                    gridcolor = "gray90",
                    zerolinecolor = "gray50"
                ))
        
                
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
            left_join(case_data) %>%
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
    
    # output$caseData<- renderDT(server = FALSE, {
    #     case_data %>%
    #         DT::datatable(extensions = 'Buttons', 
    #                       options = list(dom = 'Btrip',
    #                                      buttons = c('copy', 'excel', 'csv'), 
    #                                      searching = FALSE, 
    #                                      lengthMenu = FALSE)) 
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
