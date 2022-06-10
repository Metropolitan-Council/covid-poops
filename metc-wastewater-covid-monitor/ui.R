


fluidPage(
  navbarPage(
    
    fluid = TRUE,
    title = div(
      img(src = "main-logo.png",
          height = "60px", alt = "MetCouncil logo")
    ),
    id = "navBar",
    collapsible = TRUE,
    windowTitle = "Wastewater Treatment COVID-19 Monitoring Dashboard",
    position = "fixed-top",
    header = tags$style(
      ".navbar-right {
                       float: right !important;
                       }",
      "body {padding-top: 75px;}",
      ".sidebar {max-width: 100px;}"
    ),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "font.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "colors.css"),
      includeHTML("www/google-analytics.html"),
      tags$html(lang = "en")),
    
    tabPanel(
      "COVID-19 Trends",
      br(),
      fluidRow(
        h2("COVID-19 Surveillance at the Metro Plant"),
        
        tabsetPanel(
          type = "pills",
          tabPanel(
            "COVID-19 Prevalence",
            HTML("<h6><section style='font-size:14pt'>This graph shows the amount of SARS-CoV-2 viral RNA detected in Metro Plant wastewater influent (blue line) and the number of COVID-19 cases in the seven-county area (gray line).</h3>"
            ),
            HTML("<h6><section style='font-size:12pt;font-style:italic'>Last Sample Date: June 6, 2022.</h3>"
            ),
            plotlyOutput("loadPlot", height = "auto")
          ),
          
          tabPanel(
            "Variant Prevalence",
            HTML("<h6><section style='font-size:14pt'>This graph shows the estimated amount of SARS-CoV-2 viral RNA by COVID-19 variant, sub-variant and lineage. The total amount of SARS-CoV-2 viral RNA in Metro Plant wastewater influent is shown in the background in gray.</h3>"
            ),
            HTML("<h6><section style='font-size:12pt;font-style:italic'>Last Sample Date: June 6, 2022.</h3>"
            ),
            plotlyOutput("variantLoadPlot", height = "auto")
          ),
          
          tabPanel(
            "Variant Frequencies (%)",
            HTML("<h6><section style='font-size:14pt'>This graph shows the estimated percent of SARS-CoV-2 viral RNA contributed by COVID-19 variant, sub-variant and lineage.</h3>"
            ),
            HTML("<h6><section style='font-size:12pt;font-style:italic'>Last Sample Date: June 6, 2022.</h3>"
            ),
            plotlyOutput("variantFreqPlot", height = "auto")
          ),
          tabPanel(
            "This Week's Summary",
            h6("June 10, 2022"),
            p(
              "The most recent data update includes samples taken May 31- June 6, 2022. During this sampling period:"
            ),
            tags$ul(
              tags$li("The viral RNA load in Metro Plant influent decreased by 16% compared to the previous week"
              ),
              tags$li("Omicron subvariant BA.2 made up 68% of the SARS-CoV-2 RNA in Metro Plant influent, down from 76% the week earlier"
              ),
              tags$li("Essentially all of the BA.2 in Metro Plant influent is now made up of sub-lineage BA.2.12.1"
              ),
              tags$li("Omicron subvariants BA.4 and BA.5 together made up 32% of the SARS-CoV-2 RNA in Metro Plant influent, up from 23% a week earlier"
              )
            )
          )
        )
      )
    ),
    tabPanel(
      "Download Data",
      br(),
      # load -----
      p(
        "Data are divided in two sections: prevalence (load) and cases, and variant frequencies"
      ),
      h3("Prevalence"),
      p(
        "SARS-CoV-2 prevalence in wastewater influent is determined from multiple samples of wastewater each day. Units are in millions of copies of N1 and N2 genes, per person in the sewage treatment area, per day. Viral load data are from Metropolitan Council and the University of Minnesota Genomics Center. Cases are a per-capita (per 100,000 people) 7-day rolling average case rates for the 7-county Metropolitan Council area, provided by the Minnesota Department of Health and downloaded from USA Facts (https://usafacts.org)."
      ),
      br(),
      DTOutput("loadData"),
      
      # variants -----
      h3("Variants"),
      p(
        "Variant presence and frequency are inferred from the N501Y mutation (Alpha, Beta and Gamma); the L452R mutation (Delta); and the K417N mutation (Omicron). K417N mutations present before November 18, 2020 are assumed to be Beta variants, and are marked as Other in the variant column. The two sub-lineages of Omicron (BA.1 and BA.2) are distinguished by the HV 69/70 deletion: Omicron BA.1 contains both the K417N mutation and the HV 69/70 deletion. Omicron BA.2 has the K417N mutation but not the HV 69/70 deletion. Omicron BA.2.12.1 is distinguished by the L452Q mutation."
      ),
      br(),
      DTOutput("variantData"),
      
      # # cases -----
      # h3("Cases"),
      # p("Data updated ... Data source ... "),
      # DTOutput("caseData")
    ),
    # tabPanel(
    #   "FAQ",
    #   br(),
    #   bsCollapsePanel(HTML("<h3><section style='font-size:14pt; font-weight: bold;'>What can wastewater tell us about COVID-19 infections?</h3>"), 
    #                   htmlOutput("faqAnswer1")),
    #   bsCollapsePanel(HTML("<h3><section style='font-size:14pt; font-weight: bold;'>Who monitors the presence of the virus in wastewater?</h3>"), 
    #                   htmlOutput("faqAnswer2"))
    #  ),
    footer = tags$div(
      "This project is open-source. See our GitHub repository here",
      tags$a(
        href = "https://github.com/Metropolitan-Council/covid-poops",
        shiny::icon("external-link-alt", lib = "font-awesome"),
        target = "_blank"
      ),
      tags$br(),
      "App last updated ",
      "2022-06-10",
      # using gh::gh() causes SAML error in production
      # gh::gh("GET /repos/Metropolitan-Council/covid-poops")[49][[1]] %>% as.Date(),
      style = "font-size: 1.5rem;
             display: block;
             text-align: right;
             padding: 1%;"
    )
  )
)
