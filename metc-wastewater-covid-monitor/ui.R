

fluidPage(
  navbarPage(
    fluid = TRUE,
    title = div(
      img(
        src = "main-logo.png",
        height = "60px", alt = "MetCouncil logo"
      )
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
      includeHTML("www/google-analytics.html")
    ),

    tabPanel(
      "COVID-19 Trends",
      # h4("COVID-19 Wastewater Surveillance"),
      wellPanel(
        br(),
        h2("Key Facts"),
        h4("May 20, 2022"),
        p("Scientists in the Met Council’s Environmental Services are monitoring COVID-19 prevalence in wastewater influent samples from the Metro Plant in Saint Paul. The plant serves nearly two million people in the seven-county metro area. The most recent data update includes samples taken May 10-16, 2022. During this sampling period:"),
        tags$ul(
          tags$li("The viral load increased by 58% over the previous week"),
          tags$li("Omicron BA.2 made up 92% of of the SARS-CoV-2 RNA"),
        tags$li("Omicron BA.2.12.1 made up 47% of of the SARS-CoV-2 RNA, up from 36% the previous week"),
        tags$li("Omicron BA.4 and BA.5 (not yet shown below) together made up 7% of the SARS-CoV-2 RNA")
        ),
       h4("Select a chart"),
       radioButtons(
          inputId = "plotSel",
          label = NULL,
          choices = c(
            "Total COVID-19 load",
            "COVID-19 load by variant",
            "COVID-19 variant frequencies (%)"
          ),
          selected = "Total COVID-19 load",
          inline = T
        ),
        plotlyOutput("mainPlot", height = "auto"),
        bsCollapsePanel(h6("About the data"), 
                        textOutput("figCaption"))
    )),
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
    footer = tags$div(
      "This project is open-source. See our GitHub repository here",
      tags$a(
        href = "https://github.com/Metropolitan-Council/covid-poops",
        shiny::icon("external-link-alt", lib = "font-awesome"),
        target = "_blank"
      ),
      tags$br(),
      "App last updated ",
      "2022-05-20",
      # using gh::gh() causes SAML error in production
      # gh::gh("GET /repos/Metropolitan-Council/covid-poops")[49][[1]] %>% as.Date(),
      style = "font-size: 1.5rem;
             display: block;
             text-align: right;
             padding: 1%;",
      align = "right"
    )
  )
)
