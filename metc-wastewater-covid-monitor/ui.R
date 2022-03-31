
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
    # Application title
    # h1("Wastewater Treatment COVID-19 Monitoring Dashboard"),
    # h3('App Subtitle'),
    # Show a plot of the generated distribution

    tabPanel(
      "COVID-19 Load",
      br(),
      h3("Tracking COVID-19 Prevalence with Metro Plant Wastewater"),
      p("The number of reported cases of COVID-19 infections in the seven-county metro area corresponds to the prevalence of the virus in wastewater samples at the Metro treatment plant in Saint Paul. The plant serves a large portion of the seven-county metro area. "),
      plotlyOutput("loadPlot", height = "auto"),
      p("The blue line and points show the total amount of SARS-CoV-2 viral RNA in wastewater flowing into the Metro Plant, in millions copies of the SARS-CoV-2 genome per person served by the wastewater area, per day. Blue points are daily values; the blue line is a running average of the previous 7 days. The gray line shows the average of the previous 7 days of new reported COVID-19 infections in the seven-county Metro area per 100,000 residents. Case data are provided by the Minnesota Department of Health and downloaded from USA Facts (https://usafacts.org).")
      # p("Data last updated 2022-01-13.")
    ),
    tabPanel(
      "COVID-19 Variants",
      br(),
      h3("COVID-19 variant tracker"),
      p("As the Delta variant of the SARS-CoV-2 virus declined, the Omicron variant quickly took its place as the dominant variant in wastewater samples at the Metro treatment plant in Saint Paul. The plant serves a large portion of the seven-county metro area. "),
      radioButtons(inputId = "plotSel", label = "", choices = c("Show as percentages (%)", "Show as number of copies"), selected = "Show as number of copies", inline = T),
      plotlyOutput("variantPlot", height = "auto"),
      p("Points are daily data; lines are averages of the previous 7 days. Alpha, Beta and Gamma frequencies are inferred from the presence of the N501Y mutation; Delta from the L452R mutation; and Omicron from the K417N mutation. Presence of K417N mutation before November 18 were inferred to be the Beta variant and are omitted from this image.")
    ),

    # tabPanel(
    #   "Load vs. Reported Cases",
    #   br(),
    #   h3("A strong relationship between SARS-CoV-2 daily load and reported COVID-19 cases"),
    #   h4("Metro Plant influent SARS-CoV-2 daily load versus new cases in the sewered service area, weekly mean values from 11 April 2021 to present."),
    #   p("Here is some text to explain what is going on."),
    #   plotlyOutput("casesVload", height = "auto"),
    #   p("Data updated ... Data source ... ")
    # ),
    tabPanel(
      "Download Data",
      br(),
      # load -----
      p("Data are divided in two sections: prevalence (load) and cases, and variant frequencies"),
      h3("Prevalence"),
      p("SARS-CoV-2 prevalence in wastewater influent is determined from multiple samples of wastewater each day. Units are in millions of copies of N1 and N2 genes, per person in the sewage treatment area, per day. Viral load data are from Metropolitan Council and the University of Minnesota Genomics Center. Cases are a per-capita (per 100,000 people) 7-day rolling average case rates for the 7-county Metropolitan Council area, provided by the Minnesota Department of Health and downloaded from USA Facts (https://usafacts.org)."),
      br(),
      DTOutput("loadData"),

      # variants -----
      h3("Variants"),
      p("Variant presence and frequency are inferred from the N501Y mutation (Alpha, Beta and Gamma); the L452R mutation (Delta); and the K417N mutation (Omicron). K417N mutations present before November 18, 2020 are assumed to be Beta variants, and are marked as Other in the variant column."),
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
      "2022-04-01",
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
