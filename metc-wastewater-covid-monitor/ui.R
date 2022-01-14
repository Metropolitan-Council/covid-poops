
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
      tags$link(rel = "stylesheet", type = "text/css", href = "colors.css")
    ),
    # Application title
    # h1("Wastewater Treatment COVID-19 Monitoring Dashboard"),
    # h3('App Subtitle'),
    # Show a plot of the generated distribution


    tabPanel(
      "COVID-19 Variants",
      br(),
      h1("Tracking COVID-19 Variants in Metro Plant influent"),
      h2("N501Y (Alpha, Beta, and Gamma), L452R (Delta) and K417N (Omicron) mutation frequencies in Metro Plant influent"),
      p("Here is some text to explain what is going on."),
      plotlyOutput("variantPlot", height = "auto"),
      p("Data updated ... Data source ... ")
    ),
    tabPanel(
      "COVID-19 Load",
      br(),
      h3("Plot title"),
      h4("Plot subtitle"),
      p("Here is some text to explain what is going on."),
      plotlyOutput("loadPlot", height = "auto"),
      p("Data updated ... Data source ... ")
    ),
    tabPanel(
      "Load vs. Reported Cases",
      br(),
      h3("A strong relationship between SARS-CoV-2 daily load and reported COVID-19 cases"),
      h4("Metro Plant influent SARS-CoV-2 daily load versus new cases in the sewered service area, weekly mean values from 11 April 2021 to present."),
      p("Here is some text to explain what is going on."),
      plotlyOutput("casesVload", height = "auto"),
      p("Data updated ... Data source ... ")
    ),
    tabPanel(
      "Download Data",
      br(),
      p("Data are divided in three sections: load, the variant frequencies, and the number of cases in the sewered service area."),

      # load -----
      h3("Load"),
      p("Data updated ... Data source ... "),
      DTOutput("loadData"),

      # variants -----
      h3("Variants"),
      p("Data updated ... Data source ... "),
      DTOutput("variantData"),

      # cases -----
      h3("Cases"),
      p("Data updated ... Data source ... "),
      DTOutput("caseData")
    )
  )
)
