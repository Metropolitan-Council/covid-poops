fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "font.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "colors.css"),
    includeHTML("www/google-analytics.html"),
    tags$html(lang = "en")
  ),
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
    tabPanel(
      "COVID-19 Trends",
      br(),
      fluidRow(
        h2("COVID-19 Surveillance at the Metro Plant"),
        tabsetPanel(
          type = "pills",
          tabPanel(
            "Total Viral Load",
            HTML("<h6><section style='font-size:14pt'>This graph shows the amount of SARS-CoV-2 viral RNA entering the Metro Plant each day (blue line) and the number of new daily COVID-19 cases in the Metro Plant's service area, by sample collection date (gray line; data from the Minnesota Department of Health). The most recent case data (darker gray) are incomplete and subject to change.</h3>"),
            HTML("<h6><section style='font-size:12pt;font-style:italic'>Last Sample Date: May 14, 2023. <br>*All data are preliminary and subject to revision</h3>"),
            plotlyOutput("loadPlot", height = "auto")
          ),
          tabPanel(
            "Viral Load by Variant",
            HTML("<h6><section style='font-size:14pt'>This graph shows the estimated amount of SARS-CoV-2 viral RNA by COVID-19 variant, sub-variant and lineage. The total amount of SARS-CoV-2 viral RNA in Metro Plant wastewater influent is shown in the background in gray.</h3>"),
            HTML("<h6><section style='font-size:12pt;font-style:italic'>Last Sample Date: May 14, 2023.  <br>*All data are preliminary and subject to revision</h3>"),
            plotlyOutput("variantLoadPlot", height = "auto")
          ),
          tabPanel(
            "Variant Frequencies (%)",
            HTML("<h6><section style='font-size:14pt'>This graph shows the estimated percent of SARS-CoV-2 viral RNA contributed by COVID-19 variant, sub-variant and lineage.</h3>"),
            HTML("<h6><section style='font-size:12pt;font-style:italic'>Last Sample Date: May 14, 2023. <br>*All data are preliminary and subject to revision</h3>"),
            plotlyOutput("variantFreqPlot", height = "auto")
          ),
          tabPanel(
            "This Week's Summary",
            h6("May 19, 2023"),
            p(
              "The most recent data update includes samples taken May 8 - May 14 2023. During this sampling period:"
            ),
            tags$ul(
              tags$li("The total viral RNA load entering the Metro Plant decreased by 57% last week compared to a week earlier."),
              tags$li("The load is now 84% lower than it was in mid-February and lower than it has been since mid-March 2022."),
              tags$li("XBB is the dominant variant, making up 95% of the viral RNA entering the plant last week.")
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
        "Data are divided in two sections: load (the total amount of virus in wastewater) and cases, and variant frequency (the amount of virus that can be identified as specific variants)."
      ),
      p(
        em("*All data are preliminary and subject to revision.")
      ),
      h3("Total Viral Load"),
      p(
        "SARS-CoV-2 viral load in wastewater influent is determined from multiple samples of wastewater each day. Units are in millions of copies of N1 and N2 genes, per person in the sewage treatment area, per day. Viral load data are from Metropolitan Council and the University of Minnesota Genomics Center. Cases are a per-capita (per 100,000 people) 7-day rolling average case rates for the Metro Plant's service area, by sample collection date (Data from the Minnesota Department of Health). The most recent seven days of case data are incomplete and subject to change."
      ),
      br(),
      DTOutput("loadData"),

      # variants -----
      h3("Variant Frequency"),
      p(
        "Variant presence and prevalence are inferred from the N501Y mutation (Alpha, Beta and Gamma); the L452R mutation (Delta); and the K417N mutation (Omicron). K417N mutations present before November 18, 2020 are assumed to be Beta variants, and are marked as Other in the variant column. The two sub-lineages of Omicron (BA.1 and BA.2) are distinguished by the HV 69/70 deletion: Omicron BA.1 contains both the K417N mutation and the HV 69/70 deletion. Omicron BA.2 has the K417N mutation but not the HV 69/70 deletion. Omicron BA.2.12.1 is distinguished by the L452Q mutation."
      ),
      br(),
      DTOutput("variantData"),

      # # cases -----
      # h3("Cases"),
      # p("Data updated ... Data source ... "),
      # DTOutput("caseData")
    ),
    tabPanel(
      "FAQ",
      br(),
      bsCollapsePanel(
        HTML("<h3><section style='font-size:14pt; font-weight: bold; letter-spacing: 0rem;;'>What can wastewater tell us about COVID-19 infections?</h3>"),
        htmlOutput("faqAnswer1")
      ),
      bsCollapsePanel(
        HTML("<h3><section style='font-size:14pt; font-weight: bold; letter-spacing: 0rem;;'>Who monitors the presence of the virus in wastewater?</h3>"),
        htmlOutput("faqAnswer2")
      ),
      bsCollapsePanel(
        HTML("<h3><section style='font-size:14pt; font-weight: bold; letter-spacing: 0rem;;'>Does wastewater sampling for SARS-CoV-2 cover the entire metro area?</h3>"),
        htmlOutput("faqAnswer3"),
        br(),
        img(
          src = "sampling-area.PNG",
          height = "400px", alt = "Covid Wastewater Sampling Area"
        )
      ),
      bsCollapsePanel(
        HTML("<h3><section style='font-size:14pt; font-weight: bold; letter-spacing: 0rem;;'>Do wastewater treatment plants release the virus into the environment along with treated wastewater?</h3>"),
        htmlOutput("faqAnswer4")
      ),
      bsCollapsePanel(
        HTML("<h3><section style='font-size:14pt; font-weight: bold; letter-spacing: 0rem;;'>Can I get COVID-19 from wastewater/sewage?</h3>"),
        htmlOutput("faqAnswer5")
      ),
      bsCollapsePanel(
        HTML("<h3><section style='font-size:14pt; font-weight: bold; letter-spacing: 0rem;'>What is meant by virus “variants” and “subvariants?”</h3>"),
        htmlOutput("faqAnswer6")
      ),
      bsCollapsePanel(
        HTML("<h3><section style='font-size:14pt; font-weight: bold; letter-spacing: 0rem;;'>How often does the Council update data on the prevalence of the virus?</h3>"),
        htmlOutput("faqAnswer7")
      ),
      bsCollapsePanel(
        HTML("<h3><section style='font-size:14pt; font-weight: bold; letter-spacing: 0rem;;'>Are more frequent data updates available?</h3>"),
        htmlOutput("faqAnswer8")
      ),
      bsCollapsePanel(
        HTML("<h3><section style='font-size:14pt; font-weight: bold; letter-spacing: 0rem;;'>Can wastewater sampling and testing track other infectious diseases?</h3>"),
        htmlOutput("faqAnswer9")
      ),
      bsCollapsePanel(
        HTML("<h3><section style='font-size:14pt; font-weight: bold; letter-spacing: 0rem;;'>Does the Council provide health-related guidance for COVID-19?</h3>"),
        htmlOutput("faqAnswer10")
      )
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
      "2023-05-19",
      # using gh::gh() causes SAML error in production
      # gh::gh("GET /repos/Metropolitan-Council/covid-poops")[49][[1]] %>% as.Date(),
      style = "font-size: 1.5rem;
             display: block;
             text-align: right;
             padding: 1%;"
    )
  )
)
