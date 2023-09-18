# Server -----
server <- function(input, output) {
  # plots ----
  ## load and cases -----

  left_axis_text <- list(
    tickfont = list(
      color = colors$councilBlue,
      size = 16,
      family = font_family_list
    ),
    overlaying = "y",
    side = "left",
    zerolinewidth = 0,
    zerolinecolor = colors$suppWhite,
    gridcolor = colors$suppWhite,
    rangemode = "nonnegative"
  )

  left_axis_title <- list(
    x = -0.05,
    y = 1.1,
    text = "Viral load in wastewater<br>M copies/person/day",
    xref = "paper",
    yref = "paper",
    showarrow = F,
    align = "left",
    font = list(
      size = 16,
      family = font_family_list,
      color = colors$councilBlue
    )
  )

  right_axis_title <- list(
    x = 1,
    y = 1.1,
    text = "New Daily COVID-19 cases<br>per 100K residents",
    xref = "paper",
    yref = "paper",
    showarrow = F,
    align = "right",
    font = list(
      size = 16,
      family = font_family_list,
      color = colors$suppBlack
    )
  )



  output$loadPlot <-
    renderPlotly({
      load_data %>%
        plot_ly(type = "scatter", mode = "lines") %>%
        add_trace(
          mode = "markers",
          x = ~date,
          y = ~copies_day_person_M_mn,
          name = "Viral load, Metro Plant service area",
          size = 1,
          yaxis = "y2",
          marker = list(
            color = "rgba(0, 84, 164, .5)",
            size = 8,
            line = list(
              color = colors$councilBlue,
              width = 0.5
            )
          ),
          hoverinfo = "text",
          text = ~hover_text_load
        ) %>%
        add_trace(
          mode = "lines",
          x = ~date,
          y = ~copies_day_person_7day,
          name = "7-day avg. viral load",
          size = 1,
          yaxis = "y2",
          line = list(width = 2, color = colors$councilBlue),
          hoverinfo = "text",
          text = ~hover_text_load_7day
        ) %>%
        add_trace(
          x = ~ slice(load_data, 1:(n() - 41))$date,
          y = ~ slice(load_data, 1:(n() - 41))$covid_cases_7day,
          name = "7-day avg. cases per capita, Metro Service Area",
          fill = "tozeroy",
          fillcolor = "rgba(160, 160, 160, .3)",
          line = list(width = 0.5, color = colors$suppGray),
          hoverinfo = "text",
          text = ~ slice(load_data, 1:(n() - 41))$hover_text_case
        ) %>%
        add_trace(
          x = ~ slice(load_data, (n() - 40):n())$date,
          y = ~ slice(load_data, (n() - 40):n())$covid_cases_7day,
          name = "7-day avg. cases per capita, Metro Service Area, Incomplete",
          fill = "tozeroy",
          fillcolor = "rgba(160, 160, 160, .8)",
          line = list(width = 0.5, color = colors$suppGray),
          hoverinfo = "text",
          text = ~ slice(load_data, (n() - 40):n())$hover_text_case
        ) %>%
        layout(
          annotations = list(left_axis_title, right_axis_title),
          autosize = T,
          showlegend = TRUE,
          margin = list(t = 50, l = 50, r = 50, b = 10, pad = 0),
          hovermode = "closest",
          hoverdistance = "10",
          hoverlabel = hov_lab_list,
          yaxis2 = left_axis_text,
          xaxis = list(
            range = ~ c(min(date) - 4, max(date) + 4),
            title = list(
              text = "",
              standoff = 0
            ),
            zerolinewidth = 0,
            gridcolor = colors$suppWhite,
            zerolinecolor = colors$suppWhite,
            tickfont = list(
              size = 16,
              family = font_family_list,
              color = colors$suppBlack
            )
          ),
          yaxis = list(
            side = "right",
            title = list(
              text = "",
              standoff = 0
            ),
            zerolinewidth = 0,
            tickfont = list(
              size = 16,
              family = font_family_list,
              color = colors$suppBlack
            ),
            gridcolor = colors$suppWhite,
            zerolinecolor = colors$suppWhite,
            rangemode = "nonnegative"
          ),
          legend = list(
            orientation = "h",
            x = 0, y = -0.15,
            font = list(
              size = 16,
              family = font_family_list,
              color = colors$suppBlack
            )
          )
        ) %>%
        config(
          displayModeBar = "hover",
          displaylogo = FALSE,
          showSendToCloud = FALSE,
          showEditInChartStudio = FALSE,
          modeBarButtonsToRemove = list(
            "lasso2d",
            "zoomIn2d",
            "zoomOut2d"
          )
        )
    })


  ## variant load -----
  base_ggplot <-
    copies_by_variant %>%
    rename(`7-day-average` = hover_text_variant_7day) %>%
    mutate(variant = factor(variant, levels = rev(levels(variant)))) %>%
    ggplot(aes(x = date, y = copies_7day)) +
    geom_area(
      position = "stack",
      aes(
        x = date,
        y = copies_7day,
        color = variant,
        fill = variant,
        label = `7-day-average`
      ),
      alpha = 0.75,
      na.rm = T,
      lwd = 0.25
    ) +
    # solid black line - total covid load
    geom_line(
      data = load_data[load_data$date > min(copies_by_variant$date), ] %>%
        mutate(variant = "Total COVID-19 Load") %>%
        rename(`7-day-average` = hover_text_load_7day),
      aes(
        x = date,
        y = copies_day_person_7day,
        color = variant,
        fill = variant,
        label = `7-day-average`
      ),
      lwd = 0.6
    ) +
    scale_fill_manual(
      values = pal,
      drop = T
    ) +
    scale_color_manual(
      values = pal,
      drop = T
    ) +
    scale_y_continuous(
      name = "",
      labels = scales::unit_format(unit = "M")
    ) +
    scale_x_date(
      name = "Date",
      breaks = "2 months",
      date_labels = "%b '%y"
    ) +
    geom_hline(yintercept = 0, color = "gray30", size = 0.5) +
    theme_void() +
    theme(
      panel.grid.major.x = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_text(),
      axis.text.y = element_text()
    )

  output$variantLoadPlot <-
    renderPlotly({
      ggplotly(base_ggplot, tooltip = c("label")) %>%
        layout(
          hoverlabel = list(
            font = list(
              size = 20,
              family = "Arial Narrow",
              color = colors$suppWhite
            ),
            # bgcolor = "white",
            stroke = list(
              colors$suppGray,
              colors$suppGray,
              colors$suppGray,
              colors$suppGray
            ),
            padding = list(l = 5, r = 5, b = 5, t = 5)
          ),
          margin = list(l = 225, pad = 0),
          legend = list(
            orientation = "h",
            yanchor = "top",
            xanchor = "left",
            title = list(text = ""),
            font = list(
              size = 16,
              family = "Arial Narrow",
              color = colors$suppBlack
            )
          ),
          yaxis = list(
            title = list(
              text = "",
              standoff = 0
            ),
            tickfont = list(
              size = 16,
              family = "Arial Narrow",
              color = colors$suppBlack
            ),
            rangemode = "nonnegative",
            gridcolor = "gray90",
            zerolinecolor = "gray90",
            zerolinewidth = 0
          ),
          xaxis = list(
            title = list(text = ""),
            zeroline = FALSE,
            showline = FALSE,
            showgrid = FALSE,
            tickfont = list(
              size = 16,
              family = "Arial Narrow",
              color = colors$suppBlack
            )
          ),
          annotations = list(
            list(
              x = -0.22,
              y = 0.6,
              text = "Viral load<br>(Million copies<br>per person,<br>per day)",
              xref = "paper",
              yref = "paper",
              showarrow = F,
              align = "center",
              font = list(
                size = 16,
                family = "Arial Narrow",
                color = colors$suppBlack
              )
            )
          )
        ) %>%
        config(
          displayModeBar = "hover",
          displaylogo = FALSE,
          showSendToCloud = FALSE,
          showEditInChartStudio = FALSE,
          modeBarButtonsToRemove = list(
            "lasso2d",
            "zoomIn2d",
            "zoomOut2d"
          )
        )
    })

  ## variant frequency -----
  output$variantFreqPlot <-
    renderPlotly({
      variant_data %>%
        plot_ly() %>%
        add_trace(
          type = "scatter",
          mode = "lines",
          x = ~date,
          fill = "tozeroy",
          y = ~frequency_7day,
          split = ~variant,
          color = ~variant,
          alpha = 0.25,
          colors = pal,
          hoverinfo = "none",
          showlegend = F
        ) %>%
        add_trace(
          type = "scatter",
          mode = "markers",
          x = ~date,
          y = ~frequency,
          split = ~variant,
          color = ~variant,
          alpha = 0.8,
          colors = pal,
          hoverinfo = "text",
          text = ~hover_text_variant
        ) %>%
        layout(
          annotations = list(list(
            x = -0.05,
            y = 1.1,
            text = "Variant frequency (%)<br>indicated by marker genes",
            xref = "paper",
            yref = "paper",
            showarrow = F,
            align = "left",
            font = list(
              size = 16,
              family = font_family_list,
              color = colors$suppBlack
            )
          )),
          hovermode = "closest",
          hoverdistance = "10",
          hoverlabel = hov_lab_list,
          margin = list(t = 50, l = 50, r = 50, b = 10, pad = 0),
          xaxis = list(
            title = list(
              text = "",
              standoff = 0
            ),
            zerolinewidth = 2,
            zerolinecolor = colors$suppWhite,
            zeroline = TRUE,
            showline = FALSE,
            showgrid = FALSE,
            tickfont = list(
              size = 16,
              family = font_family_list,
              color = colors$suppBlack
            )
          ),
          yaxis = list(
            title = list(
              text = "",
              standoff = 0
            ),
            tickformat = "1%",
            tickfont = list(
              size = 16,
              family = font_family_list,
              color = colors$suppBlack
            ),
            gridcolor = colors$suppWhite,
            zerolinecolor = colors$suppWhite,
            zerolinewidth = 0,
            range = c(-0.05, 1.15)
          ),
          legend = list(
            orientation = "h",
            # y = -0.2,
            xanchor = "left",
            font = list(
              size = 16,
              family = font_family_list,
              color = colors$suppBlack
            )
          )
        ) %>%
        config(
          displayModeBar = "hover",
          displaylogo = FALSE,
          showSendToCloud = FALSE,
          showEditInChartStudio = FALSE,
          modeBarButtonsToRemove = list(
            "lasso2d",
            "zoomIn2d",
            "zoomOut2d"
          )
        )
    })


  # tables -----
  ## load table -----
  output$loadData <- renderDT(server = FALSE, {
    load_data %>%
      left_join(case_data,
        by = c(
          "date",
          # "covid_cases_total",
          "covid_cases_new",
          "covid_cases_per100K",
          "covid_cases_7day",
          "hover_text_case"
        )
      ) %>%
      select(
        -hover_text_case, -hover_text_load,
        -hover_text_load_7day
      ) %>%
      arrange(desc(date)) %>%
      DT::datatable(
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          dom = "Btrip",
          buttons = c("copy", "excel", "csv"),
          searching = FALSE,
          lengthMenu = FALSE
        ),
        colnames = c(
          "Date",
          "Viral load in wastewater, M copies/person/day",
          "Standard error of viral load",
          "7-day rolling average viral load",
          # "Total COVID cases",
          "New COVID cases",
          "COVID cases per capita",
          "7-day rolling average COVID cases per capita"
        )
      ) %>%
      DT::formatSignif(columns = 2:7, digits = 2) %>%
      DT::formatRound(2:4, 2) %>%
      # round case rates to nearest digit:
      DT::formatRound(5:7, 0)
  })


  ## variant table -----
  output$variantData <- renderDT(server = FALSE, {
    variant_data %>%
      select(
        -hover_text_variant,
        -frequency_gapfill
      ) %>%
      filter(!is.na(frequency)) %>%
      arrange(desc(date)) %>%
      DT::datatable(
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          dom = "Btrip",
          buttons = c("copy", "excel", "csv"),
          searching = FALSE,
          lengthMenu = FALSE
        ),
        colnames = c(
          "Variant",
          "Date",
          "Frequency",
          "7-day rolling average frequency"
        )
      ) %>%
      DT::formatRound("frequency", 2) %>%
      DT::formatRound("frequency_7day", 2)
  })


  ## case table -----
  output$caseData <- renderDT(server = FALSE, {
    case_data %>%
      select(-hover_text_case) %>%
      arrange(desc(date)) %>%
      DT::datatable(
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          dom = "Btrip",
          buttons = c("copy", "excel", "csv"),
          searching = FALSE,
          lengthMenu = FALSE
        )
      )
  })


  ## FAQ -----
  output$faqAnswer1 <- renderUI({
    str1 <- "Measuring the levels of viruses and pathogens in wastewater (i.e., sewage) can allow communities to quantify and monitor disease trends. By measuring levels of the SARS-CoV-2 virus in untreated wastewater entering a wastewater treatment plant, public health officials can determine whether COVID-19 infections are increasing or decreasing in the area served by that plant. This type of wastewater surveillance is objective and comprehensive in that it detects virus shed by infected people whether they are symptomatic or not and whether they seek testing or health care, or not."
    str2 <- "Monitoring disease trends using wastewater surveillance helps public officials make informed policy decisions to reduce and prevent future spread. It also helps the public to make choices about how to ensure their own personal safety."
    str3 <- "Wastewater surveillance provides an unbiased and comprehensive measure of disease prevalence in a sewer service area, but it does not identify individual cases or provide detailed information about how and where outbreaks occur."
    HTML(paste(str1, str2, str3, sep = "<br/><br/>"))
  })

  output$faqAnswer2 <- renderUI({
    str1 <- "In the Twin Cities metro area, the Metropolitan Council’s Environmental Services (MCES) Division collects and treats wastewater. At the start of the COVID-19 pandemic in 2020, research staff at the Council’s Metro Plant in Saint Paul developed a new laboratory method for extracting SARS-CoV-2 RNA (ribonucleic acid) from untreated wastewater samples."
    str2 <- "The viral RNA is present in all virus particles and is used by the virus to replicate itself inside human cells. Fragments of this RNA are present in the feces of people infected with COVID-19 and flushed into the community’s wastewater collection system."
    str3 <- "Samples containing the viral RNA extracted from wastewater were analyzed by scientists at the University of Minnesota Genomics Center (UMGC) to quantify the viral RNA. The amount of viral RNA they observe is a measure of the prevalence of COVID-19 in the plant’s service area and an indicator of community spread."
    str4 <- "For the period of November 1st, 2020 to August 27, 2023 the Council reported this information weekly to the Minnesota Department of Health and the Governor’s Office and posted this information on its website.  Monitoring of Metropolitan Council’s wastewater continues through the <a href='https://experience.arcgis.com/experience/a8d269bd670a421e9fd45f967f23f13c' target='_blank'> University of Minnesota Medical School wastewater project
            <i class='fa fa-external-link-alt' role='presentation' aria-label='external-link-alt icon'></i></a>"
    HTML(paste(str1, str2, str3, str4, sep = "<br/><br/>"))
  })

  output$faqAnswer3 <- renderUI({
    str1 <- "The Council reported the total amount of viral RNA, or viral load, flowing into the Metro Plant in Saint Paul. This plant treats wastewater from 66 metro-area communities, serving nearly two million people. Because the Metro Plant is so large and serves so many people, it provides a good picture of COVID-19 infections and trends occurring throughout the region."
    str2 <- "The Council also collected data on the virus at three of its other wastewater regional treatment plants. These data, too, were shared with public health authorities to inform their decision-making."
    HTML(paste(paste(str1, str2, sep = "<br/><br/>"), "<br/>"))
  })

  output$faqAnswer4 <- renderUI({
    str1 <- "Viable (infectious) virus has not been detected in treated effluent from wastewater treatment plants. Modern wastewater treatment methods remove the virus before the treated water is discharged to receiving waters."
    HTML(paste(str1, sep = "<br/><br/>"))
  })

  output$faqAnswer5 <- renderUI({
    str1 <- "While SARS-CoV-2 can be shed in the feces of individuals with COVID-19 and discharged into wastewater collection systems, there is no evidence to date of COVID-19 infections arising from direct exposure to treated or untreated wastewater."
    HTML(paste(str1, sep = "<br/><br/>"))
  })

  output$faqAnswer6 <- renderUI({
    str1 <- "The virus that causes COVID-19, SARS-CoV-2, is constantly changing and mutating. A strain of the virus that incorporates new mutations in its RNA is called a variant. Variants you’ve likely heard about include Alpha, Beta, Gamma, Delta, and Omicron, for example. Omicron has had a series of subvariants, including BA.1, BA.2, BA2.12.1, BA.4, and BA.5. These variants and subvariants each have their own transmissibility and immune escape traits and disease severity. The Council tracks the prevalence of these variants and subvariants and makes this information publicly available."
    HTML(paste(str1, sep = "<br/><br/>"))
  })

  output$faqAnswer7 <- renderUI({
    str1 <- "The Council generated daily wastewater data for the period of November 1, 2020 to September 27, 2023. It has transitioned to having all wastewater monitoring accomplished through the National Wastewater Surveillance System and the University of Minnesota Medical School Wastewater Project."
    HTML(paste(str1, sep = "<br/><br/>"))
  })

  output$faqAnswer8 <- renderUI({
    str1 <- "The Council has transitioned to having all wastewater monitoring accomplished through the National Wastewater Surveillance System and the University of Minnesota Medical School Wastewater Project."
    HTML(paste(str1, sep = "<br/><br/>"))
  })

  output$faqAnswer9 <- renderUI({
    str1 <- "The science is still evolving, but we do expect public health agencies to take the lead in monitoring wastewater for other diseases. Monitoring wastewater gives scientists another resource to help inform public health decision-making, and the surveillance of other pathogens and infectious diseases in wastewater will gradually be incorporated into public health maintenance efforts. The U.S. Center for Disease Control and Prevention is very interested in this work and is currently building a national program to pursue it."
    HTML(paste(str1, sep = "<br/><br/>"))
  })

  output$faqAnswer10 <- renderUI({
    str1 <- "The Council and its Environmental Services division do not provide guidance on health-related policy or procedures with respect to COVID-19. That information is conveyed by the Minnesota Health Department and national Center for Disease Control and Prevention."
    HTML(paste(str1, sep = "<br/><br/>"))
  })
}
