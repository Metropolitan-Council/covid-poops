

# Server -----
server <- function(input, output) {
  

  # plots ----
  ## load and cases -----
  
  left_axis_text <- list(
    tickfont = list(color = colors$councilBlue,
                    size = 16,
                    family = font_family_list),
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
      color = councilR::colors$councilBlue
    )
  )
  
  right_axis_title <- list(
    x = 1,
    y = 1.1,
    text = "COVID-19 cases<br>per 100K residents",
    xref = "paper",
    yref = "paper",
    showarrow = F,
    align = "right",
    font = list(
      size = 16,
      family = font_family_list,
      color = councilR::colors$suppBlack
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
          x = ~date,
          y = ~covid_cases_7day,
          name = "7-day avg. cases per capita, 7-county metro area",
          fill = "tozeroy",
          fillcolor = "rgba(160, 160, 160, .3)",
          line = list(width = 0.5, color = colors$suppGray),
          hoverinfo = "text",
          text = ~hover_text_case
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
            range = ~c(min(date)-4, max(date) + 4),
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
              color = councilR::colors$suppBlack
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
              color = councilR::colors$suppBlack
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
              color = councilR::colors$suppBlack
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
  output$variantLoadPlot <-
    renderPlotly({
      plot_ly() %>%
        # total load:
        add_trace(
          data = load_data,
          type = "scatter",
          mode = "lines",
          x = ~date,
          fill = "tozeroy",
          y = ~copies_day_person_7day,
          alpha = 0.25,
          line = list(width = 0.5, color = colors$suppGray),
          color = ~"Total load",
          colors = pal,
          name = "Total load",
          hoverinfo = "text",
          text = ~hover_text_load_7day
        ) %>%
        add_trace(
          data = copies_by_variant,
          type = "scatter",
          mode = "lines",
          x = ~date,
          fill = "tozeroy",
          y = ~copies_7day,
          split = ~variant,
          color = ~variant,
          alpha = 0.25,
          colors = pal,
          hoverinfo = "none"
        ) %>%
        add_trace(
          data = copies_by_variant,
          type = "scatter",
          mode = "markers",
          x = ~date,
          y = ~copies,
          split = ~variant,
          color = ~variant,
          alpha = 0.8,
          colors = pal,
          hoverinfo = "text",
          text = ~hover_text_variant,
          showlegend = F
        ) %>%
        layout(
          annotations = list(list(
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
              color = councilR::colors$suppBlack
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
              color = councilR::colors$suppBlack
            )
          ),
          yaxis = list(
            title = list(
              text = "",
              standoff = 0
            ),
            tickfont = list(
              size = 16,
              family = font_family_list,
              color = councilR::colors$suppBlack
            ),
            range = ~c(min(copies_7day)-0.05*max(copies_7day), 1.1*max(copies_7day)),
            gridcolor = colors$suppWhite,
            zerolinecolor = colors$suppWhite,
            zerolinewidth = 2
          ),
          legend = list(
            orientation = "h",
            # y = -0.3,
            xanchor = "left",
            font = list(
              size = 16,
              family = font_family_list,
              color = councilR::colors$suppBlack
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
              color = councilR::colors$suppBlack
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
              color = councilR::colors$suppBlack
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
              color = councilR::colors$suppBlack
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
              color = councilR::colors$suppBlack
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
  ## prevalence table -----
  output$loadData <- renderDT(server = FALSE, {
    load_data %>%
      left_join(case_data,
        by = c(
          "date",
          "covid_cases_total",
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
          "Total COVID cases",
          "New COVID cases",
          "COVID cases per capita",
          "7-day rolling average COVID cases per capita"
        )
      ) %>%
      DT::formatSignif(columns = 2:8, digits = 2) %>%
      DT::formatRound(2:4, 2) %>%
      # round case rates to nearest digit:
      DT::formatRound(5:8, 0)
  })


  ## variant table -----
  output$variantData <- renderDT(server = FALSE, {
    variant_data %>%
      select(
        -hover_text_variant,
        -frequency_gapfill
      ) %>%
      filter(!is.na(frequency)) %>%
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
  
  
  # ## FAQ -----
  # output$faqAnswer1 <- renderUI({ 
  #   str1 <- "Wastewater surveillance measures the levels of viruses and bacteria in wastewater, otherwise known as sewage, and is used to evaluate community infection trends."
  #   str2 <- "With respect to the SARS-CoV-2 virus that causes the COVID-19 disease, wastewater surveillance tells us about the presence and prevalence of the virus shed by people with and without symptoms. By measuring viral levels in untreated wastewater public health officials can determine whether infections are increasing and decreasing in a sewershed, which is the area served by a wastewater treatment plant."
  #   str3 <- "Observing these trends helps public officials make informed policy decisions to reduce and prevent future spread. It also helps the public to make choices about how to ensure their own personal safety."
  #   str4 <-  "Wastewater sampling does not confirm individual cases or provide detailed information about how and where outbreaks occur. But it is an unbiased measure of disease prevalence in a sewer service area."
  #   HTML(paste(str1, str2, str3, str4, sep = '<br/><br/>'))
  #  })
  # 
  # output$faqAnswer2 <- renderUI({ 
  #   str1 <- "In the Twin Cities metro area, the Metropolitan Council’s Environmental Services Division collects and treats wastewater. With the outbreak of COVID-19 pandemic research staff at the largest of the region’s nine plants, the Metro Plant, learned how to test wastewater for the virus by extracting its genetic material from untreated wastewater samples."
  #   str2 <- "The extracted material, RNA (ribonucleic acid), is a remnant from infected people who release the viral particles through their bodily waste. RNA is what allows viral participles to reproduce and spread."
  #   str3 <- "Working in partnership with the University of Minnesota Genomics Center (UMGC) scientists can quantify the viral RNA in samples using advanced analytical instruments. The amount of viral RNA they observe is a measure of the prevalence of COVID-19 in the plant’s service area and indicator of community spread."
  #   str4 <- "The Council reports this information to the Minnesota Department of Health, the University of Minnesota Medical School and the national Wastewater Surveillance System. The Council also posts this information on its website weekly so it’s available to the public, media, and other health officials."
  #   HTML(paste(str1, str2, str3, str4, sep = '<br/><br/>'))
  # })
  
 }
