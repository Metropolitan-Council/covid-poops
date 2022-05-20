

# Server -----
server <- function(input, output) {

  # dynamic output ----
  
  ## select plot -----
  # code here to select whether variantPlot = variantFreqPlot or variantLoadPlot
  output$mainPlot <- renderPlotly({
    if (input$plotSel == "COVID-19 variant frequencies (%)") {
      variantFreqPlot
    } else if (input$plotSel == "COVID-19 load by variant") {
      variantLoadPlot
    } else if (input$plotSel == "Total COVID-19 load") {
      loadPlot
    }
  })
  

  ## select caption ----
  # code here to select whether variantPlot = variantFreqPlot or variantLoadPlot
  output$figCaption <- renderText({
    if (input$plotSel == "COVID-19 variant frequencies (%)") {
      "Points are daily data; lines are averages of the previous 7 days. Alpha, Beta and Gamma frequencies are inferred from the presence of the N501Y mutation; Delta from the L452R mutation; and Omicron from the K417N mutation. Some variants share mutations: the presence of the K417N mutation before November 18 was inferred to be the Beta variant (data not shown). The two sub-lineages of Omicron (BA.1 and BA.2) are distinguished by the HV 69/70 deletion: Omicron BA.1 contains both the K417N mutation and the HV 69/70 deletion. Omicron BA.2 has the K417N mutation but not the HV 69/70 deletion. Omicron BA.2.12.1 is distinguished by the L452Q mutation."
    } else if (input$plotSel == "COVID-19 load by variant") {
      "Points are daily data; lines are averages of the previous 7 days. Alpha, Beta and Gamma frequencies are inferred from the presence of the N501Y mutation; Delta from the L452R mutation; and Omicron from the K417N mutation. Some variants share mutations: the presence of the K417N mutation before November 18 was inferred to be the Beta variant (data not shown). The two sub-lineages of Omicron (BA.1 and BA.2) are distinguished by the HV 69/70 deletion: Omicron BA.1 contains both the K417N mutation and the HV 69/70 deletion. Omicron BA.2 has the K417N mutation but not the HV 69/70 deletion. Omicron BA.2.12.1 is distinguished by the L452Q mutation."
    } else if (input$plotSel == "Total COVID-19 load") {
      "The blue line and points show the total amount of SARS-CoV-2 viral RNA in wastewater flowing into the Metro Plant, in millions copies of the SARS-CoV-2 genome per person served by the wastewater area, per day. Blue points are daily values; the blue line is a running average of the previous 7 days. The gray line shows the average of the previous 7 days of new reported COVID-19 infections in the seven-county Metro area per 100,000 residents. Case data are provided by the Minnesota Department of Health and downloaded from USA Facts (https://usafacts.org)."
    }
  })

  # plots ----
  ## load and cases -----

  ay <- list(
    tickfont = list(color = colors$councilBlue,
                    size = 16,
                    family = font_family_list),
    overlaying = "y",
    side = "left",
    title = list(
      text = "Viral load in wastewater<br>M copies/person/day",
      standoff = 0,
      font = list(color = colors$councilBlue, size = 16)
    ),
    zerolinewidth = 0,
    zerolinecolor = colors$suppWhite,
    gridcolor = colors$suppWhite,
    rangemode = "nonnegative"
  )

  loadPlot <-
    load_data %>%
    # left_join(case_data, by = "date") %>%
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
      # fillcolor = ,
      # line = list(width = 2, color = colors$esBlue),
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
      # fill = "tozeroy",
      # fillcolor = "rgba(0, 154, 199, .5)",
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
      autosize = T,
      annotations = ann_list <- list(
        text = paste(
          "<br>",
          "<i>", "Latest sample date",
          max(c(
            load_data$date,
            variant_data$date
          ), na.rm = T),
          "</i>"
        ),
        font = list(
          size = 16,
          family = font_family_list,
          color = councilR::colors$suppBlack
        ),
        x = 1,
        y = -0.12,
        showarrow = F,
        xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "auto",
        xshift = 0, yshift = -40
      ),
      showlegend = TRUE,
      margin = list(l = 100, r = 50, b = 50, pad = 10),
      hovermode = "closest",
      hoverdistance = "10",
      hoverlabel = hov_lab_list,
      yaxis2 = ay,
      xaxis = list(
        title = list(
          text = "",
          standoff = 40,
          font = list(
            size = 16,
            family = font_family_list,
            color = councilR::colors$suppBlack
          )
        ),
        zerolinewidth = 2,
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
          text = "COVID-19 cases<br>per 100K residents",
          standoff = 25,
          font = list(
            size = 16,
            family = font_family_list,
            color = councilR::colors$suppBlack
          )
        ),
        zerolinewidth = 1,
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


  ## variant load -----
  variantLoadPlot <-
    # browser()
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
      annotations = ann_list,
      hovermode = "closest",
      hoverdistance = "10",
      hoverlabel = hov_lab_list,
      margin = list(
        l = 50,
        r = 50,
        b = 50,
        pad = 10
      ),
      xaxis = list(
        title = list(
          text = "",
          standoff = 25,
          font = list(
            size = 16,
            family = font_family_list,
            color = councilR::colors$suppBlack
          )
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
          text = "Viral load in wastewater<br> M copies/person/day",
          standoff = 10,
          font = list(
            size = 16,
            family = font_family_list,
            color = councilR::colors$suppBlack
          )
        ),
        tickfont = list(
          size = 16,
          family = font_family_list,
          color = councilR::colors$suppBlack
        ),
        rangemode = "nonnegative",
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

  ## variant frequency -----
  variantFreqPlot <-
    # browser()
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
      annotations = ann_list,
      hovermode = "closest",
      hoverdistance = "10",
      hoverlabel = hov_lab_list,
      margin = list(
        l = 50,
        r = 50,
        b = 50,
        pad = 10
      ),
      xaxis = list(
        title = list(
          text = "",
          standoff = 25,
          font = list(
            size = 16,
            family = font_family_list,
            color = councilR::colors$suppBlack
          )
        ),
        zerolinewidth = 0,
        zeroline = F,
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
          text = "Variant frequency (%)<br>indicated by maker genes",
          standoff = 10,
          font = list(
            size = 16,
            family = font_family_list,
            color = councilR::colors$suppBlack
          )
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
        range = c(0, 1.1)
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


  # map ------
  output$map <- leaflet::renderLeaflet({
    pal <- leaflet::colorFactor(c("#D6582A", "#B37F2E", "#0066A3", "#009AC7"),
      domain = sewershed$WWTP
    )
    map <- leaflet::leaflet() %>%
      leaflet::addMapPane("Main", zIndex = 400) %>%
      leaflet::addMapPane(name = "Carto Positron", zIndex = 430) %>%
      leaflet::addProviderTiles("CartoDB.PositronOnlyLabels",
        options = leaflet::leafletOptions(pane = "Carto Positron"),
        group = "Carto Positron"
      ) %>%
      leaflet::addProviderTiles("CartoDB.PositronNoLabels",
        group = "Carto Positron"
      ) %>%
      leaflet::addPolygons(
        data = sewershed,
        fillColor = ~ pal(WWTP),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "",
        fillOpacity = 0.8,
        highlightOptions = leaflet::highlightOptions(
          weight = 5,
          color = "white",
          dashArray = "",
          fillOpacity = 0.6,
          bringToFront = TRUE
        ),
        label = ~WWTP,
        labelOptions = leaflet::labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      leaflet::addLegend(
        data = sewershed,
        title = "Wastewater Treatment Plant Service Area",
        position = "bottomright",
        pal = pal,
        values = ~WWTP,
        opacity = 0.7
      ) %>%
      leaflet.extras::addFullscreenControl(pseudoFullscreen = TRUE)
  })
  
  ## case and load -----
  output$casesVload <- renderPlotly({
    cases_vs_load_plot <-
      # aggregate data to week:
      combined_data %>%
      # scatterplot:
      ggplot(aes(
        x = covid_cases_7day,
        y = copies_day_person_M_mn
        # label = hover_text_predict
      )) +
      geom_point() +
      geom_smooth(
        method = "lm", se = F,
        color = colors$esBlue,
        fill = colors$esBlue
      )
    
    
    ggplotly(cases_vs_load_plot) %>%
      layout(
        annotations = ann_list,
        hovermode = "closest",
        hoverdistance = "10",
        hoverlabel = hov_lab_list,
        margin = list(
          l = 50,
          r = 100,
          b = 75,
          pad = 10
        ),
        xaxis = list(
          title = list(
            text = "COVID Cases, 7 day", standoff = 25,
            font = list(
              size = 14,
              family = font_family_list,
              color = councilR::colors$suppBlack
            )
          ),
          zerolinewidth = 2,
          zeroline = TRUE,
          showline = FALSE,
          showgrid = FALSE,
          tickfont = list(
            size = 12,
            family = font_family_list,
            color = councilR::colors$suppBlack
          )
        ),
        yaxis = list(
          title = list(
            text = "<b>Copies per day per person</b>",
            standoff = 25,
            font = list(
              size = 14,
              family = font_family_list,
              color = councilR::colors$suppBlack
            )
          ),
          # tickformat = "%",
          zerolinewidth = 2,
          tickfont = list(
            size = 12,
            family = font_family_list,
            color = councilR::colors$suppBlack
          ),
          gridcolor = "gray90",
          zerolinecolor = "gray50",
          rangemode = "nonnegative"
        ),
        legend = list(
          font = list(
            size = 14,
            family = font_family_list,
            color = councilR::colors$suppBlack
          )
        )
      ) %>%
      config(displayModeBar = "hover",
             displaylogo = FALSE,
             showSendToCloud = FALSE,
             showEditInChartStudio = FALSE,
             modeBarButtonsToRemove = list("lasso2d",
                                           "zoomIn2d",
                                           "zoomOut2d"))
  })
  
  # tables -----
  ## Prevalence table -----
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
}
