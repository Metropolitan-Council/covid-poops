

# Server -----
server <- function(input, output) {
  
  
  # variant -----
  output$variantPlot <- renderPlotly({
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
      )  %>%
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
        hoverinfo = "none"
      ) %>%
      layout(
        annotations = ann_list,
        hovermode = "closest",
        hoverdistance = "10",
        hoverlabel = hov_lab_list,
        margin = list(
          l = 50,
          r = 100,
          b = 50,
          pad = 10
        ),
        xaxis = list(
          title = list(
            text = "", standoff = 25,
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
            text = "<b>Frequency of marker genes (%)</b>",
            standoff = 25,
            font = list(
              size = 14,
              family = font_family_list,
              color = councilR::colors$suppBlack
            )
          ),
          tickformat = "1%",
          tickfont = list(
            size = 12,
            family = font_family_list,
            color = councilR::colors$suppBlack
          ),
          gridcolor = "gray90",
          zerolinecolor = "gray50",
          zerolinewidth = 2,
          range = c(0, 1.1)
        ),
        legend = list(
          orientation = "h",
          font = list(
            size = 14,
            family = font_family_list,
            color = councilR::colors$suppBlack
          )
        )
      ) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  
  
  # load -----
  output$loadPlot <- renderPlotly({
    ay <- list(
      tickfont = list(color = colors$esBlue),
      overlaying = "y",
      side = "right",
      title = list(
        text = "<b>Viral load in wastewater,</b> M copies/person/day", standoff = 25,
        font = list(color = colors$esBlue)
      ),
      zerolinewidth = 2,
      zerolinecolor = colors$suppWhite,
      gridcolor = colors$suppWhite
    )
    
    load_plot <-
      load_data %>%
      # left_join(case_data, by = "date") %>%
      plot_ly(type = "scatter", mode = "lines") %>%
      add_trace(
        x = ~date,
        y = ~copies_day_person_M_mn,
        name = "Viral load",
        size = 1,
        yaxis = "y2",
        # fill = "tozeroy",
        # fillcolor = "rgba(0, 154, 199, .5)",
        line = list(width = 2, color = colors$esBlue),
        hoverinfo = "text",
        text = ~hover_text_load
      ) %>%
      add_trace(
        x = ~date,
        y = ~covid_cases_7day,
        name = "Cases per 100,000",
        fill = "tozeroy",
        fillcolor = "rgba(160, 160, 160, .3)",
        line = list(width = 0.5, color = colors$suppGray),
        hoverinfo = "text",
        text = ~hover_text_case
      ) %>%
      layout(
        annotations = ann_list,
        showlegend = FALSE,
        margin = list(l = 50, r = 100, b = 50, pad = 10),
        hovermode = "closest",
        hoverdistance = "10",
        hoverlabel = hov_lab_list,
        yaxis2 = ay,
        xaxis = list(
          title = list(
            text = "Date",
            standoff = 25,
            font = list(
              size = 14,
              family = font_family_list,
              color = councilR::colors$suppBlack
            )
          ),
          zerolinewidth = 2,
          gridcolor = colors$suppWhite,
          zerolinecolor = colors$suppWhite
        ),
        yaxis = list(
          title = list(
            text = "<b>Reported COVID-19 cases,</b> 7-day average", standoff = 25,
            font = list(
              size = 14,
              family = font_family_list,
              color = councilR::colors$suppBlack
            )
          ),
          zerolinewidth = 2,
          tickfont = list(
            size = 12,
            family = font_family_list,
            color = councilR::colors$suppBlack
          ),
          gridcolor = colors$suppWhite,
          zerolinecolor = colors$suppWhite
        ),
        legend = list(
          orientation = "h",
          font = list(
            size = 14,
            family = font_family_list,
            color = councilR::colors$suppBlack
          )
        )
      ) %>%
      config(displayModeBar = F)
    
    load_plot
  })
  
  
  
  # case and load -----
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
          b = 50,
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
          zerolinecolor = "gray50"
        ),
        legend = list(
          font = list(
            size = 14,
            family = font_family_list,
            color = councilR::colors$suppBlack
          )
        )
      ) %>%
      config(displayModeBar = F)
  })
  
  output$loadData <- renderDT(server = FALSE, {
    load_data %>%
      DT::datatable(
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          dom = "Btrip",
          buttons = c("copy", "excel", "csv"),
          searching = FALSE,
          lengthMenu = FALSE
        )
      ) %>%
      DT::formatRound("copies_day_person_M_mn", 2) %>%
      DT::formatRound("copies_day_person_M_mn", 2)
  })
  
  
  output$variantData <- renderDT(server = FALSE, {
    variant_data %>%
      DT::datatable(
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          dom = "Btrip",
          buttons = c("copy", "excel", "csv"),
          searching = FALSE,
          lengthMenu = FALSE
        )
      ) %>%
      DT::formatRound("frequency", 2)
  })
  
  output$caseData <- renderDT(server = FALSE, {
    case_data %>%
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
