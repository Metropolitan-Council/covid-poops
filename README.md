
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covid-poops

<!-- badges: start -->

[![Deploy](https://github.com/Metropolitan-Council/covid-poops/actions/workflows/deploy.yaml/badge.svg)](https://github.com/Metropolitan-Council/covid-poops/actions/workflows/deploy.yaml)
<!-- badges: end -->

This repository contains the code for cleaning, transforming and
visualizing data from Environmental Services, whose researchers were
monitoring COVID-19 prevalence in the metro area by analyzing SARS CoV-2
viral RNA load in Metro Plant wastewater.

## Data update schedule

The Council generated daily wastewater data for the period of November
1, 2020 to September 27, 2023. It has transitioned to having all
wastewater monitoring accomplished through the National Wastewater
Surveillance System and the University of Minnesota Medical School
Wastewater Project.

[University of Minnesota Medical School Wastewater
Project](https://experience.arcgis.com/experience/a8d269bd670a421e9fd45f967f23f13c?data_id=dataSource_1-regions_wwtp_view_6302%3A2l)

[National Wastewater Surveillance
System](https://covid.cdc.gov/covid-data-tracker/#wastewater-surveillance)

[National Wastewater Surveillance System
Dataset](https://data.cdc.gov/Public-Health-Surveillance/NWSS-Public-SARS-CoV-2-Wastewater-Metric-Data/2ew6-ywp6)

## Structure

All processing scripts for data and figures are in ./R. The prefix “d”
indicates data processing, while the prefix “fig” indicates figure
processing. `0_run_all.R` runs all scripts and saves final
data.

<PRE class="fansi fansi-output"><CODE>#&gt; <span style='color: #0000BB; font-weight: bold;'>R</span>
#&gt; +-- <span style='color: #00BB00;'>0_run_all.R</span>
#&gt; +-- <span style='color: #00BB00;'>covid-plot-theme.R</span>
#&gt; +-- <span style='color: #00BB00;'>d_copies_by_variant.R</span>
#&gt; +-- <span style='color: #00BB00;'>d_covid_cases.R</span>
#&gt; +-- <span style='color: #00BB00;'>d_load.R</span>
#&gt; +-- <span style='color: #00BB00;'>d_sewershed_map.R</span>
#&gt; +-- <span style='color: #00BB00;'>d_variants.R</span>
#&gt; +-- <span style='color: #00BB00;'>d_variants_umgc.R</span>
#&gt; +-- <span style='color: #00BB00;'>fig_cases_vs_load.R</span>
#&gt; +-- <span style='color: #00BB00;'>fig_copies_by_variant.R</span>
#&gt; +-- <span style='color: #00BB00;'>fig_copies_by_variant_stacked.R</span>
#&gt; +-- <span style='color: #00BB00;'>fig_plotly_cases_vs_load.R</span>
#&gt; +-- <span style='color: #00BB00;'>fig_plotly_variant_frequency.R</span>
#&gt; +-- <span style='color: #00BB00;'>fig_plotly_variant_load.R</span>
#&gt; +-- <span style='color: #00BB00;'>fig_variant_frequency.R</span>
#&gt; +-- <span style='color: #00BB00;'>map_sewershed_leaflet.R</span>
#&gt; \-- <span style='color: #00BB00;'>sharepointfilepath.R</span>
</CODE></PRE>

The Shiny app is located in ./metc-wastewater-covid-monitor. /data
contains relevant CSV data and /www contains CSS, HTML, and relevant
font files the app needs upon
running.

<PRE class="fansi fansi-output"><CODE>#&gt; <span style='color: #0000BB; font-weight: bold;'>metc-wastewater-covid-monitor/</span>
#&gt; +-- <span style='color: #0000BB; font-weight: bold;'>data</span>
#&gt; +-- deploy.sh
#&gt; +-- <span style='color: #00BB00;'>global.R</span>
#&gt; +-- <span style='color: #00BB00;'>server.R</span>
#&gt; +-- <span style='color: #00BB00;'>ui.R</span>
#&gt; \-- <span style='color: #0000BB; font-weight: bold;'>www</span>
</CODE></PRE>

## License

This project is shared freely under the MIT License. See
[LICENSE.md](LICENSE.md)

## Code of Conduct

Please note that the covid-poops project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Contributors

Special thanks to our community
contributors\!

## [@adamstener](https://github.com/adamstener), [@ashleyasmus](https://github.com/ashleyasmus), [@eggsurplus](https://github.com/eggsurplus), [@ehesch](https://github.com/ehesch), [@enebo](https://github.com/enebo), [@eroten](https://github.com/eroten), [@JonathanEhrlichMC](https://github.com/JonathanEhrlichMC), [@jpflanigan](https://github.com/jpflanigan), [@knumat](https://github.com/knumat), [@Lex137](https://github.com/Lex137), [@lfletch0025](https://github.com/lfletch0025), and [@riggs](https://github.com/riggs).

<a href="https://metrocouncil.org" target="_blank"><img src="metc-wastewater-covid-monitor/www/main-logo.png" style="margin-left: 50%;margin-right: 50%;">

<div>

</div>

</a>
