
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covid-poops

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Deploy](https://github.com/Metropolitan-Council/covid-poops/workflows/deploy/badge.svg)](https://github.com/Metropolitan-Council/covid-poops/actions)
<!-- badges: end -->

This repository contains the code for cleaning, transforming and
visualizing data from Environmental Services, whose researchers are
monitoring COVID-19 prevalence in the metro area by analyzing SARS CoV-2
viral RNA load in Metro Plant wastewater.

## Data update schedule

All data used in the interactive Shiny app lives in
`/metc-wastewater-covid-monitor/data`. Though samples are taken daily,
samples are processed and analyzed in the University of Minnesota
Genomics Center lab once a week. We aim to release the weekly data
update every Friday before noon.

## Structure

All processing scripts for data and figures are in ./R. The prefix “d”
indicates data processing, while the prefix “fig” indicates figure
processing. `0_run_all.R` runs all scripts and saves final data.

<PRE class="fansi fansi-output"><CODE>#&gt; <span style='color: #0000BB; font-weight: bold;'>R</span>
#&gt; ├── <span style='color: #00BB00;'>0_run_all.R</span>
#&gt; ├── <span style='color: #00BB00;'>covid-plot-theme.R</span>
#&gt; ├── <span style='color: #00BB00;'>d_copies_by_variant.R</span>
#&gt; ├── <span style='color: #00BB00;'>d_covid_cases.R</span>
#&gt; ├── <span style='color: #00BB00;'>d_load.R</span>
#&gt; ├── <span style='color: #00BB00;'>d_sewershed_map.R</span>
#&gt; ├── <span style='color: #00BB00;'>d_variants.R</span>
#&gt; ├── <span style='color: #00BB00;'>fig_cases_vs_load.R</span>
#&gt; ├── <span style='color: #00BB00;'>fig_copies_by_variant.R</span>
#&gt; ├── <span style='color: #00BB00;'>fig_copies_by_variant_stacked.R</span>
#&gt; ├── <span style='color: #00BB00;'>fig_plotly_cases_vs_load.R</span>
#&gt; ├── <span style='color: #00BB00;'>fig_plotly_variant_frequency.R</span>
#&gt; ├── <span style='color: #00BB00;'>fig_plotly_variant_load.R</span>
#&gt; ├── <span style='color: #00BB00;'>fig_variant_frequency.R</span>
#&gt; └── <span style='color: #00BB00;'>map_sewershed_leaflet.R</span>
</CODE></PRE>

The Shiny app is located in ./metc-wastewater-covid-monitor. /data
contains relevant CSV data and /www contains CSS, HTML, and relevant
font files the app needs upon running.

<PRE class="fansi fansi-output"><CODE>#&gt; <span style='color: #0000BB; font-weight: bold;'>metc-wastewater-covid-monitor/</span>
#&gt; ├── <span style='color: #0000BB; font-weight: bold;'>data</span>
#&gt; ├── deploy.sh
#&gt; ├── <span style='color: #00BB00;'>global.R</span>
#&gt; ├── <span style='color: #0000BB; font-weight: bold;'>rsconnect</span>
#&gt; ├── <span style='color: #00BB00;'>server.R</span>
#&gt; ├── <span style='color: #00BB00;'>ui.R</span>
#&gt; └── <span style='color: #0000BB; font-weight: bold;'>www</span>
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

Special thanks to our community contributors!

## [@adamstener](https://github.com/adamstener), [@ashleyasmus](https://github.com/ashleyasmus), [@eggsurplus](https://github.com/eggsurplus), [@ehesch](https://github.com/ehesch), [@enebo](https://github.com/enebo), [@eroten](https://github.com/eroten), [@JonathanEhrlichMC](https://github.com/JonathanEhrlichMC), [@jpflanigan](https://github.com/jpflanigan), [@knumat](https://github.com/knumat), [@Lex137](https://github.com/Lex137), [@lfletch0025](https://github.com/lfletch0025), and [@riggs](https://github.com/riggs).

<a href="https://metrocouncil.org" target="_blank"><img src="metc-wastewater-covid-monitor/www/main-logo.png" style="margin-left: 50%;margin-right: 50%;">

<div>

</div>

</a>
