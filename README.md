
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covid-poops

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

This repository contains the code for cleaning, transforming and
visualizing data from Environmental Services, whose researchers are
monitoring COVID-19 prealence in the metro area by analyzing SARS CoV-2
viral RNA load in Metro Plant wastewater.

## Structure

All processing scripts for data and figures are in ./R. The prefix “d”
indicates data processing, while the prefix “fig” indicates figure
processing. `0_run_all.R` runs all scripts.

<PRE class="fansi fansi-output"><CODE>#&gt; <span style='color: #0000BB; font-weight: bold;'>R</span>
#&gt; ├── <span style='color: #00BB00;'>0_run_all.R</span>
#&gt; ├── <span style='color: #00BB00;'>d_covid_cases.R</span>
#&gt; ├── <span style='color: #00BB00;'>d_load.R</span>
#&gt; ├── <span style='color: #00BB00;'>d_variants.R</span>
#&gt; ├── <span style='color: #00BB00;'>fig_cases_vs_load.R</span>
#&gt; └── <span style='color: #00BB00;'>fig_variants.R</span>
</CODE></PRE>

The Shiny app is located in ./metc-wastewater-covid-monitor. /data
contains relevant CSV data and /www contains CSS, HTML, and relevant
font files the app needs upon running.

<PRE class="fansi fansi-output"><CODE>#&gt; <span style='color: #0000BB; font-weight: bold;'>metc-wastewater-covid-monitor/</span>
#&gt; ├── <span style='color: #0000BB; font-weight: bold;'>data</span>
#&gt; ├── <span style='color: #00BB00;'>global.R</span>
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

[@ashleyasmus](https://github.com/ashleyasmus), and
[@eroten](https://github.com/eroten).

------------------------------------------------------------------------

<a href="https://metrocouncil.org" target="_blank"><img src="metc-wastewater-covid-monitor/www/main-logo.png" style="margin-left: 50%;margin-right: 50%;">

<div>

</div>

</a>
