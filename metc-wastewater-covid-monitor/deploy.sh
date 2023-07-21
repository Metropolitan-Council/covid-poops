Rscript -e "rsconnect::setAccountInfo(name='metrotransitmn', token='${SHINYAPPSIO_TOKEN}', secret='${SHINYAPPSIO_SECRET}')"
Rscript -e "rsconnect::deployApp(
  appDir = './metc-wastewater-covid-monitor', 
  account = 'metrotransitmn', 
  server = 'shinyapps.io', 
  appName = 'metc-wastewater-covid-monitor', 
  appTitle = 'metc-wastewater-covid-monitor',
  appId = 5437936,
  lint = FALSE,
  forceUpdate = TRUE,
  metadata = list(asMultiple = FALSE, 
                  asStatic = FALSE,
                  ignoredFiles = 'deploy.sh'), 
  logLevel = 'verbose')"
