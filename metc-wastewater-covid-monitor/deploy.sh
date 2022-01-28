Rscript -e "rsconnect::setAccountInfo(name='metrotransitmn', token='${SHINYAPPSIO_TOKEN}', secret='${SHINYAPPSIO_SECRET}')"
Rscript -e "rsconnect::deployApp(
  appDir = './metc-wastewater-covid-monitor', 
  account = 'metrotransitmn', 
  server = 'shinyapps.io', 
  appName = 'test-metc-wastewater-covid-monitor', 
  appTitle = 'test-metc-wastewater-covid-monitor',
  lint = FALSE,
  metadata = list(asMultiple = FALSE, 
                  asStatic = FALSE,
                  ignoredFiles = 'deploy.sh'), 
  logLevel = 'verbose')"
