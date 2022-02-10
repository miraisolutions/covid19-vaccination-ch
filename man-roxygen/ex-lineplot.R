if (interactive()) {
  #devtools::load_all()
  # example 1st shiny plot
  
  ui <- lineplot_ui()
  
  server <- function(input, output, session) {
    
    data = readRDS("tests/testthat/data/DataRoll4W100k.rds")
    status <- names(vac_levels())[grep("fully_vaccinated|not_vaccinated", vac_levels())]
    
    
    lineplot_server(input, output, session, data = data, status = status)
  }
  shinyApp(ui = ui, server = server)
}

if (interactive()) {
  # example second shiny plot
  ui <- lineplot_ui()
  
  server <- function(input, output, session) {
    
    data = readRDS("tests/testthat/data/DataRoll4W100kratio.rds")
    status <- names(vac_levels())[grep("fully", vac_levels())]
    
    
    lineplot_server(input, output, session, data = data, status = status)
  }
  shinyApp(ui = ui, server = server)
}
