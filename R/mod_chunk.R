#' chunk UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_chunk_ui <- function(id){
  ns <- NS(id)
  tagList(
    htmlOutput(ns("chunk"))
  )
}
    
#' chunk Server Functions
#' @param id, Internal parameters for {shiny}.
#' @param path character, path of rmd file.
#' 
#' @noRd 
mod_chunk_server <- function(id, path){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$chunk <-renderUI({
      includeHTML(rmarkdown::render(path))
      #includeRMarkdown(path)
    })
 
  })
}
    
## To be copied in the UI
# mod_chunk_ui("chunk_ui_1")
    
## To be copied in the server
# mod_chunk_server("chunk_ui_1")
