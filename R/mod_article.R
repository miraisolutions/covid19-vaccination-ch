#' mod_article UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny 
mod_article_ui <- function(id){
  ns <- NS(id)
  tagList(
    #fluidRow(htmlOutput(ns("chunk1"))),
    fluidRow(mod_chunk_ui(id = ns("chunk1"))),
    fluidRow(column(12, mod_lineplot_ui(id = ns("lineplot1")))),
    #fluidRow(htmlOutput(ns("chunk2"))),
    fluidRow(mod_chunk_ui(id = ns("chunk2"))),
    fluidRow(column(12, mod_lineplot_ui(id = ns("lineplot2")))),
    #fluidRow(htmlOutput(ns("chunk3")))
    fluidRow(mod_chunk_ui(id = ns("chunk3")))
    
  )
}
    
#' mod_article Server Functions
#' 
#' @import shiny 
#' @import rmarkdown
#' 
#' @noRd 
mod_article_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    # output$chunk1 <-renderUI({
    #   includeHTML(rmarkdown::render("inst/article/Covid19-breakthroughs-ch_chunks_1.Rmd"))
    # }
    # )
    mod_chunk_server("chunk1", path = "inst/article/Covid19-breakthroughs-ch_chunks_1.Rmd")
    mod_lineplot_server(id = "lineplot1", data = DataRoll4W100k %>% filter(AsOfDate %in% last_timeline_weeks),
                    status = names(vac_levels())[grep("fully_vaccinated|not_vaccinated", vac_levels())]
    )
    
    # output$chunk2 <-renderUI({
    #   includeHTML(rmarkdown::render("inst/article/Covid19-breakthroughs-ch_chunks_2.Rmd"))
    # }
    # )
    mod_chunk_server("chunk2", path = "inst/article/Covid19-breakthroughs-ch_chunks_2.Rmd")
    
    mod_lineplot_server(id = "lineplot2", data = DataRoll4W100kratio %>% filter(AsOfDate %in% last_timeline_weeks),
                    status = names(vac_levels())[grep("fully", vac_levels())]
    )
    
    # output$chunk3 <-renderUI({
    #   includeHTML(rmarkdown::render("inst/article/Covid19-breakthroughs-ch_chunks_3.Rmd"))
    # })
    mod_chunk_server("chunk3", path = "inst/article/Covid19-breakthroughs-ch_chunks_3.Rmd")
    
    
  })
}
    
## To be copied in the UI
# mod_article_ui("mod_article_ui_1")
    
## To be copied in the server
# mod_article_server("mod_article_ui_1")
