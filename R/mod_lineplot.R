#' mod_lineplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' 
#' @noRd 
mod_lineplot_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      mainPanel(
        fluidRow(
          uiOutput(ns("widgets"))
          #uiOutput("widgets")
          
        ),
        plotlyOutput(ns("plot")) #, width = "100%")
        #plotlyOutput("plot") #, width = "100%")
        
      ),
      #style = "align:center; margin-left:250px; margin-right:320px; height:275%; width:100%"
      style = "align:center; height:275%; width:120%"
      
    )
  )
}
    
#' mod_lineplot Server Functions
#' 
#' @param data data.frame input data.
#' @param g_palette character color palette foreach Status or for the whole graph (if of length R).
#' @param status character vaccination status as choice in selectInput.
#'
#' @import dplyr
#' @import shiny
#' @importFrom plotly renderPlotly
#' @importFrom graphics plot.new 
#' 
#' @noRd 
mod_lineplot_server <- function(id,  data, #g_palette = c(rev(VaxCol),NoVaxCol), 
                                status = names(vac_levels())){
  moduleServer( id, function(input, output, session){
    
    #message("Start mod_lineplot_server")
    output$widgets <- renderUI({
      ns <- session$ns
      tagList(
        fluidRow(
          column(5,
                 selectInput(inputId = ns("case"), label = "Hospitalizations or Deaths",
                             choices = c("Hospitalizations","Deaths"), selected = "Hospitalizations")
          ),
          column(5,
                 selectInput(inputId = ns("status"), label = "Vac. Status",
                             choices = c("All",status), selected = "All")
          )
        )#,
        # plotlyOutput(ns("plot"), width = "100%")
      )
    })
    #print plot
    output$plot <- renderPlotly({
      if (is.null(input$case))
        return(plot.new())
      
      message("case: ", input$case)
      message("status: ", input$status)
      dat <- data %>% filter(Case == input$case)
      if (input$status != "All")
        dat <- dat %>%  filter(Status == input$status)
      col_status <- vac_levels_colors()[as.character(unique(dat$Status))] 
      dat_start <- dat %>% group_by(AsOfDate) %>% summarize(sumvalue = sum(Value, na.rm = TRUE)) %>%
        filter(sumvalue > 0) %>% summarize(mindate = min(AsOfDate)) %>% as.data.frame() %>% .[,1]
      dat <- dat %>%
        filter(AsOfDate >= dat_start)
      LinePlotCovid(dat, 
                    FACET = "AgeClass", g_palette = col_status, 
                    title = "Time-line of Vac Status per Age class") })
    
  })
}
