#' lineplot UI Function
#'
#' @description lineplot ui
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' 
#' @noRd 
lineplot_ui <- function(){
  fluidPage(
    div(
      mainPanel(
        fluidRow(
          uiOutput("widgets")
        ),
        plotlyOutput("plot") #, width = "100%")
      ),
      #style = "align:center; margin-left:250px; margin-right:320px; height:275%; width:100%"
      style = "align:center; height:275%; width:120%"
      
    )
  )
}

#' filter lineplot data function
#' 
#' @param dat data.frame input data.
#' @param status character vaccination status as choice in selectInput.
#' @param case character Hospitalization or Deaths case as choice in selectInput.
#' 
#' @import dplyr
#' 
#' @noRd 
filter_dataplot <- function(dat, status, case) {
  dat <- dat %>% filter(Case == case)
  if (status != "All")
    dat <- dat %>%  filter(Status == status)
  col_status <- vac_levels_colors()[as.character(unique(dat$Status))] 
  dat_start <- dat %>% group_by(AsOfDate) %>% summarize(sumvalue = sum(Value, na.rm = TRUE)) %>%
    filter(sumvalue > 0) %>% summarize(mindate = min(AsOfDate)) %>% as.data.frame() %>% .[,1]
  dat <- dat %>%
    filter(AsOfDate >= dat_start)
  list(data = dat, col_status = col_status)
}
    
#' lineplot Server Functions
#' 
#' @param data data.frame input data.
#' @param status character vaccination status as choice in selectInput.
#'
#' @import dplyr
#' @import shiny
#' @importFrom plotly renderPlotly
#' @importFrom graphics plot.new 
#' 
#' @noRd 
lineplot_server <- function(input, output, session,  data,
                                status = names(vac_levels())){
    #message("Start lineplot_server")
    output$widgets <- renderUI({
      ns <- session$ns
      tagList(
        fluidRow(
          column(5,
                 selectInput(inputId = "case", label = "Hospitalizations or Deaths",
                             choices = c("Hospitalizations","Deaths"), selected = "Hospitalizations",
                             selectize=TRUE)
          ),
          column(5,
                 selectInput(inputId = "status", label = "Vac. Status",
                             choices = c("All",status), selected = "All",
                             selectize=TRUE)
          )
        )#,
        # plotlyOutput("plot"), width = "100%")
      )
    })
    #print plot
    output$plot <- renderPlotly({
      if (is.null(input$case))
        return(plot.new())
      
      message("case: ", input$case)
      message("status: ", input$status)
      # dat <- data %>% filter(Case == input$case)
      # if (input$status != "All")
      #   dat <- dat %>%  filter(Status == input$status)
      # col_status <- vac_levels_colors()[as.character(unique(dat$Status))] 
      # dat_start <- dat %>% group_by(AsOfDate) %>% summarize(sumvalue = sum(Value, na.rm = TRUE)) %>%
      #   filter(sumvalue > 0) %>% summarize(mindate = min(AsOfDate)) %>% as.data.frame() %>% .[,1]
      # dat <- dat %>%
      #   filter(AsOfDate >= dat_start)

      res <- filter_dataplot(data, input$status, input$case)
      LinePlotCovid(res$data, 
                    FACET = "AgeClass", g_palette = res$col_status, 
                    title = "Time-line of Vac Status per Age class") })
}
