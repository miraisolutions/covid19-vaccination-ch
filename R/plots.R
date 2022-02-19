
# .getdg_lab <- function(dg, maxv, minxv) {
#   if (dg > 3)
#     dglab <- 0
#   else if (dg == 1 && maxv <= 1 && minxv >= 0)
#     dglab <- 1
#   else if (dg > 0)
#     dglab <- dg - (c(-2, 0, 2))[dg]
#   else stop("error wrong digits for rounding", dg)
#   dglab
# }

#' Labels for numeric vector
#'
#' @param x numeric vector
#'
#' @noRd
.getdg_lab_vect <- function(x) {
  dg = nchar(as.character(round(abs(x))))
  dglab <- ifelse(dg >= 2, 0, 
                  ifelse(
                    dg > 2 , 1, 
                    ifelse(dg > 1,  1, 
                           ifelse(dg == 1 & x > 0.1,  2, 3)
                    )
                  ))
  dglab
}

#' Round number
#'
#' @param y numeric vector
#'
#' @noRd
.roundlab <- function(y) {
  maxy <- max(y, na.rm = T)
  minxy <- min(y, na.rm = T)
  dglab <- .getdg_lab_vect(y)
  round(y, dglab)
}
#' Formats numbers with thousands separator or with %
#'
#' @param x numeric vector
#' @param perc logical if TRUE then %
#' @param digits integer, if perc == TRUE then controls the rounding of the figure
#' 
#' @importFrom stringr str_split
#'
#' @noRd
.funformat <- function(x, perc, digits = NULL) {
  if (!perc) {
    y <- as.character(.roundlab(x))
    y_split <- str_split(y, pattern = "[.]")
    y_left <- sapply(y_split, function(z) z[[1]])
    
    y_left <- formatC(as.numeric(y_left), format = "f", big.mark = "'", digits = 0)
    y_right = sapply(y_split, function(z) {
      if (length(z) == 1)
        ""
      else
        z[[2]]
    })
    y_res <- paste(y_left, y_right, sep = ".")
    gsub("\\.$", "", y_res)
  }
  else paste0(round(x, ifelse(is.null(digits), 2, digits)), "%")
}

#' Labels for X Y Axis when percentage
#'
#' @param x numeric vector
#'
#' @noRd
.lab_percent <- function(x) {
  maxx <- max(x, na.rm = TRUE)
  dg <- nchar(as.character(round(maxx)))
  digit <- 1
  if (dg == 1)
    digit <- 2
  if (diff(range(x, na.rm = TRUE)) > 20)
    digit <- 0
  paste0(round(x, digit), "%")
}
#' Labels for X Y Axis when percentage, wrapper of .lab_percent, multiplies by 100
#'
#' @param x numeric vector
#'
#' @noRd
.lab_percent100 <- function(x) {
  x <- 100*x
  .lab_percent(x)
}
#' Labels for X Y Axis when numeric
#'
#' @param x numeric vector
#' @importFrom scales label_number
#' @importFrom stats median
#'
#' @noRd
.lab_num <- function(x) {
  mx <- median(x, na.rm = TRUE)
  thausands <- ifelse(mx > 7500 & mx <= 750000, TRUE, FALSE)
  millions <- ifelse(mx > 750000, TRUE, FALSE)
  if (!is.na(millions) && millions) {
    x <- round(x/1e+06, 2)
    suffix <- "M"
  }
  else if (!is.na(thausands) && thausands) {
    x <- round(x/1000, 2)
    suffix <- "K"
  }
  accy <- ifelse(diff(range(x, na.rm = TRUE)) < 0.05, 0.001,
                 ifelse(diff(range(x, na.rm = TRUE)) < 1, 0.01,
                        ifelse(max(x, na.rm = TRUE) <= 10, 0.1,
                               ifelse(max(x, na.rm = TRUE) <= 100, 1,
                                      ifelse(max(x, na.rm = TRUE) <= 1000, 10,
                                             ifelse(max(x, na.rm = TRUE) <= 10000, 100, 1000))))))
  .labnumfun <- label_number(accuracy = accy, big.mark = "'")
  if ((!is.na(thausands) && !is.na(thausands)) && (thausands || millions)) {
    x <- paste(.labnumfun(x), suffix)
  }
  else {
    x <- .labnumfun(x)
  }
  x
}
#' breaks for X Y Axis
#'
#' @param x numeric vector
#' @param breaks integer number of breaks
#'
#' @noRd
.breaks_lab <- function(x, breaks) {
  x.d.lim <- range(x, na.rm = TRUE)
  x.d.breaks <- seq(x.d.lim[1], x.d.lim[2], length.out = breaks)
  x.d.breaks
}
#' generic plot theme
#'
#' @param facet logical if TRUE then the graph is facet
#' 
#' @import ggplot2
#'
#' @noRd
.basic_plot_theme <- function(facet = TRUE) {

  .sizetext <- function(facet) {
    ifelse(facet, 9, 10)
  }
    
  theme(
    plot.title = element_text(color = "grey45", size = 12,
                              face = "bold.italic", hjust = 0.5),
    text = element_text(size = 10),
    #title = element_text(size = 12),
    panel.background = element_rect(fill = "grey90"),
    panel.grid.major = element_line(colour = "white", size = 0.3),
    panel.grid.minor = element_line(colour = "white", size = 0.1),
    # panel.spacing.x = unit(1, "points"),
    panel.spacing.x = unit(2, "lines"),
    line = element_line(size = 2.2),
    axis.line.x = element_line(color = "grey45", size = 0.5),
    axis.line.y = element_line(color = "grey45", size = 0.5),
    axis.text.x = element_text(size = .sizetext(facet),
                               angle = 30, hjust = 1),
    axis.text.y = element_text(size = .sizetext(facet)),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = .sizetext(facet)),
    legend.key = element_rect(fill = alpha("white", 0)),
    legend.justification = "center"
  )
}

#' Calculates Y limits for graph
#'
#' @param df data.frame
#' @param perc logical TRUE if Value is %
#'
#' @noRd
.y_lim <- function(df, perc) {
  minv <- min(df$Value, na.rm = TRUE)
  ylim_bottom <- ifelse(minv < 0, minv*1.05, 0)
  y_min <- c(ifelse(perc, 0, ylim_bottom))
  ylim <- c(y_min, max(df$Value, na.rm = TRUE)*1.05)
  ylim
}

#' length breaks of x axis
.breaks.xaxis <- 8
#' length breaks of y axis
.breaks.yaxis <- 6

#' Plot legend parameters
#'
#' @param facet logical if TRUE the graph is facet, text size will be smaller
#'
#' @noRd
legend_pars <- function(facet) {
  list(
    title = "",
    font = list(
      family = "sans-serif",
      size = ifelse(facet, 9, 10),
      color = "#000"),
    bgcolor = "#E2E2E2",
    bordercolor = "#FFFFFF",
    borderwidth = 2,
    orientation = 'h', #, y = 1.2
    x = 0.5,
    xanchor = "center",
    yanchor = "top",
    clickmode = "event"
  )
}

#' Barplot function
#'
#' @param df data.frame data
#' @param X character, variable name in X axis
#' @param FACET character, variable name for the facets
#' @param percent logical if TRUE then data and labels are in %
#' @param g_palette character vector of colors for the graph
#' @param percent logical if TRUE then data and labels are in %
#' @param position character position of barplot, fill
#' @param title character
#' 
#' @import ggplot2
#' @importFrom plotly ggplotly layout style
#' @importFrom scales pretty_breaks
#' 
#' @noRd
BarplotCovid <- function(df, X, FACET, percent = FALSE, g_palette, position = "fill", title = "") {
  
  if (nrow(df) == 0 || all(is.na(df$Value))) {
    p <- ggplot()
    return(p)
  }
  if (percent) {
    df$Value <- 100*df$Value
  }
  
  ylim = .y_lim(df, percent)
  
  .popuptext <- function(asofdate, xvarexpr, percent, digits = NULL, text) {
    txt <- paste(
      paste("AsOfDate: ", asofdate, "<br>"),
      paste0("Value: ", .funformat(xvarexpr, percent, digits), "<br>"),
      sep = ""
    )
    if (!missing(text))
      txt <- paste(txt, text, sep = "")
    txt
  }
  
  avgVal <- mean(df$Value, na.rm = TRUE)
  #
  barplotfacet <- ifelse(missing(FACET), FALSE, TRUE)
  
  percentLab <- ifelse(percent, TRUE, FALSE)
  
  if (percent) {
    digits <- 2
  } else  {
    digits <-NULL
  }
  
  if (barplotfacet && (length(g_palette) > 1))
    g_palette = rep(g_palette, length(unique(df[[FACET]])))
  
  p <- ggplot(df, aes(x = .data[[X]], y = Value, group = 1,
                      text = .popuptext(AsOfDate, Value, percent, digits))) +
    geom_bar(stat = "identity", fill = g_palette)
  
  if (barplotfacet) {
    scale <- ifelse(percentLab, "fixed", "free_y")
    
    p <- p + facet_wrap(~ get(FACET), scales = scale)
    
  } else {
    if (position != "stack")
      p <- p +
        coord_cartesian(ylim = ylim)
  }
  p <- p +
    .basic_plot_theme(facet = barplotfacet) +
    # theme(panel.background = element_rect(fill = "grey90")) + # set grey background
    # theme(
    #   axis.text.x = element_text(angle = labangle, size = labsize)
    # ) +
    ggtitle(title) #+
  #xlab(unique(df$AsOfDate))
  
  labfun <- ifelse(percentLab, .lab_percent, .lab_num)
  labfun <- ifelse(position == "fill" && percentLab, .lab_percent100, labfun)
  if (barplotfacet)
    #p <- p + scale_y_continuous(labels = labfun, n.breaks = .breaks.yaxis)
    p <- p + scale_y_continuous(labels = labfun, breaks = pretty_breaks(.breaks.yaxis))
  else
    p <- p + scale_y_continuous(labels = labfun, breaks = .breaks_lab(ylim, .breaks.yaxis))
  
  deltaIncr <- diff(ylim) / 50
  
  traces <- c("keep")
  
  if (barplotfacet)
    traces <- rep("keep", length(unique(df[[FACET]])))
  
  if ((!barplotfacet)) {
    
    p <- p +
      annotate("segment",
               x = 0.5, xend = nrow(df) + 0.5, y = avgVal, yend = avgVal,
               linetype = "dotted", size = 0.3) +
      annotate("text", x = nrow(df), y = avgVal + deltaIncr, label = "Avg",
               size = 1.5, group = 3, hjust = 1)
    traces <- c(traces, c("remove", "remove"))
  }
  
  if (length(unique(df[[X]])) < 10) {# add text if there is space, not working due to plotly
    if (!barplotfacet) {
      p <- p +
        annotate("text", x = df[[X]], y = df$Value + deltaIncr,
                 label = .funformat(df$Value, percent, digits),
                 size = 2.4, vjust = -0.5, group = 2)
      
      traces <- c(traces, c("remove"))
      # s
      #traces <- c(2, traces+1)
      
    } else {
      df2 <- df %>% group_by(!!sym(FACET)) %>% summarize(maxval = max(Value, na.rm = TRUE)*1.05)
      df2$TextHigh <- (df2$maxval - ylim[1])/100
      df <- df %>% left_join(df2, by = FACET)
      df$TextHigh <- df$Value + df$TextHigh
      p = p + 
        geom_text(data = df,
                  mapping = aes(x = .data[[X]], y = TextHigh,
                                label = .funformat(Value, percent, digits)),
                  size = 2.4, vjust = -0.5, group = 2)
      # skip after facet
      traces <- c(traces, rep("remove", nrow(df2)))
    }
    
  }
  
  showLegend <- FALSE
  
  # p +
  # theme(aspect.ratio=3/4)
  pply <- p %>% plotly::ggplotly(tooltip = c("x", "text"),
                                 layerData = 1,
                                 #dynamicTicks = TRUE,
                                 #textposition = 'outside'
                                 originalData = FALSE
  )
  
  # if (any(traces == "remove"))
  #   pply <- pply %>% plotly::style(hoverinfo = "skip", traces = which(traces == "remove"))
  pply <- pply %>%
    #plotly::config(displayModeBar = F)  %>%
    #plotly::style(hoverinfo = "text") %>%
    plotly::layout(
      hovermode = 'closest', clickmode = 'none', #clickmode = "event",
      #xaxis = list(autorange = TRUE, fixedrange = TRUE),
      showlegend = showLegend,
      legend = legend_pars(barplotfacet),
      dragmode = FALSE,
      # xaxis = list(#zerolinewidth = 4., 
      #              #autorange = TRUE, 
      #             fixedrange = TRUE),
      yaxis = list(#fixedrange = TRUE, 
                   zerolinewidth = 4)
    )
  if (any(traces == "remove"))
    pply <- pply %>% plotly::style(hoverinfo = "skip", traces = which(traces == "remove"))
  
  pply
}

#' Stacked Barplot function
#'
#' @param df data.frame data
#' @param X character, variable name in X axis
#' @param FILL character, variable name for position fill
#' @param FACET character, variable name for the facets
#' @param percent logical if TRUE then data and labels are in %
#' @param g_palette character vector of colors for the graph
#' @param percent logical if TRUE then data and labels are in %
#' @param position character position of barplot, fill
#' @param title character
#' 
#' @import ggplot2
#' @importFrom plotly ggplotly layout style
#' @import dplyr
#' @importFrom scales pretty_breaks
#' 
#' @noRd
StackedBarplotCovid <- function(df, X, FILL, FACET, percent = FALSE, g_palette,
                                position = "fill", title = "") {
  
  if (nrow(df) == 0 || all(is.na(df$Value))) {
    p <- ggplot()
    return(p)
  }
  if (percent) {
    df$Value <- 100*df$Value
  }

  ylim = .y_lim(df, percent)
  
  .popuptext <- function(asofdate, xvarexpr, percent, digits = NULL, text, perc, percprint = TRUE) {
    txt <- paste(
      paste("AsOfDate: ", asofdate, "<br>"),
      paste0("Value: ", .funformat(xvarexpr, percent, digits), "<br>"),
      sep = ""
    )
    if (percprint)
      txt <- paste(txt, paste0("Percentage: ", .funformat(perc, TRUE, 1), "<br>"), sep = "")
    
    if (!missing(text))
      txt <- paste(txt, text, sep = "")
    txt
  }
  
  avgVal <- mean(df$Value)
  
  barplotfacet <- ifelse(missing(FACET), FALSE, TRUE)
  
  percentLab <- ifelse(percent, TRUE, FALSE)
  
  if (barplotfacet)  {
    groupvars =  c(X, FACET)
  } else  {
    groupvars = X
  }
  # calculate percentages for tooltips
  df <- df %>% group_by(across(all_of(groupvars))) %>% 
    mutate(Percentage = Value/sum(Value, na.rm = TRUE)*100) %>%
    ungroup()
  
  if (percent) {
    digits <- 2
  } else  {
    digits <-NULL
  }
  # percentages not to be in popup text if percent = TRUE and if it is not dodge
  percprint = ifelse(!percent && position != "dodge", TRUE, FALSE)
  
  p <- ggplot(df, aes(x = .data[[X]], y = Value, fill = .data[[FILL]], 
                      text = .popuptext(AsOfDate, Value, percent, digits, 
                                        perc = Percentage, percprint = percprint)),
              color = "black"
  ) +
    geom_col(position = position, na.rm = TRUE) +
    scale_fill_manual(values = c(g_palette))
  
  if (position %in% c("fill")) {
    ylim = c(0, 1)
    percentLab = TRUE
  } else if (position %in% c("stack") && percent) {
    ylim = c(0, 100)
  }
  
  if (barplotfacet) {
    scale <- ifelse(percentLab, "fixed", "free_y")
    
    p <- p + facet_wrap(~ get(FACET), scales = scale)
    if (position != "fill") {
      p <- p + geom_blank()
    } else {
      p <- p +
        coord_cartesian(ylim = ylim)
    }
  } else {
    if (position != "stack")
      p <- p +
        coord_cartesian(ylim = ylim)
  }
  p <- p +
    .basic_plot_theme(facet = barplotfacet) +
    # theme(panel.background = element_rect(fill = "grey90")) + # set grey background
    # theme(
    #   axis.text.x = element_text(angle = labangle, size = labsize)
    # ) +
    ggtitle(title) 
  
  labfun <- ifelse(percentLab, .lab_percent, .lab_num)
  labfun <- ifelse(position == "fill", .lab_percent100, labfun)
  
  if (barplotfacet) {
    # p <- p + scale_y_continuous(labels = labfun, n.breaks = .breaks.yaxis)
    # try in this way for facet
    p <- p + scale_y_continuous(labels = labfun, breaks = pretty_breaks(.breaks.yaxis))
  } else
    p <- p + scale_y_continuous(labels = labfun, breaks = .breaks_lab(ylim, .breaks.yaxis))
  
  deltaIncr <- diff(ylim) / 100
  
  traces <- c("keep")
  
  if (barplotfacet)
    traces <- rep("keep", length(unique(df[[FACET]])))
  
  if (length(unique(df[[X]])) < 10) {# add text if there is space, not working due to plotly
    if (!barplotfacet) {
      
      if (position != "fill") {
        df2 <- df %>% group_by(!!sym(X)) %>%
          mutate(Value = cumsum(Value)) %>%
          ungroup()
        # increase for Partially vac
        df2$Value[df2[[FILL]] == levels(df2[[FILL]])[1]] = 
          df2$Value[df2[[FILL]] == levels(df2[[FILL]])[1]] * 1.015
        df$ValueText = df$Value
        df <- df %>% group_by(!!sym(X)) %>%
          #arrange(!!sym(FILL)) %>% 
          mutate(ValueText = cumsum(ValueText)) %>%
          ungroup()
        p <- p +
          annotate("text", x = df2[[X]], y = df2$Value + deltaIncr,
                   label = .funformat(df$ValueText, percent, digits),
                   size = 2.4, vjust = -0.3, group = 2)
        traces_fill = rep("keep", length(unique(df[[FILL]])))
        traces_fill[length(traces_fill)] = "remove"                  
        traces <- c(traces, traces_fill)
        
      } 
    }
  }
  
  showLegend <- TRUE
  
  pply <- p %>% plotly::ggplotly(tooltip = c("x", "fill", "text"),
                                 layerData = 1,
                                 #dynamicTicks = TRUE,
                                 #textposition = 'outside'
                                 originalData = FALSE)
  if (any(traces == "remove"))
    pply <- pply %>% plotly::style(hoverinfo = "skip", traces = which(traces == "remove"))
  
  pply <- pply %>%
    #plotly::config(displayModeBar = F)  %>%
    plotly::layout(
      hovermode = 'closest', clickmode = "none", #event
      #xaxis = list(autorange = TRUE), 
      showlegend = showLegend,
      legend = legend_pars(barplotfacet),
      dragmode = FALSE,
      # xaxis = list(#zerolinewidth = 4., 
      #              #autorange = TRUE, 
      #              fixedrange = TRUE),
      yaxis = list(#fixedrange = TRUE, 
                   zerolinewidth = 4)
    )

  
  pply

}

#' Line Plot function
#'
#' @param df data.frame data
#' @param FACET character, variable name for the facets
#' @param percent logical if TRUE then data and labels are in %
#' @param g_palette character vector of colors for the graph
#' @param percent logical if TRUE then data and labels are in %
#' @param title character
#' 
#' @import ggplot2
#' @importFrom plotly ggplotly layout
#' @import dplyr
#' @importFrom scales pretty_breaks
#' 
#' @noRd
LinePlotCovid <- function(df, FACET = "AgeClass", g_palette, percent = FALSE,
                          title = "Time-line of Records per Age class") {
  
  if (nrow(df) == 0 || all(is.na(df$Value))) {
    p <- ggplot()
    return(p)
  }
  if (percent) {
    df$Value <- 100*df$Value
  }
  
  # df <- df %>% group_by(!!sym(FACET)) %>%
  #   summarize(nacheck = all(is.na(Value)))

  ylim = .y_lim(df, percent)
  
  .popuptext <- function(asofdate, xvarexpr, status, percent, digits = NULL, text) {
    txt <- paste(
      paste("AsOfDate: ", asofdate, "<br>"),
      sep = ""
    )
    if (!missing(status))
      txt <- paste(txt, paste("Status: ", status, "<br>"), sep = "")
    txt <- paste(txt, paste0("Value: ", .funformat(xvarexpr, percent, digits), "<br>"), sep = "")
    
    if (!missing(text))
      txt <- paste(txt, text, sep = "")
    txt
  }
  
  percentLab <- ifelse(percent, TRUE, FALSE)
  scale <- ifelse(percentLab, "fixed", "free_y")
  
  if (percent) {
    digits <- 2
  } else  {
    digits <-NULL
  }
  
  .ylabfun = ifelse(percent, .lab_percent, .lab_num)
  
  df$Week  <- rep(1:length(table(df$AsOfDate)), times = table(df$AsOfDate))
  
  xbreaks = seq(to = length(unique(df$AsOfDate)), 
                length = .breaks.xaxis, 
                by = round(length(unique(df$AsOfDate))/.breaks.xaxis))
  xbreaks = xbreaks[xbreaks>=1]
  
  xlabels = unique(df$AsOfDate)[xbreaks]
  xbreaks = unique(df$Week)[xbreaks]
  
  if ("Status" %in% names(df))
    p <- ggplot(df, aes(x = Week, y = Value,  color = Status, group = Status ,
                        text = .popuptext(AsOfDate, Value, Status, percent, digits))
                #color = "black"
    ) 
  else 
    p <- ggplot(df, aes(x = Week, y = Value, 
                        text = .popuptext(AsOfDate, Value, percent = percent, digits = digits))
                #color = g_palette
    )   
  
  if (length(g_palette)>1)
    p <- p +
      geom_line() +
      geom_point(size = 1)
  else
    p <- p +
      geom_line(col = g_palette) +
      geom_point(size = 1, col = g_palette)
  
  p <- p +
    facet_wrap(~ get(FACET), scales = scale, shrink = FALSE, ncol = 2)

  p <- p +
    .basic_plot_theme(facet = TRUE) +
    scale_color_manual(values = g_palette) +
    scale_y_continuous(labels = .ylabfun, breaks = pretty_breaks(.breaks.yaxis)) +
    scale_x_continuous(labels = xlabels, breaks = xbreaks) + 
    theme(#panel.background = element_rect(fill = "grey90"), 
          panel.spacing.x = unit(0.25, "lines"),
    ) + # set grey background
    # theme(
    #   axis.text.x = element_text(angle = labangle, size = labsize),
    #   legend.position = "top"
    # ) +
    ggtitle(title)
  
  weeks_line <- weeks_to_date(unique(df$AsOfDate), range = FALSE)
  # Months
  full_lines_order <- intersect(substring(weeks_line, 5,8), 
                               paste0("-",c(rep(0,4), c("","")), seq(2,12,2), "-"))
  full_lines_char <- sapply(paste0("-",c(rep(0,4), c("","")), seq(2,12,2), "-"), function(x)
    grep(x,weeks_line, value = TRUE)[1]
  )
  full_lines_char <- full_lines_char[full_lines_order]
  
  full_lines <- unique(df$Week)[as.character(weeks_line) %in% full_lines_char]
  
  full_lines_label = as.character(lubridate::month(full_lines_char, label = TRUE))
  
  data_line = data.frame(x = full_lines, y = Inf, lab = full_lines_label)
  data_line0 = data_line
  
  for (aclass in unique(df[[FACET]])[-1])
    data_line <- bind_rows(data_line,data_line0)
  data_line[[FACET]] <- rep(unique(df$AgeClass), each = length(full_lines))
  

  data_line <- data_line %>% left_join(
    df %>% group_by(!!sym(FACET)) %>% 
      summarize(maxval = ifelse(all(is.na(Value)), Inf,max(Value, na.rm = TRUE)) *1.1),
    by = FACET)
  
  p <- p +
    geom_vline(xintercept = full_lines, linetype = "dotted", size = 0.3) +
    geom_text(data = data_line, aes(x = x, y = maxval, label = lab), 
              size = 1.7, inherit.aes = FALSE, hjust = 1.1, vjust = 1, angle = 90)
  
  # Axis labels get modfied by ggplotly
  pply <- p %>% plotly::ggplotly(tooltip = c("text"),
                                 layerData = 3,
                                 #textposition = 'outside',
                                 dynamicTicks = TRUE,
                                 originalData = TRUE
  )
  # not easy to ger the number of traces for the months
  # pply <- pply %>% plotly::style(hoverinfo = "skip", traces = 4)


  
  pply <- pply %>%
    plotly::layout(
      hovermode = 'closest', clickmode = "event",
      #hovermode = 'x', #clickmode = "event",
      showlegend = TRUE,
      legend = legend_pars(TRUE),
      dragmode = FALSE,
      xaxis = list(zerolinewidth = 2)
      # yaxis = list(#autorange = TRUE, 
      #   fixedrange = TRUE)
    )
  p <- pply
  p
}

#' Color table cells
#'
#' @param data data.frame data
#' @param header character, variable name for header
#' @param cgroup character, variable name for cgroup if present, NULL
#' @param rnames character, variable name for rows
#' @param rgbn numeric RGBN value
#' @param rnames character, variable level to skip
#' @param table_cell_css character feature of css cells
#' 
#' @import dplyr
#' @import tidyr
#' 
#' @noRd
color_cells <- function(data, header, cgroup = NULL, rnames, rgbn = 255, skip = "All", table_cell_css = "") {
  
  colvalues <- data %>% mutate(value = replace_na(value,0)) 
  
  .fun_col <- function(vv,vskip,skip){
    # colvect <-  1-vv/max(vv[vskip != skip])/2
    # sd_vect <- mean(vv[vskip != skip])/sd(vv[vskip != skip])
    # logv <- log(vv)
    # colvect
    colvect <- 1- ((vv- min(vv[vskip != skip])) / diff(range(vv[vskip != skip]))/3)
    colvect[vskip == skip] <-1
    
    #(1-(vv / max(diff(range(vv[vskip != skip])))))/2
    colvect
  }
  
  if (!is.null(cgroup)) {
    if (length(cgroup) == 1)
      colvalues <- colvalues %>% group_by(!!sym(cgroup[1])) %>%
        mutate(value = .fun_col(value, !!sym(rnames), skip)) %>%
        ungroup()
    else 
      colvalues <- colvalues %>% group_by(!!sym(cgroup[1]), !!sym(cgroup[2])) %>%
        mutate(value = .fun_col(value, !!sym(rnames), skip)) %>%
        ungroup()      
  } else {
    colvalues <- colvalues %>% group_by(!!sym(header)) %>%
      mutate(value = .fun_col(value, !!sym(rnames), skip)) %>%
      ungroup()
  }
  #%>%
  #mutate(OrigValue = data$value)
  colvalues <- colvalues %>%
    mutate(value = paste(table_cell_css, 
                         paste0("background-color:RGB(255, ",round(value*rgbn,0)," , ",
                                round(value*rgbn,0) ," )"), sep = ";")) %>%
    mutate(value = ifelse(!!sym(rnames) == skip, table_cell_css, value))
  
  if (!is.null(cgroup)) {
    colvalues <- colvalues %>%
      pivot_wider(names_from = all_of(cgroup), values_from = "value", names_sort = TRUE)

    cols_from <- setdiff(colnames(colvalues), c(header,rnames))
    
  } else {
    cols_from = "value"
  }
  
  colvalues <-  colvalues %>%   
    pivot_wider(names_from = all_of(header), values_from = all_of(cols_from), names_sep = ".", names_sort = TRUE)
  
  # hard code removal of Ratio over fully vac
  if (any(grepl("Ratio over fully Vac..Fully vac.", colnames(colvalues))))
    colvalues <- colvalues[, -grep("Ratio over fully Vac..Fully vac.", colnames(colvalues)), drop = FALSE]
  
  colvalues <-  colvalues %>%   
    mutate(!!sym(rnames) := table_cell_css) %>%
    as.matrix()
  colvalues
}

