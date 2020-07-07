library(shiny)
library(shinydashboard)

## Source R scripts
source("R/mapAverageLayerFiles.R", local = TRUE)
source("R/polygonPlotsFromAggLoglikeFiles.R", local = TRUE)
source("R/polygonPlotsFromPopDynFiles.R", local = TRUE)
source("R/helperFunctions.R", local = TRUE)

sbox <- shinydashboard::box

## Find available RData files and pick out scenarios
loglikefns <- dir("data", "loglike.*RData", full.names = TRUE)
loglikescenarios <- gsub("^.*agg_|[.]RData", "", loglikefns)
popdynfns <- dir("data", "popdyn.*RData", full.names = TRUE)
popdynscenarios <- gsub("^.*popdyn_|[.]RData", "", popdynfns)

## Load all loglike and popdyn files
for (f in c(loglikefns, popdynfns)) load(f, envir = .GlobalEnv)

jsCode <- 'shinyjs.hideSidebar = function(){
  if (!$("#sidebarCollapsed").data("collapsed")) {
    $("a.sidebar-toggle").trigger("click")
  }
}'

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

## User interface ----
ui <- dashboardPage(
  dashboardHeader(title = "DISPLACE output viewer"),
  dashboardSidebar(
    # sidebarMenu( id="menu",
    #              menuItem("Maps", tabName = "map", icon = icon("map")),
    #              menuSubItem(icon = NULL, tabName = "map",
    #                          conditionalPanel(condition = "input.menu === 'map'",
    #                                           selectInput("sel.mapquantity", "Select quantity",
    #                                                       choices = selquantity(), multiple = FALSE, selectize = FALSE))),
    #              menuItem("Time series", tabName = "ts", icon = icon("chart-line")),
    #              menuSubItem(icon = NULL, tabName = "ts",
    #                          conditionalPanel(condition = "input.menu === 'ts'",
    #                                           selectInput("sel.sce", "Select scenarios", choices = selsce(), selected = selsce(), multiple = TRUE, selectize = FALSE),
    #                                           selectInput("sel.var", "Select a variable", choices = selvar(), selected = "gradva", multiple = FALSE),
    #                                           selectInput("sel.pop", "Select populations", choices = selpop(), selected = "pop.1", multiple = TRUE, selectize = FALSE),
    #                                           selectInput("sel.sum.szgroups", "Sum over size groups", choices = selsumoverszgrp(), selected = selsumoverszgrp()[1],
    #                                                       multiple = FALSE, selectize = FALSE))),
    #              menuItem("Box plots", tabName = "boxplots", icon = icon("chart-bar")),
    #              menuSubItem(icon = NULL, tabName = "boxplots",
    #                          conditionalPanel(condition = "input.menu === 'boxplots'",
    #                                           selectInput("sel.sce2", "Select scenarios", choices = selsce(), selected = selsce(), multiple = TRUE, selectize = FALSE)
    #                          )))
    sidebarMenu(
      sidebarMenu(id = "menu",
                  convertMenuItem(menuItem("Maps", tabName = "map", icon = icon("map"), startExpanded = TRUE,
                                           selectInput("sel.mapquantity", "Select quantity", choices = selquantity(), multiple = FALSE, selectize = FALSE)), "map"),
                  convertMenuItem(menuItem("Time series", tabName = "ts", icon = icon("chart-line"),
                                           selectInput("sel.sce", "Select scenarios", choices = selsce(), selected = selsce(), multiple = TRUE, selectize = FALSE),
                                           selectInput("sel.var", "Select a variable", choices = selvar(), selected = "gradva", multiple = FALSE),
                                           selectInput("sel.pop", "Select populations", choices = selpop(), selected = "pop.1", multiple = TRUE, selectize = FALSE),
                                           selectInput("sel.sum.szgroups", "Sum over size groups", choices = selsumoverszgrp(), selected = selsumoverszgrp()[1],
                                                       multiple = FALSE, selectize = FALSE)
                  ), "ts"),
      convertMenuItem(menuItem("Boxplots", tabName = "boxplots", icon = icon("chart-bar"),
                               selectInput("sel.sce2", "Select scenarios", choices = selsce(), selected = selsce(), multiple = TRUE, selectize = FALSE)), "boxplots")
    )
  )
),
dashboardBody(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jsCode, functions = c("hideSidebar")),
  tabItems(
    tabItem("map",
            plotOutput("cumulativeMap", height = "750px", width = "500px")
    ),
    tabItem("ts",
            fluidRow(
              sbox(width = 6, plotOutput("linePlot"), title = "Polygon plot", status = "primary", solidHeader = TRUE),
              sbox(width = 6, plotOutput("linePlot2"), title = "Catch development over time", status = "primary", solidHeader = TRUE),
              sbox(width = 6, plotOutput("linePlot3"), title = "", status = "primary", solidHeader = TRUE)
            )),
    tabItem("boxplots",
            plotOutput("boxplot"))
  )
)
)

## Server side logic ----
server <- function(input, output) {
  output$cumulativeMap <- renderPlot({
    warningPlot("ddd")
    if(F) {
      scedir <- "output" ## "data/CelticSea"
      scenarios <- dir(scedir, "^sce[^_]*")
      m <- regexpr("sce[^_]*", scenarios)
      scenarios <- unique(regmatches(scenarios, m))
      outdir <- "output"

      makeCumulativeMap(scedir, outdir = outdir, scenarios = scenarios,
                        a_type = input$sel.mapquantity, in_relative = FALSE)
    }
  })

  output$linePlot <- renderPlot({
    req(input$sel.var, input$sel.sce)
    par(mar = c(4, 5, 0.5, 0.5))
    do_polygon_plot(
      a_variable = input$sel.var,
      nby = 5,
      a_set_of_scenarios=input$sel.sce,
      the_scenario_names=input$sel.sce,
      name_set_of_sces= "setA",
      selected="_selected_set1_",
      export=FALSE,
      a_ylab = switch(input$sel.var,
                      gradva = "Acc. GVA (million €)",
                      rev_from_av_prices = "Income from landings (mio Euro)",
                      rev_explicit_from_av_prices = "Income from landings (mio Euro)",
                      "Accumulated Gross Added Value (millions Euro)"),
      add_legend=TRUE,
      color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4),
                      rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),
      a_width=3500,
      a_height=1000

    )
  })

  output$linePlot2 <- renderPlot({
    par(mar=c(4,4,1.5,0.9))
    #onesim <- lst_loglike_agg_weight_all_scebaseline[[1]]
    onesim <- get(paste0("lst_loglike_agg_weight_all_", input$sel.sce))[[1]]
    str(onesim, 1)
    ym <- ym2date(onesim$year.month)
    nms <- names(onesim)
    selected <- onesim[, nms %in% input$sel.pop, drop = FALSE] / 1000
    maxima <- apply(selected, 2, max)
    limits <- c(0, 10, 100, 1000, Inf)
    labels <- paste("Max catches: ", paste(limits[-5], limits[-1], sep = "-"), "tonnes")
    labels[length(labels)] <- "Max catches: > 1000 tonnes"
    lvls <- droplevels(cut(maxima, limits, include.lowest = TRUE,
                           labels = labels))
    switch(length(levels(lvls)),
           "1" = par(mfrow = c(1,1)),
           "2" = par(mfrow = c(2,1)),
           "3" = par(mfrow = c(2,2)),
           "4" = par(mfrow = c(2,2)))
    for(l in levels(lvls)) {
      matplot(ym, selected[, lvls == l],
              type = "l", ylab = "Catch (tonnes)", xlab = "")
      mtext(l, line = 0.5, cex = 1.3)
    }
  })


  output$linePlot3 <- renderPlot({
    req(input$sel.pop, input$sel.sce, input$sel.sum.szgroups)
    if (length(input$sel.sce) < 2) {
      warningPlot("Select two or more scenarios to compare")
    } else {
      plot_popdyn (sces=input$sel.sce,
                   explicit_pops= input$sel.pop,
                   sum_all=input$sel.sum.szgroups
      ) }
  })

  output$boxplot <- renderPlot({
    warningPlot("Not implemented yet")
  })
  observeEvent(input$tabs, {
    if (input$tabs == "B") {
      shinyjs::js$hideSidebar()
    }
  })

}


shinyApp(ui = ui, server = server)
