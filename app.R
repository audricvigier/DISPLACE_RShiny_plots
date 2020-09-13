library(shiny)
library(shinydashboard)
library(plotly)
shinyOptions(shiny.autoreload = TRUE)
shinyOptions(shiny.launch.browser = TRUE)

## Source R scripts
source("R-scripts/mapAverageLayerFiles.R", local = TRUE)
source("R-scripts/polygonPlotsFromAggLoglikeFiles.R", local = TRUE)
source("R-scripts/polygonPlotsFromPopDynFiles.R", local = TRUE)
source("R-scripts/helperFunctions.R", local = TRUE)
source("R-scripts/barplotLanDisPerScenario.R", local = TRUE)

sbox <- shinydashboard::box

## Find available RData files and pick out scenarios
loglikefns <- dir("data", "loglike.*RData", full.names = TRUE)
loglikescenarios <- gsub("^.*agg_|[.]RData", "", loglikefns)
popdynfns <- dir("data", "popdyn.*RData", full.names = TRUE)
popdynscenarios <- gsub("^.*popdyn_|[.]RData", "", popdynfns)

## Load all loglike and popdyn files
for (f in c(loglikefns, popdynfns)) load(f, envir = .GlobalEnv)

## Read population names
## popnames <- read.table("data/CelticSea44/pop_names_CelticSea.txt", header = TRUE); save("popnames", file = "data/popnames.Rdata")
load(file = "data/popnames.Rdata")

convertMenuItem <- function(tabName, mi) {
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

## User interface ----
ui <- dashboardPage(
  dashboardHeader(title = "DISPLACE output viewer"),
  dashboardSidebar(
    sidebarMenu(
      sidebarMenu(id = "menu",
                  convertMenuItem("intro",
                                  menuItem("Model info", tabName = "intro", icon = icon("info"), startExpanded = TRUE)),
                  convertMenuItem("map",
                                  menuItem("Maps", tabName = "map", icon = icon("map"),
                                           selectInput("sel.mapquantity", "Select quantity", choices = selquantity(), multiple = FALSE, selectize = FALSE))),
                  convertMenuItem("ts",
                                  menuItem("Time series", tabName = "ts", icon = icon("chart-line"),
                                           selectInput("sel.sce", "Select scenarios", choices = selsce(), selected = selsce(), multiple = TRUE, selectize = FALSE),
                                           selectInput("sel.var", "Select a variable", choices = selvar(), selected = "gradva", multiple = FALSE),
                                           checkboxInput("quantCumSum", label = "Cumulative sum", value = TRUE))),
                  convertMenuItem("tab_landis_perpop",
                                  menuItem("Populations", tabName = "tab_landis_perpop", icon = icon("chart-bar"),
                                           selectInput("sel.sce2", "Select scenarios", choices = selsce(), selected = selsce(), multiple = TRUE, selectize = FALSE),
                                           selectInput("sel.pop", "Select populations", choices = selpop(), selected = c("pop.2", "pop.5", "pop.7", "pop.12"), multiple = TRUE, selectize = FALSE),
                                           selectInput("sel.sum.szgroups", "Sum over size groups", choices = c(TRUE, FALSE), selected = TRUE,
                                                       multiple = FALSE, selectize = FALSE)))
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("intro",
              h2("DISPLACE model for the Irish demersal Celtic Sea fisheries"),
              sbox(width = 12, title = "Background", status = "primary", solidHeader = FALSE, collapsible = FALSE,
                   div("Little info about the aim of the DISPLACE model.")),
              sbox(width = 6, title = "Conditioning fisheries", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                   div("Fishing vessels of the Irish demersal fishing fleet are considered (DAFM, 2017; EC, 2017). "), collapsed = TRUE),
              sbox(width = 6, title = "Study area", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                   div(img(src = "studyAreaMap.png"))),
              sbox(width = 6, title = "Species", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                   div(tableOutput("speciesTable"))),
              sbox(width = 6, title = "Gear categories", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                   div(tableOutput("gearTable")))),
      tabItem("map",
              plotOutput("cumulativeMap", height = "500px")
      ),
      tabItem("ts",
              fluidRow(
                sbox(width = 6, plotOutput("linePlot"), title = "", status = "primary", solidHeader = FALSE)
              )),
      tabItem("tab_landis_perpop",
              fluidRow(
              sbox(width = 6, plotOutput("catchTimeSeriesPlot"), title = "Catch development over time", status = "primary", solidHeader = TRUE),
              sbox(plotOutput("barplot_landis_perpop"), title = "Landings per population", solidHeader = TRUE, status = "primary"),
              sbox(width = 6, plotOutput("populationSizePlot", height = "auto"), title = "Population size", status = "primary", solidHeader = TRUE))),
      tabItem("tab_plotlymap",
              plotlyOutput("cumulativeMaps"))
    )
  )
)

## Server side logic ----
server <- function(input, output) {
  output$speciesTable <- renderTable(read.csv("data/species.csv"))
  output$gearTable <- renderTable({
    tbl <- read.csv("data/gears.csv")
    names(tbl) <- c("Gear", "Code")
    tbl
  })

  output$cumulativeMap <- renderPlot({
    # scedir <- "data/CelticSea44/"
    # scenarios <- dir(scedir, "^sce[^_]*")
    # m <- regexpr("sce[^_]*", scenarios)
    # scenarios <- unique(regmatches(scenarios, m))
    scenarios <- unique(sapply(strsplit(dir("output", ".*Rds"), split = "_"), first))
    outdir <- "output"

    makeCumulativeMap(scedir = scedir, outdir = outdir, scenarios = scenarios,
                      a_type = input$sel.mapquantity, in_relative = FALSE)

  })

    output$linePlot <- renderPlot({
    req(input$sel.var, input$sel.sce)
    par(mar = c(4, 5, 0.5, 0.5))
    do_polygon_plot(
      a_variable = input$sel.var,
      nby = 5,
      documsum = input$quantCumSum,
      a_set_of_scenarios = input$sel.sce,
      the_scenario_names = sub("sce", "", input$sel.sce),
      name_set_of_sces = "setA",
      selected = "_selected_set1_",
      export = FALSE,
      a_ylab = switch(input$sel.var,
                      gradva = "Accumulated Gross Value Added (million €)",
                      rev_from_av_prices = "Income from landings (million €)",
                      effort = "Effort",
                      nbtrip = "Number of trips",
                      totland = "Total landings",
                      ""),
      add_legend = TRUE,
      color_legend = c(rgb(94/255,79/255,162/255,0.5), rgb(158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4),
                       rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),
      a_width = 3500,
      a_height = 1000

    )
  })

  output$catchTimeSeriesPlot <- renderPlot({
    par(mar = c(4,4,1.5,0.9))
    #onesim <- lst_loglike_agg_weight_all_scebaseline[[1]]
    add <- FALSE
    for (s in input$sel.sce2) {
      onesim <- get(paste0("lst_loglike_agg_weight_all_", s))[[1]]
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
      for (l in levels(lvls)) {
        matplot(ym, selected[, lvls == l],
                type = "l", ylab = "Catch (tonnes)", xlab = "", add = add)
        add <- FALSE
        mtext(l, line = 0.5, cex = 1.3)
      }
    }
  })


  output$populationSizePlot <- renderPlot({
    req(input$sel.pop, input$sel.sce2, input$sel.sum.szgroups)
    plot_popdyn(sces = input$sel.sce2,
                explicit_pops = input$sel.pop,
                sum_all = input$sel.sum.szgroups)
  }, height = function() {((length(input$sel.pop) + 1) %/% 2 ) * 300 })

  output$barplot_landis_perpop <- renderPlot({
    #warningPlot("Not implemented yet")
    barplotTotLandingsPerSce(selected_scenarios = input$sel.sce2, scenarios_names = sub("sce", "", input$sel.sce2),
                             selected_pops = sub("pop.", "", input$sel.pop))
  })

  output$cumulativeMaps <- renderPlotly({

  })
}


shinyApp(ui = ui, server = server, options = list("shiny.autoload.r" = FALSE))
