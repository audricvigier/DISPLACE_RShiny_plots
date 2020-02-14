## This is a test shiny application for visualising DISPLACE output

library(shiny)

sces <- c("scebaseline", "scesizespectra")

# loglike outcomes
for (sce in sces)
  load(paste0("data/lst_loglike_weight_agg_",sce,".RData"))

# poplike outcomes
for (sce in sces)
  load(paste0("data/lst_popdyn_",sce,".RData"))


source("R/polygonPlotsFromAggLoglikeFiles.R")
source("R/polygonPlotsFromPopDynFiles.R")
##source("R/DISPLACE_plotting.R")
##the_baseline <- ""
##combined_name <- paste(the_baseline,"_vs_", sce, sep='')

ym2date <- function(x) {
    sapply(strsplit(x, "[.]"),
           function(y) {
               as.numeric(y[1]) + as.numeric(y[2]) / 12 - 1/12
           })
}

selpop <- function() {
    nms <- names(lst_loglike_agg_weight_all_scebaseline[[1]])
    pops <- nms[startsWith(nms, "pop.")]
    pops
}

selsce <- function() {
    sces
}

selvar <- function() {
    nms <- names(lst_loglike_agg_weight_all_scebaseline[[1]])
    nms[-1]
}



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DISPLACE output viewer"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("sel.sce", "Select scenarios", choices = selsce(), selected = selsce(), multiple = TRUE, selectize = FALSE),
            selectInput("sel.var", "Select a variable", choices = selvar(), selected = "gradva", multiple = FALSE),
            selectInput("sel.pop", "Select populations", choices = selpop(), selected = "pop.1", multiple = TRUE, selectize = FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("linePlot"),
            plotOutput("linePlot2"),
            plotOutput("linePlot3")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$linePlot <- renderPlot({
        do_polygon_plot(
            a_variable = input$sel.var,
            nby = 5,
            a_set_of_scenarios= c(
                input$sel.sce),
            the_scenario_names= c(
                input$sel.sce) ,
            name_set_of_sces= "setA",
            selected="_selected_set1_",
            export=FALSE,
            a_ylab = switch(input$sel.var,
                            gradva = "Acc. GVA (mio Euro)",
                            rev_from_av_prices = "Income from landings (mio Euro)",
                            rev_explicit_from_av_prices = "Income from landings (mio Euro)",
                           "Accumulated Gross Added Value (millions Euro)"),
                  add_legend=TRUE,
                  color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4), rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),
            a_width=3500,
            a_height=1000

        )
        title(main = "Polygon plot")
    })

    output$linePlot2 <- renderPlot({
        ym <- ym2date(lst_loglike_agg_weight_all_scebaseline[[1]]$year.month)
        matplot(ym, lst_loglike_agg_weight_all_scebaseline[[1]][, names(lst_loglike_agg_weight_all_scebaseline[[1]]) %in% input$sel.pop],
                type = "l", ylab = "Kg", xlab = "Year")
        title(main = "Catch development over time")
    })
    
    
     output$linePlot3 <- renderPlot({
        plot_popdyn (sces=input$sel.sce,
                      explicit_pops= input$sel.pop,
                      sum_all=FALSE
                      ) 
     })
     
    # output$boxplot <- renderPlot({
    #
    # })

}

# Run the application
shinyApp(ui = ui, server = server)
