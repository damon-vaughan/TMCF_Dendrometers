library(needs)
needs(tidyverse, shiny, lubridate, readxl, shinyWidgets)

source("app_functions_dendro.R")
options(readr.show_col_types = FALSE)

import.log <- read_csv(file.path("Dendro_data_supporting", "Dendro_import_log.csv"))
max.date <- max(import.log$Last.import, na.rm = T)

dendro.download <- read_csv(file.path("Dendro_data_supporting", "Dendro_DL_Dates.csv")) %>%
  mutate(DL.date = ymd_hms(str_c(DL.date, "10:00:00"), tz = "UTC")) %>%
  unite(Tree, Letter, col = "Dendrometer", sep = "")

tree.visits <- read_excel("C:/Users/vaug8/OneDrive - University of Kentucky/TMCF/Continuous_data/Maintenance notes/Tree_visits.xlsx") %>%
  pivot_longer(2:21, names_to = "Tree", values_to = "Timestamp")%>%
  mutate(Timestamp = ymd_hms(str_c(Timestamp, "10:00:00"), tz = "UTC"))

# UI -------------------------------------
ui <- fluidPage(
  titlePanel("Dendro graphs"),

  sidebarLayout(
    sidebarPanel(

      sliderInput("daterange",
                  label = h4("Select date range"),
                  min = ymd_hms("2022-09-01 00:00:00"),
                  max = max.date,
                  value = c(ymd_hms("2022-09-01 00:00:00"),
                            max.date)),

      fluidRow(
        column(3,
               radioButtons("fixed.y",
                            label = h4("Fix y-axis?"),
                            choices = c("Yes", "No"),
                            selected = "No")),
        column(9,
               sliderInput("yrange",
                           label = h4("Select y-axis range"),
                           min = -200,
                           max = 4000,
                           value = c(0, 2000)))),
      fluidRow(
        column(3,
               radioButtons("Level",
                            label = h4("Select level"),
                            choices = c("L1", "L2", "L2a"),
                            selected = "L2a")),
        column(3, offset = 1,
               radioButtons("show.download",
                            label = h4("Show downloads?"),
                            choices = c("yes", "no"),
                            selected = "no")),
        column(3, offset = 1,
               radioButtons("show.visit",
                            label = h4("Show visits?"),
                            choices = c("yes", "no"),
                            selected = "no"))),
      fluidRow(
        column(2,
               radioButtons("tree",
                            label = h4("Select tree"),
                            choices = c("ET1", "ET2", "ET3", "ET4",
                                        "ET5", "ET6", "ET7", "ET8",
                                        "FB1", "FB2", "FB3", "FB4",
                                        "FB5", "FB6", "FB7", "FB8",
                                        "TV1", "TV2", "TV3", "TV4"),
                            selected = "ET1")),
        column(2, offset = 1,
               radioButtons("letter",
                            label = h4("Select Letter"),
                            choices = c("a", "b"),
                            selected = "a")
        )
      ),
      downloadButton(outputId = "downloadData", label = "Download")
    ),

    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot",
                 "Graph shows data until:",
                 verbatimTextOutput("maxdate.output"),
                 plotOutput("plot1",
                            click = "plot_click",
                            brush = "plot_brush"),
                 verbatimTextOutput("plot_clickinfo"),
                 plotOutput("plot_brushedpoints")),
        tabPanel("Summary", tableOutput("summary")),
        tabPanel("Data", tableOutput("data1"), tableOutput("data2"))))
    )
)


# Server ---------------------------------------------------------

server <- function(input, output, session) {
  dataInput <- reactive({
    read_csv(file.path(str_c("Dendro_data_", input$Level),
                       str_c(input$tree, input$letter, "_Dendro_",
                             input$Level, ".csv")), guess_max = 10000) %>%
      filter(Timestamp >= input$daterange[1] & Timestamp <= input$daterange[2]) %>%
      mutate(Tree = str_sub(Dendrometer, start = 1, end = 3))
  })

  dataInput2 <- reactive({
    dataInput() %>%
      mutate(Timestamp = as.character(Timestamp)) %>%
      select(Dendrometer, Timestamp, Radius, Maximum = max, TWD = twd, Growth = gro_yr)
  })

  label.DL <- reactive({
    dendro.download %>%
      filter(Dendrometer == str_c(input$tree, input$letter)) %>%
      filter(DL.date >= input$daterange[1] & DL.date <= input$daterange[2]) %>%
      left_join(dataInput(), by = c("Dendrometer", "DL.date" = "Timestamp"))
  })

  label.visit <- reactive({
    tree.visits %>%
      filter(Tree == input$tree) %>%
      filter(Timestamp >= input$daterange[1] & Visit <= input$daterange[2]) %>%
      left_join(dataInput(), by = c("Tree", "Timestamp"))
  })

  output$plot1 <- renderPlot({
    p = ggplot() +
      geom_line(data = dataInput(), aes(x = Timestamp, y = Radius)) +
      labs(y = "Micrometers") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 20),
            axis.title.y = element_text(size = 24),
            axis.text.y = element_text(size = 20),
            plot.title = element_text(size = 24)) +
      ggtitle(str_c(input$tree, input$letter))
    if(input$show.download == "yes"){
      p = p +
        geom_label(data = label.DL(), aes(x = DL.date, y = Radius, label = "DL"))}
    if(input$show.visit == "yes"){
      p = p +
        geom_point(data = label.visit(),
                   aes(x = Timestamp, y = Radius), color = "blue", size = 5)}
    if(input$fixed.y == "Yes"){
      p = p +
        ylim(input$yrange[1], input$yrange[2])
    }
    p
    })

  output$maxdate.output <- renderText({
    as.character(max(dataInput()$Timestamp, na.rm = T))
  })

  output$plot_clickinfo <- renderPrint({
    val <- nearPoints(dataInput(), input$plot_click, maxpoints = 1)
    unique(val$Timestamp)
  })

  output$plot_brushedpoints <- renderPlot({
    dat <- brushedPoints(dataInput(), input$plot_brush)
    if (nrow(dat) == 0)
      return()
    ggplot(dat) +
      geom_line(aes(x = Timestamp, y = Radius)) +
      labs(y = "micrometers") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 18),
            axis.title.y = element_text(size = 20),
            axis.text.y = element_text(size = 18),
            plot.title = element_text(size = 24)) +
      ggtitle(str_c(input$tree, "_", input$dendro))
  })

  output$summary <- renderTable({
      summarise_dendro(dataInput2())
  })

  output$data1 <- renderTable({
    head(dataInput2())
  })

  output$data2 <- renderTable({
    tail(dataInput2())
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      str_c(str_c(input$tree, input$letter,
                  str_sub(as.character(input$daterange[1]), start = 1, end = 10),
                  str_sub(as.character(input$daterange[2]), start = 1, end = 10),
                  sep = "_"),
            ".csv")
    },
    content = function(file) {
      write_csv(dataInput(), file)
    }
  )
}

# Run ----------------------------------------------------------
shinyApp(ui, server)

# Test server -------------------------------------------------------------
#
# testServer(server, {
#   session$setInputs(Level = "L2a")
#   session$setInputs(tree = "FB1")
#   session$setInputs(letter = "a")
#   session$setInputs(daterange = c(min = ymd("2022-09-01"),
#                                   max = ymd(as.character(Sys.Date()))))
#   test <<- print(dataInput2())
# })



# Extra stuff... ----------------------------------------------------------



## UI 2: L2a twd and growth --------------------------------------------
#
#ui <- fluidPage(
#  titlePanel("Dendro graphs"),
#
#  sidebarLayout(
#    sidebarPanel(
#      sliderInput("daterange",
#                  label = h4("Select date range"),
#                  min = ymd_hms("2022-09-01 00:00:00"),
#                  max = max.date,
#                  value = c(ymd_hms("2022-09-01 00:00:00"),
#                            max.date)),
#      fluidRow(
#        column(2,
#               radioButtons("tree",
#                            label = h4("Select tree"),
#                            choices = c("ET1", "ET2", "ET3", "ET4",
#                                        "ET5", "ET6", "ET7", "ET8",
#                                        "FB1", "FB2", "FB3", "FB4",
#                                        "FB5", "FB6", "FB7", "FB8",
#                                        "TV1", "TV2", "TV3", "TV4"),
#                            selected = "ET1")),
#        column(2, offset = 1,
#               fluidRow(
#                 radioButtons("letter",
#                              label = h4("Select Letter"),
#                              choices = c("a", "b"),
#                              selected = "a")),
#               fluidRow(
#                 radioButtons("show.jumps",
#                              label = h4("Show jumps?"),
#                              choices = c("yes", "no"),
#                              selected = "yes"))
#               )
#      )
#    ),
#
#    mainPanel(plotOutput("plot1",
#                         hover = "plot_hover",
#                         brush = "plot_brush"),
#              plotOutput("plot2"),
#              verbatimTextOutput("plot_hoverinfo"),
#              plotOutput("plot_brushedpoints"))
#  )
#)
#
#
## Server 2 ----------------------------------------------------------------
#
#server <- function(input, output, session) {
#  dataInput <- reactive({
#    read_csv(file.path("Dendro_data_L2a",
#                       str_c(input$tree, input$letter,
#                             "_Dendro_L2a.csv")), guess_max = 10000) %>%
#      filter(Timestamp >= input$daterange[1] & Timestamp <= input$daterange[2])
#  })
#
#  dataLabel <- reactive({
#    dataInput() %>%
#      filter(flags == "jump1")
#  })
#
#  output$plot1 <- renderPlot({
#    p <- ggplot() +
#      geom_line(data = dataInput(), aes(x = Timestamp, y = gro_yr)) +
#      labs(y = "Micrometers") +
#      theme_bw() +
#      theme(axis.title.x = element_blank(),
#            axis.text.x = element_text(size = 20),
#            axis.title.y = element_text(size = 24),
#            axis.text.y = element_text(size = 20),
#            plot.title = element_text(size = 24)) +
#      ggtitle(str_c(input$tree, input$letter))
#
#    if(input$show.jumps == "yes"){
#      p <- p +
#        geom_point(data = dataLabel(), aes(x = Timestamp, y = gro_yr),
#                 color = "blue", size = 2)
#    }
#    p
#  })
#
#  output$plot2 <- renderPlot({
#    p <- ggplot() +
#      geom_line(data = dataInput(), aes(x = Timestamp, y = twd)) +
#      labs(y = "Micrometers") +
#      theme_bw() +
#      theme(axis.title.x = element_blank(),
#            axis.text.x = element_text(size = 20),
#            axis.title.y = element_text(size = 24),
#            axis.text.y = element_text(size = 20),
#            plot.title = element_text(size = 24))
#    if(input$show.jumps == "yes"){
#      p <- p +
#        geom_point(data = dataLabel(), aes(x = Timestamp, y = twd),
#                   color = "blue", size = 2)
#    }
#    p
#  })
#
#  output$plot_hoverinfo <- renderPrint({
#    val <- nearPoints(dataInput(), input$plot_hover, maxpoints = 1)
#    unique(val$Timestamp)
#  })
#
#  output$plot_brushedpoints <- renderPlot({
#    dat <- brushedPoints(dataInput(), input$plot_brush)
#    if (nrow(dat) == 0)
#      return()
#    ggplot(dat) +
#      geom_line(aes(x = Timestamp, y = Radius)) +
#      labs(y = "micrometers") +
#      theme_bw() +
#      theme(axis.title.x = element_blank(),
#            axis.text.x = element_text(size = 18),
#            axis.title.y = element_text(size = 20),
#            axis.text.y = element_text(size = 18),
#            plot.title = element_text(size = 24)) +
#      ggtitle(str_c(input$tree, "_", input$dendro))
#  })
#}
#
## Run 2 -------------------------------------------------------------------
#
#shinyApp(ui, server)
#

## UI 3: Navbar ---------------------------------------------------------------

# ui <- fluidPage(
#   titlePanel("Dendro graphs"),
#
#   navbarPage(
#     sidebarPanel(
#
#       sliderInput("daterange",
#                   label = h4("Select date range"),
#                   min = ymd_hms("2022-09-01 00:00:00"),
#                   max = max.date,
#                   value = c(ymd_hms("2022-09-01 00:00:00"),
#                             max.date)),
#
#       fluidRow(
#         column(3,
#                radioButtons("fixed.y",
#                             label = h4("Fix y-axis?"),
#                             choices = c("Yes", "No"),
#                             selected = "No")),
#         column(9,
#                sliderInput("yrange",
#                            label = h4("Select y-axis range"),
#                            min = -200,
#                            max = 4000,
#                            value = c(0, 2000)))),
#       fluidRow(
#         column(3,
#                radioButtons("Level",
#                             label = h4("Select level"),
#                             choices = c("L1", "L2", "L2a"),
#                             selected = "L2a")),
#         column(3, offset = 1,
#                radioButtons("show.download",
#                             label = h4("Show downloads?"),
#                             choices = c("yes", "no"),
#                             selected = "no")),
#         column(3, offset = 1,
#                radioButtons("show.visit",
#                             label = h4("Show visits?"),
#                             choices = c("yes", "no"),
#                             selected = "no"))),
#       fluidRow(
#         column(2,
#                radioButtons("tree",
#                             label = h4("Select tree"),
#                             choices = c("ET1", "ET2", "ET3", "ET4",
#                                         "ET5", "ET6", "ET7", "ET8",
#                                         "FB1", "FB2", "FB3", "FB4",
#                                         "FB5", "FB6", "FB7", "FB8",
#                                         "TV1", "TV2", "TV3", "TV4"),
#                             selected = "ET1")),
#         column(2, offset = 1,
#                radioButtons("letter",
#                             label = h4("Select Letter"),
#                             choices = c("a", "b"),
#                             selected = "a"))
#       ),
#       downloadButton("downloadData", "Download")
#     ),
#
#     mainPanel(
#       tabSetPanel(
#         type = "tabs",
#         tabPanel("Plot",
#                  "Graph shows data until:",
#                  verbatimTextOutput("maxdate.output"),
#                  plotOutput("plot1",
#                             click = "plot_click",
#                             brush = "plot_brush"),
#                  verbatimTextOutput("plot_clickinfo"),
#                  plotOutput("plot_brushedpoints")),
#         tabPanel("Summary")))
#   )
# )

