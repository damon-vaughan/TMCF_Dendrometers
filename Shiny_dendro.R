library(needs)
#needs(tidyverse, shiny, lubridate, readxl, ggpubr)
needs(tidyverse, shiny, lubridate, readxl)

import.log <- read_csv(file.path("Dendro_data_supporting",
                                 "Dendro_import_log.csv"),
                       show_col_types = F)
max.date <- max(import.log$Last.import, na.rm = T)


# Dendro_DL <- read_csv(here("Dendro_data_supporting", "Dendro_DL_Dates.csv"),
#                       show_col_types = F) %>%
#   mutate(DL_date = ymd(DL_date, tz = "UTC")) %>%
#   left_join(newdat, by = c("Tree", "Letter", "DL_date" = "Timestamp")) %>%
#   select(Tree, Letter, DL_date, LabelLoc = Radius) %>%
#   mutate(LabelLoc = LabelLoc - 50)

# Tree_visits <- read_excel("C:/Users/vaug8/OneDrive - University of Kentucky/TMCF/Site info/Maintenance notes/Tree_visits.xlsx") %>%
#   mutate(Visit = ymd(Visit, tz = "UTC")) %>%
#   left_join(newdat, by = c("Tree", "Visit" = "Timestamp")) %>%
#   select(Tree, Dendro, Visit, LabelLoc = Radius) %>%
#   mutate(LabelLoc = LabelLoc)

# UI 1 ----
ui <- fluidPage(
  titlePanel("Dendro graphs"),

  sidebarLayout(
    sidebarPanel(
      helpText("Move slider to select date range"),
      helpText("Hover pointer over plot to display the exact timestamp"),
      helpText("Zoom plots by clicking and dragging a square over the desired region"),
      helpText("Download data to a folder on your computer"),

      sliderInput("daterange",
                  label = h4("Select date range"),
                  min = ymd_hms("2022-09-01 00:00:00"),
                  max = max.date,
                  value = c(ymd_hms("2022-09-01 00:00:00"),
                            max.date)),

      fluidRow(
        column(3,
               radioButtons("Level",
                            label = h4("Select level"),
                            choices = c("L1", "L2", "L2a"),
                            selected = "L1")),
        column(3, offset = 1,
               radioButtons("showDL",
                            label = h4("Show downloads?"),
                            choices = c("yes", "no"),
                            selected = "yes")),
        column(3, offset = 1,
               radioButtons("show.jumps",
                            label = h4("Show jumps?"),
                            choices = c("yes", "no"),
                            selected = "yes"))),
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
                            selected = "a"))
      ),
      downloadButton("downloadData", "Download")
    ),

    mainPanel("Graph shows data until:",
              verbatimTextOutput("maxdate.output"),
              plotOutput("plot1",
                         hover = "plot_hover",
                         brush = "plot_brush"),
              verbatimTextOutput("plot_hoverinfo"),
              plotOutput("plot_brushedpoints"))
  )
)

# Server 1 ----

server <- function(input, output, session) {
  dataInput <- reactive({
    read_csv(file.path(str_c("Dendro_data_", input$Level),
                       str_c(input$tree, input$letter, "_Dendro_",
                             input$Level, ".csv")), guess_max = 10000) %>%
      filter(Timestamp >= input$daterange[1] & Timestamp <= input$daterange[2])
  })

  output$maxdate.output <- renderPrint({
    as.character(max(dataInput()$Timestamp, na.rm = T))
  })

  # labelInput <- reactive({
  #   Dendro_DL %>%
  #     filter(Tree == input$tree) %>%
  #     filter(Letter == input$letter) %>%
  #     filter(DL_date >= input$daterange[1] & DL_date <= input$daterange[2])
  # })

  # labelInput2 <- reactive({
  #   Tree_visits %>%
  #     filter(Tree == input$tree) %>%
  #     filter(Visit >= input$daterange[1] & Visit <= input$daterange[2])
  # })

  output$plot1 <- renderPlot({
    p = ggplot() +
      geom_line(data = dataInput(), aes(x = Timestamp, y = Radius)) +
      # geom_point(data = labelInput2(), aes(x = Visit, y = LabelLoc), color = "blue",
      #            size = 2) +
      labs(y = "Micrometers") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 20),
            axis.title.y = element_text(size = 24),
            axis.text.y = element_text(size = 20),
            plot.title = element_text(size = 24)) +
      ggtitle(str_c(input$tree, input$letter))
    # if(input$showDL == "yes"){
    #   p = p +
    #     geom_label(data = labelInput(), aes(x = DL_date, y = LabelLoc, label = "DL"))}
    if(input$Level == "L2" & input$show.jumps == "yes"){
      p = p +
        geom_point(data = dataInput(),
                   aes(x = Timestamp, y = jumpStart), color = "blue", size = 5)
    }
    p
    })

  output$plot_hoverinfo <- renderPrint({
    val <- nearPoints(dataInput(), input$plot_hover, maxpoints = 1)
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

  output$downloadData <- downloadHandler(
    filename = function() {
      str_c(str_c(input$tree, input$dendro,
                  input$daterange[1], input$daterange[2], sep = "_"),
            ".csv")
    },
    content = function(file) {
      write_csv(dataInput(), file)
    }
  )
}

# Run 1 ----
shinyApp(ui, server)


# UI 2: L2a twd and growth --------------------------------------------
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
# Server 2 ----------------------------------------------------------------
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
# Run 2 -------------------------------------------------------------------
#
#shinyApp(ui, server)
#
# Test server -------------------------------------------------------------
#
#test <- testServer(server, {
#  session$setInputs(Level = "L2a")
#  session$setInputs(tree = "FB1")
#  session$setInputs(letter = "a")
#  session$setInputs(daterange = c(min = ymd("2022-09-01"),
#                                  max = ymd(as.character(Sys.Date()))))
#  print(dataLabel())
#})
