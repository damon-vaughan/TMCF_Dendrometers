library(needs)
needs(tidyverse, shiny, here, lubridate, readxl)

newfilenames <- list.files(here("Dendro_data_LVL1"), full.names = T)
newdat <- lapply(newfilenames, read_csv, show_col_types = F) %>%
  bind_rows() %>%
  separate(DendroID, into = c("Tree", "Dendro"), 3)
max.date <- max(newdat$Time, na.rm = T)

Dendro_DL <- read_csv(here("Dendro_data_supporting", "Dendro_DL_Dates.csv"),
                      show_col_types = F) %>%
  mutate(DL_date = ymd(DL_date, tz = "UTC")) %>%
  left_join(newdat, by = c("Tree", "Dendro", "DL_date" = "Time")) %>%
  select(Tree, Dendro, DL_date, LabelLoc = Radius) %>%
  mutate(LabelLoc = LabelLoc - 50)

# Tree_visits <- read_excel("C:/Users/vaug8/OneDrive - University of Kentucky/TMCF/Site info/Maintenance notes/Tree_visits.xlsx") %>%
#   mutate(Visit = ymd(Visit, tz = "UTC")) %>%
#   left_join(newdat, by = c("Tree", "Visit" = "Time")) %>%
#   select(Tree, Dendro, Visit, LabelLoc = Radius) %>%
#   mutate(LabelLoc = LabelLoc)

# User interface ----
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
                  max = ymd_hms(as.character(max.date)),
                  value = c(ymd_hms("2022-09-01 00:00:00"), ymd_hms(as.character(max.date)))),

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
               radioButtons("dendro",
                            label = h4("Select Dendrometer"),
                            choices = c("a", "b"),
                            selected = "a"),
               radioButtons("showDL",
                            label = h4("Show downloads?"),
                            choices = c("yes", "no"),
                            selected = "yes"))
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

# Server logic ----

server <- function(input, output, session) {
  dataInput <- reactive({
    newdat %>%
      filter(Tree == input$tree) %>%
      filter(Dendro == input$dendro) %>%
      filter(Time >= input$daterange[1] & Time <= input$daterange[2])
  })

  output$maxdate.output <- renderPrint({
    max(dataInput()$Time)
  })

  labelInput <- reactive({
    Dendro_DL %>%
      filter(Tree == input$tree) %>%
      filter(Dendro == input$dendro) %>%
      filter(DL_date >= input$daterange[1] & DL_date <= input$daterange[2])
  })

  # labelInput2 <- reactive({
  #   Tree_visits %>%
  #     filter(Tree == input$tree) %>%
  #     filter(Visit >= input$daterange[1] & Visit <= input$daterange[2])
  # })

  output$plot1 <- renderPlot({
    p = ggplot() +
      geom_line(data = dataInput(), aes(x = Time, y = Radius)) +
      # geom_point(data = labelInput2(), aes(x = Visit, y = LabelLoc), color = "blue",
      #            size = 2) +
      labs(y = "Micrometers") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 20),
            axis.title.y = element_text(size = 24),
            axis.text.y = element_text(size = 20),
            plot.title = element_text(size = 24)) +
      ggtitle(str_c(input$tree, "_", input$dendro))
    if(input$showDL == "yes"){
      p = p +
        geom_label(data = labelInput(), aes(x = DL_date, y = LabelLoc, label = "DL"))}
    p
    })

  output$plot_hoverinfo <- renderPrint({
    val <- nearPoints(dataInput(), input$plot_hover, maxpoints = 1)
    unique(val$Time)
  })

  output$plot_brushedpoints <- renderPlot({
    dat <- brushedPoints(dataInput(), input$plot_brush)
    if (nrow(dat) == 0)
      return()
    ggplot(dat) +
      geom_line(aes(x = Time, y = Radius)) +
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

# Run app ----
shinyApp(ui, server)


# Test server -------------------------------------------------------------

testServer(server, {
  session$setInputs(tree = "FB1")
  session$setInputs(dendro = "a")
  session$setInputs(daterange = c(min = ymd("2022-09-01"), max = ymd(as.character(Sys.Date()))))
  print(dataInput())
})
