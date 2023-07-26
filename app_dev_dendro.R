library(needs)
needs(tidyverse, shiny, here, lubridate)

filenames <- list.files(here("Dendro_data_LVL2"), full.names = T)
d <- lapply(filenames, read_csv, show_col_types = F) %>% 
  bind_rows()

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
                  min = ymd("2022-09-01"), 
                  max = ymd(as.character(Sys.Date())), 
                  value = c(ymd("2022-09-01"), ymd(as.character(Sys.Date())))),
      
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
                            selected = "a"))
      ),
      downloadButton("downloadData", "Download")
    ),
    
    mainPanel(plotOutput("plot1",
                         hover = "plot_hover",
                         brush = "plot_brush"),
              verbatimTextOutput("plot_hoverinfo"),
              plotOutput("plot_brushedpoints"))
  )
)

# Server logic ----

server <- function(input, output, session) {
  
  dataInput <- reactive({
    d %>% 
      filter(Tree == input$tree) %>%
      filter(Dendro == input$dendro) %>% 
      filter(Timestamp >= input$daterange[1] & Timestamp <= input$daterange[2])})
  
  output$plot1 <- renderPlot({
    ggplot(dataInput()) +
      geom_line(aes(x = Timestamp, y = Radius)) +
      labs(y = "Micrometers") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 20),
            axis.title.y = element_text(size = 24),
            axis.text.y = element_text(size = 20),
            plot.title = element_text(size = 24)) +
      ggtitle(str_c(input$tree, "_", input$dendro))})
  
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

# Run app ----
shinyApp(ui, server)


# Test server -------------------------------------------------------------

testServer(server, {
  session$setInputs(tree = "FB1")
  session$setInputs(dendro = "a")
  session$setInputs(daterange = c(min = ymd("2022-09-01"), max = ymd(as.character(Sys.Date()))))
  print(dataInput())
})