
summarise_dendro <- function(x){
  create_summary_table <- list(
    min = ~min(.x, na.rm = TRUE),
    mean = ~mean(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE)
  )

  x2 <- x %>%
    summarise(across(where(is.numeric), create_summary_table))

  x3 <- x2 %>%
    pivot_longer(1:ncol(x2), names_to = "var", values_to = "value")  %>%
    separate(var, into = c("measure", "sumstat"), sep = "_") %>%
    pivot_wider(names_from = sumstat, values_from = value) %>%
    mutate(Change = max - min)

  return(x3)
}

# set_filename <- function() {
#   str_c(str_c(input$tree, input$dendro,
#               input$daterange[1], input$daterange[2], sep = "_"),
#         ".csv")
# }
#
# choose_content <- function(file) {
#   write_csv(dataInput(), file)
# }
