# x <- test
summarise_dendro_vars <- function(x){
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
    pivot_wider(names_from = sumstat, values_from = value)

  return(x3)
}

summarise_growth_stats <- function(x){
  x %>%
    summarise(Total.growth = max(Growth, na.rm = T) - min(Growth, na.rm = T))
}

summarise_TWD <- function(x){
  x %>%
    mutate(TWD = ifelse(TWD == 0, "No", "Yes")) %>%
    group_by(TWD) %>%
    summarise(time.spent = n()) %>%
    mutate(total = sum(time.spent)) %>%
    mutate(percentage = 100 * round(time.spent/total, 4)) %>%
    select(TWD, percentage)
}

