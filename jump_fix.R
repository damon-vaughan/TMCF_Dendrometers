library(tidyverse)

# Read in ET2a. You can change the filename to read different dendros
d <- read_csv("Dendro_data_LVL1/ET2a_Dendro_LVL1.csv")

# This is a function that takes a single input (jump threshold), and adjusts the dataframe that you just loaded (d)
# Un-comment the line below, and you will be able to run the function line-by-line to understand it and diagnose problems
# jump.threshold <- 5
fix_jumps <- function(d, jump.threshold){

  # change the name of the data (not really necessary)
  d2 = d

  # add a lag column so you can compare one record with the next
  d3 = d2 %>%
    mutate(lagRadius = lag(Radius),
           Growth = Radius - lagRadius)

  # Find where the lag exceeds the threshold
  jumps = d3 %>%
    filter(abs(Growth) > jump.threshold) %>%
    rename(Jump = Growth) %>%
    mutate(cumulativeJump = cumsum(Jump),
           jumpStart = "yes") %>%
    select(Dendrometer, Timestamp, Jump, cumulativeJump, jumpStart)

  # Join the jump indicator df onto the main df, and adjust all values accordingly
  d4 = d3 %>%
    left_join(jumps, by = c("Dendrometer", "Timestamp")) %>%
    mutate(cumulativeJump = ifelse(Timestamp == min(d3$Timestamp, na.rm = T),
                                   0, cumulativeJump)) %>%
    fill(cumulativeJump, .direction = "down") %>%
    mutate(newRadius = Radius - cumulativeJump, na.rm = T) %>%
    select(Dendrometer, Timestamp, Radius, newRadius, jumpStart)
  return(d4)
}

# This actually applies the funtion. Change threshold here- try 5 and try 50 to see the difference in the plot
newDat <- fix_jumps(50)

# Create df of jump locations, so they can be plotted
newDat.jumpStart <- newDat %>%
  filter(jumpStart == "yes")

# Plot- show original data (dashed) and new data (solid). Blue circles indicate the jumps
ggplot() +
  geom_line(data = newDat, aes(x = Timestamp, y = newRadius)) +
  geom_line(data = newDat, aes(x = Timestamp, y = Radius), linetype = "dashed") +
  geom_point(data = newDat.jumpStart,
             aes(x = Timestamp, y = newRadius), color = "blue") +
  theme_bw() +
  scale_x_datetime(date_breaks="1 month", date_labels="%m-%Y") +
  ggtitle(unique(newDat$Dendrometer))
