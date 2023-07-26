library(needs)
needs(tidyverse, here, lubridate, readxl, plotly)


# Function to eliminate jumps -------------------------------------------------------

jump.cutoff <- read_excel(here("Dendro_data_supporting", "Tomst_install.xlsx")) %>% 
  mutate(Jump.cutoff = as.numeric(Jump.cutoff))

DendroVec <- c("ET1", "ET2a", "ET2b", "ET3", "ET4a", "ET4b", "ET5",  
               "ET6a", "ET7", "ET8", "FB1", "FB2a", "FB3a", "FB3b", "FB4a", "FB4b", 
               "FB5a", "FB5b", "FB6a", "FB7a", "FB8a", "TV1a", "TV2a", "TV3a", "TV4a")

# y is the biggest acceptable single-step increase

x <- DendroVec[3]
y <- 5
fix_jumps <- function(x){
  
  jump.cut2 <- jump.cutoff %>% 
    filter(DendroID == x) 
  
  x2 = read_csv(str_c("Dendro_data_LVL2/", x, "_Dendro_LVL2.csv"), 
                show_col_types = F)
  
  x3 = x2 %>% 
    mutate(lagRadius = lag(Radius),
           Growth = Radius - lagRadius)
  
  # The jump comes right before the indicated timestamp, so should be subtracted from all including the indicated
  # jumps = x3 %>% 
  #   filter(Growth > y) %>% 
  #   rename(Jump = Growth) %>% 
  #   mutate(cumulativeJump = cumsum(Jump),
  #          jumpStart = "y") %>% 
  #   select(DendroID, date_time, Jump, cumulativeJump, jumpStart) 
  
  jumps = x3 %>% 
    filter(abs(Growth) > unique(jump.cut2$Jump.cutoff)) %>% 
    rename(Jump = Growth) %>% 
    mutate(cumulativeJump = cumsum(Jump),
           jumpStart = "y") %>% 
    select(DendroID, date_time, Jump, cumulativeJump, jumpStart)
  
  x4 = x3 %>% 
    left_join(jumps, by = c("DendroID", "date_time")) %>% 
    mutate(cumulativeJump = ifelse(date_time == min(x3$date_time, na.rm = T), 
                                   0, cumulativeJump)) %>% 
    fill(cumulativeJump, .direction = "down") %>% 
    mutate(newRadius = Radius - cumulativeJump, na.rm = T) %>% 
    select(DendroID, date_time, Radius, newRadius, jumpStart)
  return(x4)
}

d2 <- fix_jumps(DendroVec[25])
d2.jumpStart <- d2 %>% 
  filter(jumpStart == "y")

ggplot() +
  geom_line(data = d2, aes(x = date_time, y = newRadius)) +
  geom_line(data = d2, aes(x = date_time, y = Radius), linetype = "dashed") +
  geom_point(data = d2.jumpStart, aes(x = date_time, y = newRadius), color = "blue") +
  theme_bw() +
  scale_x_datetime(date_breaks="1 month", date_labels="%m-%Y") +
  ggtitle(unique(d2$DendroID))

# ggplotly(ggplot() +
#   geom_line(data = d2, aes(x = date_time, y = newRadius)) +
#   geom_line(data = d2, aes(x = date_time, y = Radius), linetype = "dashed") +
#   geom_point(data = d2.jumpStart, aes(x = date_time, y = newRadius), color = "blue") +
#   theme_bw() +
#   scale_x_datetime(date_breaks="1 month", date_labels="%m-%Y") +
#   ggtitle(unique(d2$DendroID)))

# Apply function ----------------------------------------------------------

fix_jumps_and_save <- function(x, y){
  
  jump.cut2 <- jump.cutoff %>% 
    filter(DendroID == x) 
  
  x2 = read_csv(str_c("Dendro_data_LVL2/", x, "_Dendro_LVL2.csv"), 
                show_col_types = F)
  
  x3 = x2 %>% 
    mutate(lagRadius = lag(Radius),
           Growth = Radius - lagRadius)
  
  # The jump comes right before the indicated timestamp, so should be subtracted from all including the indicated
  jumps = x3 %>% 
    filter(abs(Growth) > unique(jump.cut2$Jump.cutoff)) %>% 
    rename(Jump = Growth) %>% 
    mutate(cumulativeJump = cumsum(Jump),
           jumpStart = "y") %>% 
    select(DendroID, date_time, Jump, cumulativeJump, jumpStart)
  
  x4 = x3 %>% 
    left_join(jumps, by = c("DendroID", "date_time")) %>% 
    mutate(cumulativeJump = ifelse(date_time == min(x3$date_time, na.rm = T), 
                                   0, cumulativeJump)) %>% 
    fill(cumulativeJump, .direction = "down") %>% 
    mutate(newRadius = Radius - cumulativeJump, na.rm = T) %>% 
    select(DendroID, date_time, Radius, newRadius, jumpStart)
  
  write_csv(x4, str_c("Dendro_data_LVL3/", x, "_Dendro_LVL3.csv"))
}

DendroVec <- c("ET1", "ET2a", "ET2b", "ET3", "ET4a", "ET4b", "ET5",  
               "ET6a", "ET7", "ET8", "FB1", "FB2a", "FB3a", "FB3b", "FB4a", "FB4b", 
               "FB5a", "FB5b", "FB6a", "FB7a", "FB8a", "TV1a", "TV2a", "TV3a", "TV4a")

lapply(DendroVec, fix_jumps_and_save)

# Viewer ------------------------------------------------------------------

plot_tomst <- function(x){
  ggplot(x) +
    geom_line(aes(x = date_time, y = Radius)) +
    theme_bw() +
    scale_x_datetime(date_breaks="1 month", date_labels="%m-%Y") +
    ggtitle(unique(x$DendroID))
}

Dendro.ID <- "FB1a"
d <- read_csv(str_c("Dendro_data_LVL2/", Dendro.ID, "_Dendro_LVL2.csv"))
plot_tomst(d)

p <- plot_tomst(d)
ggplotly(p)

read_and_plot_tomst <- function(x){
  d = read_csv(str_c("Dendro_data_LVL3/", x, "_Dendro_LVL3.csv"))
  ggplot(d) +
    geom_line(aes(x = date_time, y = newRadius)) +
    theme_bw() +
    scale_x_datetime(date_breaks="1 month", date_labels="%m-%Y") +
    ggtitle(unique(d$DendroID))
}

DendroVec <- c("ET1", "ET2a", "ET2b", "ET3", "ET4a", "ET4b", "ET5",  
               "ET6a", "ET7", "ET8", "FB1", "FB2a", "FB3a", "FB3b", "FB4a", "FB4b", 
               "FB5a", "FB5b", "FB6a", "FB7a", "FB8a", "TV1a", "TV2a", "TV3a", "TV4a")

read_and_plot_tomst(DendroVec[1])

lapply(DendroVec, read_and_plot_tomst)

