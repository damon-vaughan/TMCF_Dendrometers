# This workflow starts at LVL1- Elenter creates it manually. Download from Drive. Ignore Raw for now

library(needs)
needs(tidyverse, lubridate, readxl, plotly)

# For loop ----------------------------------------------------------------

DendroVec <- c(
  "ET1", "ET2a", "ET2b", "ET3", "ET4a", "ET4b", "ET5",
               "ET6a", "ET7", "ET8", "FB1", "FB2", "FB3a", "FB3b", "FB4a", "FB4b",
               "FB5a", "FB5b", "FB6a", "FB6b", "FB7a", "FB7b", "FB8", 
               "TV1", "TV2", "TV3", "TV4a")

# Good: c("ET6a", "ET7a", "FB1a", "FB2a", "FB4a")
# Strange: c("ET1a", "ET8a", "FB6a", "FB7a", "FB8a", "TV1a", "TV4a")

DendroVec <- c("TV3a", "TV4a")

i <- "FB2"

for(i in DendroVec){
  Tomst.install <- read_excel("Dendro_data_supporting/Tomst_install.xlsx")
  
  baseline <- Tomst.install %>% 
    filter(DendroID == i)
  baseline2 <- as.POSIXct(as.character((baseline[1,4])), tz = "UTC")
  
  d <- read_csv2(str_c("Dendro_data_LVL1/", i, "_Dendro_LVL1.csv"),   
                 col_names = c(".id", "date_time", "Moisture", "T1", "T2", "T3", 
                               "Tomst", "X8", "X9", "X10"),
                 show_col_types = F) %>%
    mutate(Timestamp = ymd_hm(date_time, tz = "UTC")) %>% 
    select(.id, Timestamp, Moisture, T1, T2, T3, Tomst)
  
  # Filter to start of good data
  d2 <- d %>%
    arrange(Timestamp) %>% 
    filter(Timestamp > (baseline2 + 60000)) 
  
  # Check for duplicate timestamps and remove first. Removes the last obs but thats ok
  d3 <- d2 %>% 
    mutate(Timelead = lead(Timestamp)) %>% 
    filter(Timestamp != Timelead)
  
  # Assign flag if the data were downloaded in micrometers rather than raw
  d4 <- d3 %>% 
    mutate(units = ifelse(i == "ET2b" & 
                            Timestamp >= as.POSIXct("2023-03-28 16:45:00", tz = "UTC") &
                            Timestamp <= as.POSIXct("2023-05-04 16:30:00", tz = "UTC"),
                          "Micrometers", "Tomst")
           # ,
           # units = ifelse(i == "TV4a" &
           #                  Timestamp >= as.POSIXct("2023-01-10 17:45:00", tz = "UTC") &
           #                  Timestamp <= as.POSIXct("2023-02-18 01:00:00", tz = "UTC"),
           #                "Micrometers", "Tomst")
           )
  
# Make radius column that converts raw Tomst to micrometers. If it was downloaded in micrometers, don't do the conversion
  d5 <- d4 %>% 
    mutate(Radius = ifelse(units == "Tomst", (Tomst-1278)*(8890/(34000-1278)),
                           Tomst)) 
  
  # Last line gives dendrometers with no letter ID (when only 1 is in a tree and it wasn't renamed) the letter "a" 
  d6 <- d5 %>% 
    mutate(DendroID = i) %>%
    separate(DendroID, into = c("Tree", "Dendro"), sep = 3) %>% 
    select(Tree, Dendro, .id, Timestamp, Moisture, T1, T2, T3, Radius)  %>% 
    mutate(Dendro = ifelse(str_length(i) == 3, "a", Dendro))
  
  write_csv(d6, str_c("Dendro_data_LVL2/", i, "_Dendro_LVL2.csv"))
  print(i)
}

# Viewing -----------------------------------------------------------------

library(needs)
needs(tidyverse, lubridate, readxl, plotly)

# Good: c("ET6a", "ET7a", "FB1a", "FB2a", "FB4a")
# Strange: c("ET1a", "ET8a", "FB6a", "FB7a", "FB8a", "TV1a", "TV4a")

plot_tomst <- function(x){
  ggplot(x) +
    geom_line(aes(x = date_time, y = Radius)) +
    theme_bw() +
    scale_x_datetime(date_breaks="1 month", date_labels="%m-%Y") +
    ggtitle(unique(x$DendroID))
}

read_and_plot_tomst <- function(x){
  d = read_csv(str_c("Dendro_data_LVL2/", x, "_Dendro_LVL2.csv"))
  ggplot(d) +
    geom_line(aes(x = date_time, y = Radius)) +
    theme_bw() +
    scale_x_datetime(date_breaks="1 month", date_labels="%m-%Y") +
    ggtitle(unique(d$DendroID))
}

DendroVec <- c("ET1", "ET2a", "ET2b", "ET3", "ET4a", "ET4b", "ET5",  
               "ET6a", "ET7", "ET8", "FB1", "FB2a", "FB3a", "FB3b", "FB4a", "FB4b", 
               "FB5a", "FB5b", "FB6a", "FB7a", "FB8a", "TV1a", "TV2a", "TV3a", "TV4a")

lapply(DendroVec, read_and_plot_tomst)

Dendro.ID <- DendroVec[1]
d <- read_csv(str_c("Dendro_data_LVL2/", Dendro.ID, "_Dendro_LVL2.csv"))
p <- plot_tomst(d)
p

ggplotly(p)


# One at a time -------------------------------------------------------

# c("ET1a", "ET2a", "ET2b", "ET3a", "ET4a", "ET5", "ET6a", "ET7a", "ET8a",
# "FB1a", "FB2a", "FB3a", "FB3b", "FB4a", "FB4b", "FB5a", "FB5b", "FB6a", "FB7a", "FB8a",
# "TV1a", "TV2a", "TV3a", "TV4a")

# Good: c("ET6a", "ET7a", "FB1a", "FB2a", "FB4a")
# Strange: c("ET1a", "ET8a", "FB6a", "FB7a", "FB8a", "TV1a", "TV4a")

Tomst.install <- read_excel("Dendro_data_supporting/Tomst_install.xlsx")

Dendro.ID <- "ET2a"
# filename <- "ET1_2022_09_30_0.csv" 

baseline <- Tomst.install %>% 
  filter(DendroID == Dendro.ID)
baseline2 <- as.POSIXct(as.character((baseline[1,4])), tz = "UTC")

# T2 and T3 just read -200 the entire time
# Temp is temperature in Celsius. Dendro is the dendrometer reading in Tomst units. The other columns are not important

d <- read_csv2(str_c("Dendro_data_LVL1/", Dendro.ID, "_Dendro_LVL1.csv"),   
               col_names = c(".id", "date_time", "Moisture", "T1", "T2", "T3", 
                             "Tomst", "X8", "X9", "X10")) %>%
  mutate(date_time = ymd_hm(date_time, tz = "UTC")) %>% 
  select(-X8, -X9, -X10)

d2 <- d %>%
  filter(date_time > (baseline2 + 60000))

d3 <- d2 %>% 
  mutate(Radius = (Tomst-1278)*(8890/(34000-1278))) %>%
  # mutate(Radius = Tomst) %>%
  mutate(DendroID = as.factor(Dendro.ID)) %>% 
  select(DendroID, .id, date_time, Moisture, T1, T2, T3, Radius)

write_csv(d3, str_c("Dendro_data_LVL2/", Dendro.ID, "_Dendro_LVL2.csv"))


