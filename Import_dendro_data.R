# This script imports all the files in the "import" folder for each tree, combines into a long df, and does some formatting. After this, the saved data are ready to be directly used in the app

# These are the times I know it was recorded in micrometers:
# ET2b, between "2023-03-28 16:45:00" and "2023-05-04 16:30:00"
# TV4a, between "2023-01-10 17:45:00" and 2023-02-18 01:00:00"

# Import data -------------------------------------------------------------

library(needs)
needs(tidyverse, here, lubridate, readxl)

read_dendro_data <- function(x){
  read_csv2(x, show_col_types = F,
            col_names = c(".id", "Time", "Moisture", "T1", "T2", "T3",
                          "Dendro", "X8", "X9", "X10")) %>%
    mutate(Time = ymd_hm(Time, tz = "UTC"),
           DendroUnits = ifelse(
             str_detect(x, "um") == T, "Micrometers", "Tomst")) %>%
           select(.id, Time, Moisture, T1, T2, T3, Dendro, DendroUnits)
}

DendroVec <- c(
  "ET1a", "ET2a", "ET2b", "ET3a", "ET4a", "ET4b", "ET5a", "ET6a", "ET7a", "ET8a",
  "FB1a", "FB2a", "FB3a", "FB3b", "FB4a", "FB4b",
  "FB5a", "FB5b", "FB6a", "FB6b", "FB7a", "FB7b", "FB8a",
  "TV1a", "TV2a", "TV3a", "TV4a")

i <- "TV4a"
for(i in DendroVec){
  Tree.ID = str_sub(i, start = 1, end = 3)

  filenames = list.files(here("Dendro_data_import", Tree.ID), full.names = T)
  filenames2 = filenames[which(str_detect(filenames, i) == T)]

  # Find timestamp where data starts
  baseline = read_excel(here("Dendro_data_supporting", "Tomst_install.xlsx")) %>%
    filter(DendroID == i)
  baseline2 = as.POSIXct(as.character((baseline[1,4])), tz = "UTC")

  d = lapply(filenames2, read_dendro_data) %>%
    bind_rows() %>%
    filter(Time > (baseline2 + 60000)) %>%
    mutate(DendroID = i) %>%
    select(DendroID, everything())

  d2 = d %>%
    mutate(DendroUnits = ifelse(
      DendroID == "ET2b" &
        Time >= as.POSIXct("2023-03-28 16:45:00", tz = "UTC") &
        Time <= as.POSIXct("2023-05-04 16:30:00", tz = "UTC"),
      "Micrometers", DendroUnits)) %>%
    mutate(DendroUnits = ifelse(
      DendroID == "TV4a" &
        Time >= as.POSIXct("2023-01-10 17:45:00", tz = "UTC") &
        Time <= as.POSIXct("2023-02-18 01:00:00", tz = "UTC"),
      "Micrometers", DendroUnits))

  d3 = d2 %>%
    mutate(Radius = ifelse(DendroUnits == "Tomst",
                           (Dendro-1278)*(8890/(34000-1278)), Dendro)) %>%
    select(DendroID, .id, Time, Moisture, T1, T2, T3, Radius)

  # Check for duplicate timestamps and remove first. Removes the last obs but thats ok
  d4 = d3 %>%
    mutate(Timelead = lead(Time)) %>%
    filter(Time != Timelead) %>%
    select(-Timelead)

  out = str_c("Dendro_data_LVL1/", i, "_Dendro_LVL1.csv")
  write_csv(d4, out)

  print(i)
}

# Find_download_dates -----------------------------------------------------

x <- "ET6a"
find_dendro_DL <- function(x){
  Tree.ID = str_sub(x, start = 1, end = 3)

  filenames = list.files(here("Dendro_data_import", Tree.ID), full.names = T)
  filenames2 = filenames[which(str_detect(filenames, x) == T)]

  d = data.frame(Dendro = x, filename = filenames2) %>%
    mutate(dateloc = str_locate(filename, "202")[,"start"],
           DL_date = str_sub(filename, start = dateloc, end = dateloc + 9)) %>%
    select(Dendro, DL_date)
  return(d)
}
Dendro_DL <- lapply(DendroVec, find_dendro_DL) %>%
  bind_rows %>%
  separate(Dendro, into = c("Tree", "Dendro"), 3)

write_csv(Dendro_DL, here("Dendro_data_supporting", "Dendro_DL.csv"))







# Extra processing --------------------------------------------------------


## Function to eliminate jumps --------------------------------------------

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

## Apply function ----------------------------------------------------------

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

## Viewer ------------------------------------------------------------------

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

