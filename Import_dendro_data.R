# This script imports all the files in the "import" folder for each tree, combines into a long df, and does some formatting. After this, the saved data are ready to be directly used in the app

# These are the times I know it was recorded in micrometers:
# ET2b, between "2023-03-28 16:45:00" and "2023-05-04 16:30:00"
# TV4a, between "2023-01-10 17:45:00" and 2023-02-18 01:00:00"

library(needs)
needs(tidyverse, here, lubridate, readxl)

# Constants, functions, and directories -----------------------------------

# Define a function to read and preprocess dendrometer data
read_dendro_data <- function(x){
  suppressMessages(read_csv2(x, show_col_types = F,
            col_names = c(".id", "Timestamp", "Moisture", "T1", "T2", "T3",
                          "Dendro", "X8", "X9", "X10"))) %>%
    # Parse the "Time" column as posixct
    mutate(Timestamp = ymd_hm(Timestamp, tz = "UTC"),
           # Identify files that were mistakenly downloaded as micrometers
           DendroUnits = ifelse(
             str_detect(x, "um") == T, "Micrometers", "Tomst")) %>%
           select(.id, Timestamp, Moisture, T1, T2, T3, Dendro, DendroUnits)
}

# Define a vector of DendroIDs
DendroVec <- c(
  "ET1a", "ET2a", "ET2b", "ET3a", "ET4a", "ET4b", "ET5a", "ET5b", "ET6a",
  "ET7a", "ET8a", "FB1a", "FB2a", "FB3a", "FB3b", "FB4a", "FB4b",
  "FB5a", "FB5b", "FB6a", "FB6b", "FB7a", "FB7b", "FB8a",
  "TV1a", "TV2a", "TV3a", "TV4a")

# Directory paths
data_raw_dir <- here("Dendro_data_raw")
data_support_dir <- here("Dendro_data_supporting")

baseline <- read_excel(file.path(data_support_dir, "Dendro_metadata.xlsx"))

# Import data -------------------------------------------------------------

i <- DendroVec[1]
Log <- NULL
for(i in DendroVec){
  # Extract the first three characters to get Tree.ID
  TreeID <- str_sub(i, start = 1, end = 3)

  # List files in a directory related to the current Tree.ID
  filenames <- list.files(file.path(data_raw_dir, TreeID), full.names = T)
  # Filter filenames to select those matching the current dendrometer
  filenames <- filenames[which(str_detect(filenames, i) == T)]

  if (length(filenames) == 0) {
    # Log <- c(Log, cat("No files found for", i, "\n"))
    Log <- c(Log, str_c("No files found for", i))
    next  # Skip to the next DendroID
  }

  # Find timestamp where data starts
  baseline_time <- baseline %>%
    filter(Dendrometer == i) %>%
    pull(Install.Datetime)

  if (length(baseline_time) == 0) {
    # Log <- c(Log, cat("No baseline data found for", i, "\n"))
    Log <- c(Log, str_c("No baseline data found for", i))
    next  # Skip to the next DendroID
  }

  # Read, process, and filter data from multiple CSV files
  data_list <- lapply(filenames, read_dendro_data)

  data_combined <- data_list %>%
    bind_rows() %>%
    filter(Timestamp > (ymd_hms(baseline_time, tz = "UTC") + days(1))) %>%
    mutate(Dendrometer = i) %>%
    select(Dendrometer, everything()) %>%
    distinct() %>%
    arrange(Timestamp)

  # Fix the occasional accidental micrometer downloads
  data_combined2 <- data_combined %>%
    mutate(DendroUnits = ifelse(
      Dendrometer == "ET2b" &
        Timestamp >= as.POSIXct("2023-03-28 16:45:00", tz = "UTC") &
        Timestamp <= as.POSIXct("2023-05-04 16:30:00", tz = "UTC"),
      "Micrometers", DendroUnits)) %>%
    mutate(DendroUnits = ifelse(
      Dendrometer == "TV4a" &
        Timestamp >= as.POSIXct("2023-01-10 17:45:00", tz = "UTC") &
        Timestamp <= as.POSIXct("2023-02-18 01:00:00", tz = "UTC"),
      "Micrometers", DendroUnits))

  # Convert radius to Micrometers (always less than Tomst)
  data_combined3 <- data_combined2 %>%
    mutate(Radius = ifelse(DendroUnits == "Tomst",
                           (Dendro-1278)*(8890/(34000-1278)), Dendro)) %>%
    select(Dendrometer, .id, Timestamp, Moisture, T1, T2, T3, Radius)

  # Check for duplicate timestamps and remove first. Removes the last obs but thats ok. At some point would be good to check and make sure this isn't deleting extra things
  data_combined4 <- data_combined3 %>%
    mutate(Timelead = lead(Timestamp)) %>%
    filter(Timestamp != Timelead) %>%
    select(-Timelead)
  # which(duplicated(d4$Time) == TRUE)

  out <- str_c("Dendro_data_LVL1/", i, "_Dendro_LVL1.csv")
  write_csv(data_combined4, out)

  # Log <- c(Log, cat("Processed", i, "\n"))
  Log <- c(Log, str_c("Processed", i))
}

# Find_download_dates -----------------------------------------------------

x <- "ET6a"
find_dendro_DL <- function(x){
  TreeID = str_sub(x, start = 1, end = 3)

  filenames = list.files(here("Dendro_data_import", TreeID), full.names = T)
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

## Treenetproc -------------------------------------------------------------

# library(devtools)
# devtools::install_github("treenet/treenetproc")
library(treenetproc)

data("dendro_data_L0")

dendro_L1 <- proc_L1(data_L0 = dendro_data_L0, reso = 10, input = "long")
# temp_L1 <- proc_L1(data_L0 = temp_L0, reso = 15, input = "long")

dendro_L2 <- proc_dendro_L2(dendro_L1 = dendro_L1,plot = T)

grow_seas(dendro_L2 = dendro_L2)

my_L0 <- read_csv(file.path("Dendro_data_LVL1", "ET6a_Dendro_LVL1.csv")) %>%
  select(series = Dendrometer, ts = Timestamp, value = Radius)

temp_L0 <- read_csv(file.path("Dendro_data_LVL1", "ET6a_Dendro_LVL1.csv")) %>%
  select(series = Dendrometer, ts = Timestamp, value = T1)

my_L1 <- proc_L1(data_L0 = my_L0, reso = 15, input = "long")
temp_L1 <- proc_L1(data_L0 = temp_L0, reso = 15, input = "long")

my_L2 <- proc_dendro_L2(dendro_L1 = my_L1, temp_L1 = temp_L1,
                     plot = T)

grow_seas(dendro_L2 = my_L2)
stats <- phase_stats(dendro_L2 = my_L2, plot_phase = T)

## Function to eliminate jumps --------------------------------------------




newfilenames <- list.files(file.path("Dendro_data_LVL1"), full.names = T)
newdat <- lapply(newfilenames, read_csv, show_col_types = F)

jump.cutoff <- read_excel(here("Dendro_data_supporting", "Tomst_install.xlsx")) %>%
  mutate(Jump.cutoff = as.numeric(Jump.cutoff))

DendroVec <- c("ET1", "ET2a", "ET2b", "ET3", "ET4a", "ET4b", "ET5",
               "ET6a", "ET7", "ET8", "FB1", "FB2a", "FB3a", "FB3b", "FB4a", "FB4b",
               "FB5a", "FB5b", "FB6a", "FB7a", "FB8a", "TV1a", "TV2a", "TV3a", "TV4a")

# y is the biggest acceptable single-step increase

d <- read_csv("Dendro_data_LVL1/ET2a_Dendro_LVL1.csv")

fix_jumps <- function(x){

  x2 = d

  x3 = x2 %>%
    mutate(lagRadius = lag(Radius),
           Growth = Radius - lagRadius)

  jumps = x3 %>%
    filter(abs(Growth) > y) %>%
    rename(Jump = Growth) %>%
    mutate(cumulativeJump = cumsum(Jump),
           jumpStart = "yes") %>%
    select(Dendrometer, Timestamp, Jump, cumulativeJump, jumpStart)

  x4 = x3 %>%
    left_join(jumps, by = c("Dendrometer", "Timestamp")) %>%
    mutate(cumulativeJump = ifelse(Timestamp == min(x3$Timestamp, na.rm = T),
                                   0, cumulativeJump)) %>%
    fill(cumulativeJump, .direction = "down") %>%
    mutate(newRadius = Radius - cumulativeJump, na.rm = T) %>%
    select(Dendrometer, Timestamp, Radius, newRadius, jumpStart)
  return(x4)
}

# Change threshold here- try 5 and try 50 to see the difference in the plot
d2 <- fix_jumps(50)

d2.jumpStart <- d2 %>%
  filter(jumpStart == "yes")

# Plot- show original data (dashed) and new data (solid). Blue circles indicate the jumps
ggplot() +
  geom_line(data = d2, aes(x = Timestamp, y = newRadius)) +
  geom_line(data = d2, aes(x = Timestamp, y = Radius), linetype = "dashed") +
  geom_point(data = d2.jumpStart,
             aes(x = Timestamp, y = newRadius), color = "blue") +
  theme_bw() +
  scale_x_datetime(date_breaks="1 month", date_labels="%m-%Y") +
  ggtitle(unique(d2$Dendrometer))

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

