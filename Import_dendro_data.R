# This script imports all the files in the "import" folder for each tree, combines into a long df, and does some formatting. After this, the saved data are ready to be directly used in the app

# These are the times I know it was recorded in micrometers:
# ET2b, between "2023-03-28 16:45:00" and "2023-05-04 16:30:00"
# TV4a, between "2023-01-10 17:45:00" and 2023-02-18 01:00:00"

# devtools::install_github("treenet/treenetproc")
library(needs)
needs(tidyverse, lubridate, readxl, treenetproc)

options(readr.show_col_types = FALSE)
source("Functions_dendro.R")

# Data and directories -----------------------------------

# Directory paths
data.raw.dir <- file.path("Dendro_data_raw")
data.support.dir <- file.path("Dendro_data_supporting")

baseline <- read_excel(file.path(data.support.dir, "Dendro_metadata.xlsx"))

import.log <- read_csv(file.path("Dendro_data_supporting",
                                 "Dendro_import_log.csv"),
                       show_col_types = F)

# Import data -------------------------------------------------------------

Log <- NULL
dendro.vec <- dendro.vec.full
# i <- "FB8b"
for(i in dendro.vec){
  # Extract the first three characters to get Tree.ID
  TreeID <- str_sub(i, start = 1, end = 3)

  # List files in a directory related to the current Tree.ID
  filenames <- list.files(file.path(data.raw.dir, TreeID), full.names = T)
  # Filter filenames to select those matching the current dendrometer
  filenames <- filenames[which(str_detect(filenames, i) == T)]

  if (length(filenames) == 0) {
    # Log <- c(Log, cat("No files found for", i, "\n"))
    cat("No files found for", i, "\n")
    next  # Skip to the next DendroID
  }

  # Find timestamp where data starts
  baseline_time <- baseline %>%
    filter(Dendrometer == i) %>%
    pull(Install.Datetime)

  if (length(baseline_time) == 0) {
    # Log <- c(Log, cat("No baseline data found for", i, "\n"))
    Log <- cat("No baseline data found for", i, "\n")
    next  # Skip to the next DendroID
  }

  # Read, process, and filter data from multiple CSV files
  data.list <- lapply(filenames, read_dendro_data)

  data.combined <- data.list %>%
    bind_rows() %>%
    filter(Timestamp > (ymd_hms(baseline_time, tz = "UTC") + days(1))) %>%
    mutate(Dendrometer = i) %>%
    select(Dendrometer, everything()) %>%
    distinct() %>%
    arrange(Timestamp)

  # Fix the occasional accidental micrometer downloads
  data.combined2 <- data.combined %>%
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
  data.combined3 <- data.combined2 %>%
    mutate(Radius = ifelse(DendroUnits == "Tomst",
                           (Dendro-1278)*(8890/(34000-1278)), Dendro)) %>%
    select(Dendrometer, .id, Timestamp, Moisture, T1, T2, T3, Radius)

  # Check for duplicate timestamps and remove first. Removes the last obs but thats ok. At some point would be good to check and make sure this isn't deleting extra things
  data.combined4 <- data.combined3 %>%
    mutate(Timelead = lead(Timestamp)) %>%
    filter(Timestamp != Timelead) %>%
    select(-Timelead) %>%
    mutate(TWD = 0)
  # which(duplicated(d4$Time) == TRUE)

  out <- str_c("Dendro_data_L1/", i, "_Dendro_L1.csv")
  write_csv(data.combined4, out)

  # Update import logs
  import.log <- read_csv(file.path("Dendro_data_supporting",
                                   "Dendro_import_log.csv"),
                         show_col_types = F)

  import.log2 <- import.log %>%
    mutate(Last.import = ifelse(
      Dendrometer == i, max(data.combined4$Timestamp, na.rm = T),
      Last.import)) %>%
    mutate(Last.import = as_datetime(Last.import))

  write_csv(import.log2, file.path("Dendro_data_supporting",
                                  "Dendro_import_log.csv"))
  # Log <- c(Log, cat("Processed", i, "\n"))
  cat("Processed", i, "\n")
}

## Find_download_dates -----------------------------------------------------

# x <- "ET6a"
find_dendro_DL <- function(x){
  TreeID = str_sub(x, start = 1, end = 3)

  filenames = list.files(file.path("Dendro_data_raw", TreeID), full.names = T)
  filenames2 = filenames[which(str_detect(filenames, x) == T)]

  d = data.frame(Dendro = x, filename = filenames2) %>%
    mutate(dateloc = str_locate(filename, "202")[,"start"],
           DL_date = str_sub(filename, start = dateloc, end = dateloc + 9)) %>%
    select(Dendro, DL_date)
  return(d)
}
Dendro_DL <- lapply(dendro.vec.full, find_dendro_DL) %>%
  bind_rows() %>%
  separate(Dendro, into = c("Tree", "Dendro"), 3)

write_csv(Dendro_DL, file.path("Dendro_data_supporting", "Dendro_DL.csv"))


# L1 to L2 ----------------------------------------------------------------

dendro.vec <- dendro.vec.full

# i <- "ET1a"
for(i in dendro.vec){
  filename.in.L1 <- str_c("Dendro_data_L1/", i, "_Dendro_L1.csv")

  if(file.exists(filename.in.L1)){
    d <- read_csv(filename.in.L1, show_col_types = F)
  } else {
    next
    cat("No L1 file for", i, "\n")
  }

  d2 <- d %>%
    select(series = Dendrometer, ts = Timestamp, value = Radius) %>%
    proc_L1(reso = 15) %>%
    proc_dendro_L2(tol_out = 1000, tol_jump = 10, plot_export = F, plot = F)

  gro.yr.reset <- d2 %>%
    group_by(Year = year(ts)) %>%
    summarise(Add.to.vals = last(gro_yr)) %>%
    mutate(Year = Year + 1) %>%
    mutate(Add.to.vals2 = cumsum(Add.to.vals)) %>%
    select(Year, Add.to.vals2)

  d3 <- d2 %>%
    rename(Dendrometer = series, Timestamp = ts, Radius = value) %>%
    mutate(Year = year(Timestamp)) %>%
    left_join(gro.yr.reset, by = "Year") %>%
    replace_na(list(Add.to.vals2 = 0)) %>%
    mutate(gro_yr = gro_yr + Add.to.vals2) %>%
    select(Dendrometer, Timestamp, Radius, TWD = twd, Growth = gro_yr)

  d4 <- d3 %>%
    ungroup() %>%
    mutate(Radius = Radius - first(Radius))

  write_csv(d4, file.path("Dendro_data_L2",
                          str_c(i, "_Dendro_L2.csv")))
}


# Other -------------------------------------------------------------------

## Sensor changes --------------------------------------------

tree.vec <- full.tree.vec

laptop.filepath <- "C:/Users/vaug8/OneDrive - University of Kentucky/TMCF/Continuous_data/Maintenance notes"
desktop.filepath <- "C:/Users/User/OneDrive - University of Kentucky/TMCF/Continuous_data/Maintenance notes"

# change here
filepath <- desktop.filepath

x <- "ET1"
read_sheet <- function(x){
  sheet = read_excel(file.path(filepath, "SensorNotes_All.xlsx"), sheet = x) %>%
    filter(Part == "a" | Part == "b") %>%
    mutate(Tree = x) %>%
    select(Tree, Dendro = Part, everything(), -Location)
  # Warning is ok
  sheet2 = sheet %>%
    pivot_longer(3:ncol(sheet), names_to = "Date", values_to = "Action") %>%
    separate(Action, into = c("Action", "Junk"), sep = ";") %>%
    select(-Junk)
  return(sheet2)
}
read_sheet("ET1")

d <- lapply(full.tree.vec, read_sheet) %>%
  bind_rows() %>%
  na.omit()

# Warning ok- just bc most entries don't have notes
d2 <- d %>%
  mutate(Date = str_sub(Date, start = 1, end = 10),
         ToD = "10:00:00",
         Timestamp = str_c(Date, ToD, sep = " ")) %>%
  select(Tree, Dendro, Timestamp, Action) %>%
  mutate(Timestamp = ymd_hms(Timestamp, tz = "UTC"))

write_csv(d2, "Dendro_data_supporting/Dendro_maintenance_actions.csv")

# Future: L2 to L3 ----------------------------------------------------------

# d <- read_csv(file.path("Dendro_data_L2", "ET4a_Dendro_L2.csv"),
#               guess_max = 10000)
# problems(d)

# Old L1 to L2, using my function -----------------------------------------

# dendro.vec <- dendro.vec.full
#
# # i <- "ET4a"
# for(i in dendro.vec){
#   filename.in.L1 <- str_c("Dendro_data_L1/", i, "_Dendro_L1.csv")
#
#   if(file.exists(filename.in.L1)){
#     d <- read_csv(filename.in.L1, show_col_types = F)
#   } else {
#     next
#     cat("No L1 file for", i, "\n")
#   }
#
#   d2 <- d %>%
#     fix_jumps(10)
#
#   d3 <- d2 %>%
#     ungroup() %>%
#     mutate(Radius = Radius - first(Radius))
#
#   write_csv(d3, file.path("Dendro_data_L2",
#                       str_c(i, "_Dendro_L2.csv")))
# }
#
# x <- d
# y <- 10
# fix_jumps <- function(x, y){
#
#   x2 = x
#
#   x3 = x2 %>%
#     mutate(lagRadius = lag(Radius),
#            Growth = Radius - lagRadius)
#
#   # The jump comes right before the indicated timestamp, so should be subtracted from all including the indicated
#   jumps = x3 %>%
#     filter(abs(Growth) > y) %>%
#     rename(Jump = Growth) %>%
#     mutate(cumulativeJump = cumsum(Jump),
#            jumpStart = "yes") %>%
#     select(Dendrometer, Timestamp, Jump, cumulativeJump, jumpStart)
#
#   x4 = x3 %>%
#     left_join(jumps, by = c("Dendrometer", "Timestamp")) %>%
#     mutate(cumulativeJump = ifelse(Timestamp == min(x3$Timestamp, na.rm = T),
#                                    0, cumulativeJump)) %>%
#     fill(cumulativeJump, .direction = "down") %>%
#     mutate(Radius.OG = Radius,
#            Radius = Radius - cumulativeJump) %>%
#     mutate(jumpStart = ifelse(jumpStart == "yes", Radius, jumpStart)) %>%
#     select(Dendrometer, Timestamp, Radius.OG, Radius, jumpStart)
#   return(x4)
# }
