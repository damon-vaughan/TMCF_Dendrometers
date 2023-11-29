# This script imports all the files in the "import" folder for each tree, combines into a long df, and does some formatting. After this, the saved data are ready to be directly used in the app

# These are the times I know it was recorded in micrometers:
# ET2b, between "2023-03-28 16:45:00" and "2023-05-04 16:30:00"
# TV4a, between "2023-01-10 17:45:00" and 2023-02-18 01:00:00"

library(needs)
needs(tidyverse, here, lubridate, readxl)

options(readr.show_col_types = FALSE)
source("Functions_dendro.R")

# Data and directories -----------------------------------

# Directory paths
data.raw.dir <- here("Dendro_data_raw")
data.support.dir <- here("Dendro_data_supporting")

baseline <- read_excel(file.path(data.support.dir, "Dendro_metadata.xlsx"))

import.log <- read_csv(file.path("Dendro_data_supporting",
                                 "Dendro_import_log.csv"),
                       show_col_types = F)

# Import data -------------------------------------------------------------

Log <- NULL
dendro.vec <- dendro.vec.full
i <- "ET1a"
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
    select(-Timelead)
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

x <- "ET6a"
find_dendro_DL <- function(x){
  TreeID = str_sub(x, start = 1, end = 3)

  filenames = list.files(here("Dendro_data_raw", TreeID), full.names = T)
  filenames2 = filenames[which(str_detect(filenames, x) == T)]

  d = data.frame(Dendro = x, filename = filenames2) %>%
    mutate(dateloc = str_locate(filename, "202")[,"start"],
           DL_date = str_sub(filename, start = dateloc, end = dateloc + 9)) %>%
    select(Dendro, DL_date)
  return(d)
}
Dendro_DL <- lapply(dendro.vec, find_dendro_DL) %>%
  bind_rows() %>%
  separate(Dendro, into = c("Tree", "Dendro"), 3)

write_csv(Dendro_DL, here("Dendro_data_supporting", "Dendro_DL.csv"))


# L1 to L2 ----------------------------------------------------------------

dendro.vec <- dendro.vec.full

i <- "ET4a"
for(i in dendro.vec){
  filename.in.L1 <- str_c("Dendro_data_L1/", i, "_Dendro_L1.csv")

  if(file.exists(filename.in.L1)){
    d <- read_csv(filename.in.L1, show_col_types = F)
  } else {
    next
    cat("No L1 file for", i, "\n")
  }

  d2 <- d %>%
    fix_jumps(10)

  write_csv(d2, file.path("Dendro_data_L2",
                      str_c(i, "_Dendro_L2.csv")))
}

x <- d
y <- 10
fix_jumps <- function(x, y){

  x2 = x

  x3 = x2 %>%
    mutate(lagRadius = lag(Radius),
           Growth = Radius - lagRadius)

  # The jump comes right before the indicated timestamp, so should be subtracted from all including the indicated
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
    mutate(Radius.OG = Radius,
           Radius = Radius - cumulativeJump) %>%
    mutate(jumpStart = ifelse(jumpStart == "yes", Radius, jumpStart)) %>%
    select(Dendrometer, Timestamp, Radius.OG, Radius, jumpStart)
  return(x4)
}


# L1 to L2a ----------------------------------------------------------------
needs(treenetproc)

dendro.vec <- dendro.vec.full

i <- "ET2b"
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
    filter(year(ts) == "2022") %>%
    filter(ts == max(ts, na.rm = T)) %>%
    pull(gro_yr)
  if(length(gro.yr.reset) == 0){
    gro.yr.reset = 0
  }

  d3 <- d2 %>%
    rename(Dendrometer = series, Timestamp = ts, Radius = value) %>%
    mutate(gro_yr = ifelse(Timestamp >= as_datetime("2023-01-01 00:00:00"),
                           gro_yr + gro.yr.reset, gro_yr))

  write_csv(d3, file.path("Dendro_data_L2a",
                          str_c(i, "_Dendro_L2a.csv")))
}


# L2 to L3 ----------------------------------------------------------------

d <- read_csv(file.path("Dendro_data_L2", "ET4a_Dendro_L2.csv"),
              guess_max = 10000)
problems(d)
# Extra processing --------------------------------------------------------

## Treenetproc -------------------------------------------------------------

# library(devtools)
# devtools::install_github("treenet/treenetproc")

d <- read_csv("Dendro_data_L1/ET6a_Dendro_L1.csv") %>%
  select(series = Dendrometer, ts = Timestamp, value = Radius)

# Basic processing functions
dendro_L1 <- proc_L1(data_L0 = d, reso = 15, input = "long")
dendro_L2 <- proc_dendro_L2(dendro_L1 = dendro_L1, plot = T, tol_jump = 10)

# Phase stats... pretty cool but not sure how it can be useful
# d <- read_csv(file.path("Dendro_data_L2a", "ET6a_Dendro_L2a.csv"),
#               guess_max = 10000)
test <- phase_stats(dendro_L2)

