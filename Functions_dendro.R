
# Constants ---------------------------------------------------------------

# Define a vector of DendroIDs
dendro.vec.full <- c(
  "ET1a", "ET2a", "ET2b", "ET3a", "ET4a", "ET4b", "ET5a", "ET5b", "ET6a",
  "ET7a", "ET8a", "FB1a", "FB2a", "FB3a", "FB3b", "FB4a", "FB4b",
  "FB5a", "FB5b", "FB6a", "FB6b", "FB7a", "FB7b", "FB8a",
  "TV1a", "TV2a", "TV3a", "TV4a")



# Functions ---------------------------------------------------------------

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

