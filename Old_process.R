# Old stuff for dendrometer data

# Old process -------------------------------------------------------------

# First timers ------------------------------------------------------------

Tree.ID <- "TV2"
Dendro.ID <- str_c(Tree.ID, "_1")

files <- list.files("raw_data_dendro", full.names = T)
tree.files <- files[which(str_detect(files, Tree.ID) == T)]
latest.locator <- which.max(file.info(tree.files)$mtime)
filename <- tree.files[latest.locator]

baseline <- read_excel("Tomst_install.xlsx") %>% 
  filter(DendroID == Dendro.ID)
baseline2 <- as.POSIXct(as.character((baseline[1,4])), tz = "UTC")

d <- read_csv2(filename,   
               col_names = c(".id", "date_time", "Moisture", "T1", "T2", "T3", 
                             "Tomst", "X8", "X9", "X10")) %>%
  mutate(date_time = ymd_hm(date_time, tz = "UTC")) %>% 
  select(-X8, -X9, -X10)

d2 <- d %>%
  filter(date_time > (baseline2 + 60000))

d3 <- d2 %>% 
  # mutate(Radius = (Tomst-1278)*(8890/(34000-1278))) %>%
  mutate(Radius = Tomst) %>%
  mutate(.id = as.factor(Dendro.ID)) %>% 
  select(.id, date_time, Moisture, T1, T2, T3, Radius)

out <- str_c("processed_data_dendro/", Dendro.ID, ".csv")
write_csv(d3, out)

# Appending ---------------------------------------------------------------
Tree.ID <- "TV3"
Dendro.ID <- str_c(Tree.ID, "_1")
# Dendro.ID <- "FB3_B"

files <- list.files("raw_data_dendro", full.names = T)
tree.files <- files[which(str_detect(files, Tree.ID) == T)]
latest.locator <- which.max(file.info(tree.files)$mtime)
# filename <- tree.files[4]
filename <- tree.files[latest.locator]
# filename <- "raw_data_dendro/FB3A_2022_11_03_0.csv"

filename.in.processed <- str_c("processed_data_dendro/", Dendro.ID, ".csv")
data.needed.starting <- max(read_csv(filename.in.processed)$date_time) + 900

d <- read_csv2(filename,   
               col_names = c(".id", "date_time", "Moisture", "T1", "T2", "T3", 
                             "Tomst", "X8", "X9", "X10")) %>%
  mutate(date_time = ymd_hm(date_time, tz = "UTC")) %>% 
  select(-X8, -X9, -X10)
new.data.begins <- min(d$date_time)

# Check to make sure no data gap
data.needed.starting <= new.data.begins

d2 <- d %>%
  filter(date_time >= as.POSIXct(data.needed.starting, tz = "UTC"))

d3 <- d2 %>% 
  mutate(Radius = (Tomst-1278)*(8890/(34000-1278))) %>%
  # mutate(Radius = Tomst) %>%
  mutate(.id = as.factor(Dendro.ID)) %>% 
  select(.id, date_time, Moisture, T1, T2, T3, Radius)

out <- str_c("processed_data_dendro/", Dendro.ID, ".csv")
write_csv(d3, out, append = T)

# Graph it -----------------------------------------------------------

Dendro.ID <- "TV3"

files <- list.files("processed_data_dendro", full.names = T)
filename <- files[which(str_detect(files, Dendro.ID) == T)]
# 
d <- read_csv(filename)

plot_tomst <- function(x){
  ggplot(x) +
    geom_line(aes(x = date_time, y = Radius)) +
    theme_bw() +
    ggtitle(Dendro.ID)
}
plot_tomst(d)
p <- plot_tomst(d3)
ggplotly(p)









# PLOTeR -------------------------------------------------------------

library(needs)
needs(PLOTeR, tidyverse)

# First, try opening the Shiny app. Problem is can't load data bc file viewers have weird bugs
PLOTeR()

# Next, try to use the read_tomst() function to make a df in R that can then be viewed in the Shiny app. Doesn't seem to be able to read the raw data correctly  
filename1 <- "Dendro_data_raw/FB1/data_92221028_2023_01_12_0.csv"
df <- read_tomst(filename1, delim = ",", radius_units = "tomst")
PLOTeR()

# Finally, try to bypass read_tomst by formatting my own df in the same way
df <- read_csv("Dendro_data_LVL2/FB1_Dendro_LVL2.csv") %>% 
  rename("record" = ".id") %>% 
  mutate(.id = as.factor("FB1a")) %>%
  select(.id, everything(), -Tree, -Dendro) %>% 
  as.data.frame()
str(df)

PLOTeR()



# Attempt to use read_tomst


df <- read_tomst("raw_data_dendro/data_92221028_2022_10_13_0.csv", radius_units = "um")
PLOTeR()

