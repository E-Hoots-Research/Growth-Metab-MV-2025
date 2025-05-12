#Script to calculate SMR, MMR and RMR from respirometry slopes pulled from Labchart
#Last edited 2023-Nov-14 BH simplified calcSMR function

rm(list=ls())

library(tidyverse) #loads ggplot2, dplyr, tidyr, tibble, stringr
library(glue)
library(mclust)
library(lme4)
library(car)
library(readxl)
library(lubridate)
library(hms)
library(modelr)

#calcSMR script shamelessly stolen and adapted from Eirik (who shamelessly stole it from Chabot, 2016)

calcSMR <- function(Y) {
  u <- sort(Y)
  tenpc <- round(0.1 * length(u))
  SD10pc <- sd(u[1:tenpc])
  #lower_threshold <- mean(u[1:tenpc]) - SD10pc
  low10pc = mean(u[(which((u > (mean(u[1:tenpc])-SD10pc)))):(tenpc+which((u > (mean(u[1:tenpc])-SD10pc -u[1]))))])
  #low10pc <- mean(u[u > lower_threshold & u <= mean(u[1:tenpc])])
  #low10pc <- mean(u[which(u > lower_threshold):u[which(u <= (u[tenpc]))]])
  #low10pc <- mean(u[2:(u[tenpc]+2)])
  #low10pc <- mean(u[1:tenpc])
  #filtered_mean <- mean(u[u >= low10pc - SD10pc & u <= low10pc])
  
  return(list(low10pc = low10pc))
}

################################# SMR Data #####################################

# Define a function to extract date strings from file names
extract_date <- function(files) {
  gsub(".*?([0-9]{8}).*", "\\1", basename(files))
}

# Specify the directories where your files are located
csv_folder <- "C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Raw Data/Respirometry Data/MR_Slopes"
xlsx_folder <- "C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Raw Data/Respirometry Data/MMR"

# Get the list of files in each folder
csv_files <- list.files(csv_folder, pattern = "_MR_slopes.csv", full.names = TRUE)
xlsx_files <- list.files(xlsx_folder, pattern = "_MMR.xlsx", full.names = TRUE)


# Extract date strings from file names
csv_dates <- extract_date(csv_files)
xlsx_dates <- extract_date(xlsx_files)


matching_files <- Map(function(date) {
  csv_file <- csv_files[grep(date, csv_dates)]
  xlsx_file <- xlsx_files[grep(date, xlsx_dates)]
  return(list(csv = csv_file, xlsx = xlsx_file))
}, unique(c(csv_dates, xlsx_dates)))


# Loop through matching files and perform your data processing
for (i in seq_along(matching_files)) {
  date <- unique(csv_dates)[i]
  current_files <- matching_files[[i]]  # Renamed 'files' to 'current_files'
  
  # Process CSV data
  tb_respirometry <- read_csv(current_files$csv) %>%
    rename(
      chamber_ch = Chamber.No,
      ID_fish    = Ind,
      mass       = Mass,
      length     = Length,
      volume_ch  = Ch.Volume,
      DOunit     = DO.unit,
      dateTime   = Date.Time,
      Temp_class = Temp.class,
      slope_wBR  = Slope.with.BR,
      BRSlope    = BR.Slope
    )  %>%
    mutate(
      dateTime       = as.POSIXct(ymd_hms(dateTime)),
      Time           = as_hms(ymd_hms(dateTime)),
      Date           = as.Date(dateTime, format = "%Y/%m/%d"),
    )
  
  
  tb_respirometry <- tb_respirometry %>%
    mutate(
      volume_net = volume_ch - mass,
      MR_wBR     = abs(slope_wBR)*(volume_net/1000)*60*60, #uncomment for mgO2/hr instead
      BR         = BRSlope*(volume_ch/1000)*60*60,
      MR         = MR_wBR + BR,
      BR_perc    = (BR/MR_wBR)*100
    ) 
  
  #Calculate RMR and variance in MR for each fish, create a new table for individual RMR calcs 
  
  tb_rmr <- tb_respirometry %>%
    group_by(chamber_ch) %>%
    arrange(chamber_ch, dateTime) %>%
    slice(3:(n() - 1)) %>%
    ungroup()  %>%
    group_by(
      ID_fish, mass, length, volume_net, chamber_ch, Temp_class
    ) %>% 
    arrange(ID_fish) %>%
    drop_na() %>%
    summarise(
      RMR        = mean(MR),
      RMR_var    = var(MR),
      RMR_perc   = (RMR_var/RMR)*100,
      time_start = dateTime %>% min(),
      time_end   = dateTime %>% max()
    ) 
  
  
  #Calculate SMR for each fish, create a new table for individual SMR calcs 
  
  tb_smr <-
    tb_respirometry %>%
    group_by(
      ID_fish, mass,length, volume_net, chamber_ch
    ) %>% 
    arrange(ID_fish) %>%
    drop_na() %>%
    summarise(
      SMR        = calcSMR(MR)$low10pc %>% unname(),
      time_start = dateTime %>% min(),
      time_end   = dateTime %>% max()
    ) 
  
    # Process XLSX data
    tb_mmr <- read_excel(current_files$xlsx) %>%
      rename(
        chamber_ch = Chamber.No,
        ID_fish    = Ind,
        mass       = Mass,
        volume_ch  = Ch.Volume,
        DOunit     = DO.unit,
        dateTime   = Date.Time,
        Temp_class = Temp.class,
        slope_wBR  = Slope.with.BR,
        BRSlope    = BR.Slope
      )  %>%
      mutate(
        dateTime       = as.POSIXct(ymd_hms(dateTime)),
        Time           = as_hms(ymd_hms(dateTime)),
        Date           = as.Date(dateTime, format = "%Y/%m/%d"),
      ) %>%
      drop_na()
    
    tb_mmr <- tb_mmr %>%
      mutate(
        volume_net = volume_ch - mass,
        MMR_wBR    = abs(slope_wBR)*(volume_net/1000)*60*60, #uncomment for mgO2/hr instead
        BR         = abs(BRSlope)*(volume_ch/1000)*60*60,
        MMR        = MMR_wBR - BR,
        BR_perc    = (BR/MMR_wBR)*100
      ) %>%
      arrange(ID_fish)
      
    # Combine the processed data
    # Combine the processed data, selecting only specific columns
    tb_MR_master <- tb_rmr %>%
      left_join(select(tb_smr, ID_fish, SMR), by = "ID_fish") %>%
      left_join(select(tb_mmr, ID_fish, MMR), by = "ID_fish") %>%
      select(-matches(".x$")) %>%
      rename_with(~gsub("\\.y$", "", .), matches(".y$"))
    
  # Export data
  setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Raw Data/Respirometry Data/MRcalcs")
  write.csv(tb_MR_master, file = paste0(date, "_MRcalcs.csv"), col.names = NA, row.names = FALSE)
}


