# Script to format Labchart outputs for use in RespScript
# SMR Files from excel file where you have copied/pasted in order outputs from labchart: Initial BResp, End BResp, SMR traces
# Last edited 2023-Dec-17 by Beth Hoots to add the looping function

rm(list=ls())

library(tidyverse)
library(car)
library(readxl)
library(lubridate)
library(hms)

# Function to process an SMR file
process_SMR_file <- function(file_path, fish_data_path) {
  tb_Labchart <- read_excel(file_path)
  tb_Labchart <- tb_Labchart[-c(1:2),]
  
  # reformat an SMR file
  tb_Labchart$Date <- round(as.numeric(tb_Labchart$Date), digits = 2)
  tb_Labchart$Date <- lapply(format(round(as.numeric(tb_Labchart$Date), digits = 2), nsmall=2), gsub, pattern ="[.]", replacement = "/")
  
  tb_Labchart$year <- "2023/"
  tb_Labchart$Date <- unite(tb_Labchart, Date, year, "Date", sep="")
  tb_Labchart$year <- NULL
  tb_Labchart$Date <- as.Date(tb_Labchart$Date$Date, format = "%Y/%m/%d") #change back for R-prepped files to "%Y/%m/%d"
  
  tb_Labchart$Time <- round(as.numeric(tb_Labchart$Time), digits = 2)
  tb_Labchart$Time <- lapply(tb_Labchart$Time, gsub, pattern = "[.]", replacement = ":")
  tb_Labchart$Time <- strptime(tb_Labchart$Time, "%H:%M")
  tb_Labchart$Time <- as_hms(tb_Labchart$Time)
  
  tb_Labchart <- tb_Labchart %>% 
    add_column(
      "Date.Time"=NA, .after = "Time"
    ) %>%
    unite(
      Date, Time, sep = " ", col = "Date.Time"
    ) %>%
    mutate(
      Date.Time = ymd_hms(Date.Time)
    )
  
  tb_mess <- tb_Labchart %>%
    pivot_longer(
      cols = c(6:21), names_to = "Chamber.No", values_to = "Slope.with.BR"
    ) %>%
    add_column(
      "Temp" = NA,
      "BR.Slope" = NA
    )
  
  # Create a mapping of chamber numbers to temperature column names
  chamber_temp_mapping <- c(
    A1 = "TempA", A2 = "TempA", A3 = "TempA", A4 = "TempA",
    B1 = "TempB", B2 = "TempB", B3 = "TempB", B4 = "TempB",
    C1 = "TempC", C2 = "TempC", C3 = "TempC", C4 = "TempC",
    D1 = "TempD", D2 = "TempD", D3 = "TempD", D4 = "TempD"
  )
  
  # Loop through rows and assign the correct temperature value
  for (i in 1:nrow(tb_mess)) {
    chamber <- tb_mess$Chamber.No[i]
    temp_column <- chamber_temp_mapping[chamber]
    tb_mess$Temp[i] <- tb_mess[[temp_column]][i]
  }
  
  # Create a mapping of chamber numbers to background slope column names
  chamber_br_mapping <- c(
    A1 = "A1_BR", A2 = "A2_BR", A3 = "A3_BR", A4 = "A4_BR",
    B1 = "B1_BR", B2 = "B2_BR", B3 = "B3_BR", B4 = "B4_BR",
    C1 = "C1_BR", C2 = "C2_BR", C3 = "C3_BR", C4 = "C4_BR",
    D1 = "D1_BR", D2 = "D2_BR", D3 = "D3_BR", D4 = "D4_BR"
  )

  # Loop through rows and assign the correct background slope value
  for (i in 1:nrow(tb_mess)) {
    chamber <- tb_mess$Chamber.No[i]
    br_column <- chamber_br_mapping[chamber]
    tb_mess$BR.Slope[i] <- tb_mess[[br_column]][i]
  }
  
  tb_output <- drop_na(tb_mess)
  
  return(tb_output)
}

# Define paths
smr_folder <- "C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Raw Data/Respirometry Data/SMR"
fish_data_folder <- "C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Raw Data/FishData"
mr_output_folder <- "C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Raw Data/Respirometry Data/MR_Slopes"

# Loop through SMR files
SMR_files <- list.files(path = smr_folder, pattern = "_SMR\\.xlsx", full.names = TRUE)
for (file_path in SMR_files) {
  # Extract the filename from the path
  file_name <- basename(file_path)
  
  # Extract the date from the filename without including "_SMR"
  date_str <- str_extract(file_name, "(\\d{8})")  # Extract 8 digits as the date
  
  # Check if a match is found
  if (!is.na(date_str)) {
    fish_data_path <- file.path(fish_data_folder, paste0(date_str, "_FishData.xlsx"))
    
    # Check if the FishData file exists
    if (file.exists(fish_data_path)) {
      tb_output <- process_SMR_file(file_path, fish_data_path)
      
      # Load and merge corresponding FishData file
      tb_FishData <- read_excel(fish_data_path)
      
      tb_output <- left_join(tb_output, tb_FishData, by = c("Chamber.No" = "Chamber.No"))
      
      tb_output <- tibble(
        "Chamber.No" = tb_output$Chamber.No, 
        "Ind" = tb_output$Ind, 
        "Mass" = tb_output$Mass, 
        "Length"= tb_output$Length, 
        "Ch.Volume"= tb_output$Ch.Volume, 
        "DO.unit"= "mg O2", 
        "Temp.class"= tb_output$Temp.class, 
        "Date.Time"= tb_output$Date.Time,
        "Temp"= as.numeric(tb_output$Temp), 
        "Slope.with.BR"= as.numeric(tb_output$Slope.with.BR),
        "BR.Slope" = tb_output$BR.Slope
      ) %>% drop_na()
      
      # Write merged output to MR_Output folder
      mr_output_path <- file.path(mr_output_folder, paste0(date_str, "_MR_slopes.csv"))
      write.csv(tb_output, file = mr_output_path, row.names = FALSE)
    } else {
      cat("FishData file not found for date:", date_str, "\n")
    }
  } else {
    cat("Date not found in filename.\n")
  }
}

