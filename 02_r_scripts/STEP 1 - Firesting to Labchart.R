#Script to prepare text files for Labchart
#Last edited 2023-Dec-14 by Beth Hoots; changed WDs to new homes. Considering autopulling text files, but I still like having this part manual for now

rm(list=ls())
#setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Raw Data/Firesting Text Files") 
setwd("//s222141733.homes.deakin.edu.au/my-home/UserData/Desktop/Growth Performance Pilot/Files for Labchart/Respirometry Text Files")

library(tidyverse) #loads ggplot2, dplyr, tidyr, tibble, stringr
library(car)
library(readxl)
library(lubridate)
library(hms)

#load in the four raw firesting files
A <- read.table("20230318_MMR_SMR_A.txt", skip = 19, header = T,  sep = "\t")
B <- read.table("20230318_MMR_SMR_B.txt", skip = 19, header = T, sep = "\t")
C <- read.table("20230318_MMR_SMR_C.txt", skip = 19, header = T, sep = "\t")
D <- read.table("20230318_MMR_SMR_D.txt", skip = 19, header = T, sep = "\t")

A_raw <- tibble(A[,c(1:9)]) %>%
  rename(
    Time  = Time..HH.MM.SS.,
    A1    = Ch1,
    A2    = Ch2,
    A3    = Ch3, 
    A4    = Ch4, 
    TempA = Ch.1
  ) %>%
  mutate(
    Time..s. = NULL,
    Comment  = NULL,    
    Date_dup = as.Date(Date, format = "%d/%m/%Y"),
    Time     = as.POSIXct(Time, format = "%H:%M:%S"),
    Time_dup = round_date(Time, unit = "2 seconds")
  ) %>% separate(
    Time_dup, sep = " ", into = c("delete", "Time_dup")
  ) %>%
  mutate(
    delete = NULL
  ) %>%
  unite(
    Date_dup, Time_dup, sep = " ", col = dateTime
  ) 
  
B_raw <- tibble(B[,c(1:9)]) %>%
  rename(
    Time  = Time..HH.MM.SS.,
    B1    = Ch1,
    B2    = Ch2,
    B3    = Ch3, 
    B4    = Ch4, 
    TempB = Ch.1
  ) %>%
  mutate(
    Time..s. = NULL,
    Comment  = NULL,    
    Date_dup = as.Date(Date, format = "%d/%m/%Y"),
    Time     = as.POSIXct(Time, format = "%H:%M:%S"),
    Time_dup = round_date(Time, unit = "2 seconds")
  ) %>% separate(
    Time_dup, sep = " ", into = c("delete", "Time_dup")
  ) %>%
  mutate(
    delete = NULL
  ) %>%
  unite(
    Date_dup, Time_dup, sep = " ", col = dateTime
  )

C_raw <- tibble(C[,c(1:9)]) %>%
  rename(
    Time  = Time..HH.MM.SS.,
    C1    = Ch1,
    C2    = Ch2,
    C3    = Ch3, 
    C4    = Ch4, 
    TempC = Ch.1
  ) %>%
  mutate(
    Time..s. = NULL,
    Comment  = NULL,    
    Date_dup = as.Date(Date, format = "%d/%m/%Y"),
    Time     = as.POSIXct(Time, format = "%H:%M:%S"),
    Time_dup = round_date(Time, unit = "2 seconds")
  ) %>% separate(
    Time_dup, sep = " ", into = c("delete", "Time_dup")
  ) %>%
  mutate(
    delete = NULL
  ) %>%
  unite(
    Date_dup, Time_dup, sep = " ", col = dateTime
  )

D_raw <- tibble(D[,c(1:9)]) %>%
  rename(
    Time  = Time..HH.MM.SS.,
    D1    = Ch1,
    D2    = Ch2,
    D3    = Ch3, 
    D4    = Ch4, 
    TempD = Ch.1
  ) %>%
  mutate(
    Time..s. = NULL,
    Comment  = NULL,    
    Date_dup = as.Date(Date, format = "%d/%m/%Y"),
    Time     = as.POSIXct(Time, format = "%H:%M:%S"),
    Time_dup = round_date(Time, unit = "2 seconds")
  ) %>% separate(
    Time_dup, sep = " ", into = c("delete", "Time_dup")
  ) %>%
  mutate(
    delete = NULL
  ) %>%
  unite(
    Date_dup, Time_dup, sep = " ", col = dateTime
  )

tb_resp_raw <- A_raw %>%
  left_join(
    B_raw, by = "dateTime"
  ) %>%
  left_join(
    C_raw, by = "dateTime"
  ) %>% 
  left_join(
    D_raw, by = "dateTime"
  ) %>%
  drop_na() %>%
  separate(
    dateTime, sep = " ", into = c("Date", "Time")
  ) %>%
  mutate(
    Date.x   = NULL,
    Time.x   = NULL,
    Date.y   = NULL,
    Time.y   = NULL,
    Date.x.x = NULL,
    Time.x.x = NULL,
    Date.y.y = NULL,
    Time.y.y = NULL
  )  %>%
  relocate(
    Time, .before = A1
  ) %>%
  relocate(
    Date, .before = Time
  ) %>%
  relocate(
    TempA, .after = Time
  ) %>%
  relocate(
    TempB, .after = TempA
  ) %>%
  relocate(
    TempC, .after = TempB
  ) %>%
  relocate(
    TempD, .after = TempC
  ) 

tb_resp_raw$Date <- gsub("-", ".", tb_resp_raw$Date)
tb_resp_raw$Date <- gsub("^[^.]*.", "", tb_resp_raw$Date)
tb_resp_raw$Time <- gsub(":", ".", tb_resp_raw$Time)

#setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Raw Data/Firesting Ready for Labchart") 
setwd("//s222141733.homes.deakin.edu.au/my-home/UserData/Desktop/Growth Performance Pilot/Files for Labchart/Jan 2024")
write.table(tb_resp_raw, file = "20230318_MMR_SMR_All.txt", sep = "\t", quote = F, row.names = FALSE)
