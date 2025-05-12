rm(list=ls())

#load in required packages
library(tidyverse)
library(modelr)
library(lubridate)
library(hms)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(dplyr)
library(cowplot)
library(ggsignif)
library(wesanderson)
library(lsmeans)
library(emmeans)
library(lme4)
library(multcomp)
library(multcompView)

#don't forget to set the appropriate working directory
setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/Chapter 2 (2023 Growth Performance)/Analysis/Raw Data/Respirometry Data/MRcalcs")

#read all "_MRcalcs.csv" files, bind into one table, and add columns with logged values 
file.list <- list.files()
calcs_all <- lapply(file.list, read_csv)
tb_MRcalcs <- bind_rows(calcs_all, .id="Resp_Day") %>%
  mutate(
    log_SMR_low10pc = log10(SMR),
    log_mass = log10(mass),
    log_RMR = log10(RMR),
    log_MMR = log10(MMR),
    Resp_Day = as.numeric(Resp_Day)
  )


tb_MRcalcs$Month <- cut(
  tb_MRcalcs$Resp_Day,
  breaks = c(0, 13, seq(23, max(tb_MRcalcs$Resp_Day) + 10, by = 10)),
  right = FALSE,
  labels = c("April", "May", "June", "July", "August", "September")
)

tb_MRcalcs <- tb_MRcalcs[which(tb_MRcalcs$Month != "April"),]

#creating a linear regression between SMR and mass for both temperature treatments 
lm_smr_18 = lm(log_SMR_low10pc ~ log_mass, tb_MRcalcs[which(tb_MRcalcs$Temp_class == 18),])
lm_smr_23 = lm(log_SMR_low10pc ~ log_mass, tb_MRcalcs[which(tb_MRcalcs$Temp_class == 23),])

summary(lm_smr_18)
summary(lm_smr_23)

log_mass_grandmean = data.frame(log_mass = log10(mean(tb_MRcalcs$mass))) #pulling the mean mass value of fish in 18C treatment, naming it "log_mass_18"

#predicting the SMR value for mass using our regression
SMR_mean_predicted_18 = predict(lm_smr_18, newdata = log_mass_grandmean) 
SMR_mean_predicted_23 = predict(lm_smr_23, newdata = log_mass_grandmean) 

#adding the residuals calculated from our regressions to our table
tb_MRcalcs <- tb_MRcalcs %>% mutate(resid = NA)

tb_MRcalcs[which(tb_MRcalcs$Temp_class == 18),] <- tb_MRcalcs[which(tb_MRcalcs$Temp_class == 18),] %>%
  mutate(resid = residuals(lm_smr_18))

tb_MRcalcs[which(tb_MRcalcs$Temp_class == 23),] <- tb_MRcalcs[which(tb_MRcalcs$Temp_class == 23),] %>%
  mutate(resid = residuals(lm_smr_23))

tb_MRcalcs <- tb_MRcalcs %>% mutate(log_SMR_mass = NA)

tb_MRcalcs[which(tb_MRcalcs$Temp_class == 18),]$log_SMR_mass <-
  SMR_mean_predicted_18 + tb_MRcalcs[which(tb_MRcalcs$Temp_class == 18),]$resid

tb_MRcalcs[which(tb_MRcalcs$Temp_class == 23),]$log_SMR_mass <-
  SMR_mean_predicted_23 + tb_MRcalcs[which(tb_MRcalcs$Temp_class == 23),]$resid


#Adding new columns with mass standardized SMR calculations
tb_MRcalcs <- 
  tb_MRcalcs %>%
  mutate(
    log_SMR_mass = ifelse(Temp_class == 18, SMR_mean_predicted_18 + resid, SMR_mean_predicted_23 + resid), #use the 2 models to calc residuals + predicted values for each FishID/time
    SMR_mass = as.numeric(10^log_SMR_mass),
    SMR_resid = resid,
    log_SMR_mlnd = NULL,
    log_SMR_low10pc = NULL,
    log_SMR_mass = NULL
  ) 

#creating a linear regression between RMR and mass for both temperature treatments 
lm_rmr_18 = lm(log_RMR ~log_mass, tb_MRcalcs[which(tb_MRcalcs$Temp_class == 18 & tb_MRcalcs$Resp_Day > 12),]) 
lm_rmr_23 = lm(log_RMR ~log_mass, tb_MRcalcs[which(tb_MRcalcs$Temp_class == 23),])

summary(lm_rmr_18)
summary(lm_rmr_23)

#predicting the RMR value for mass using our regression
RMR_mean_predicted_18 = predict(lm_rmr_18, newdata = log_mass_grandmean) 
RMR_mean_predicted_23 = predict(lm_rmr_23, newdata = log_mass_grandmean) 

#adding the residuals calculated from our regressions to our table
tb_MRcalcs <- tb_MRcalcs %>%
  mutate(resid = ifelse(Temp_class == 18, residuals(lm_rmr_18), residuals(lm_rmr_23)))

#Adding new columns with mass standardized RMR calculations
tb_MRcalcs <- tb_MRcalcs %>%
  mutate(
    log_RMR_mass = ifelse(Temp_class == 18, RMR_mean_predicted_18 + resid, RMR_mean_predicted_23 + resid), #use the 2 models to calc residuals + predicted values for each FishID/time
    RMR_mass = as.numeric(10^log_RMR_mass),
    RMR_resid = resid, 
    log_RMR = NULL,
    log_RMR_mass = NULL
  )

#creating a linear regression between MMR and mass for both temperature treatments
lm_mmr_18 = lm(log_MMR ~ log_mass, tb_MRcalcs[which(tb_MRcalcs$Temp_class == 18 & tb_MRcalcs$Resp_Day > 12),]) 
lm_mmr_23 = lm(log_MMR ~ log_mass, tb_MRcalcs[which(tb_MRcalcs$Temp_class == 23),]) 

#predicting the MMR value for "mass" using our regression
MMR_mean_predicted_18 = predict(lm_mmr_18, newdata = log_mass_grandmean) 
MMR_mean_predicted_23 = predict(lm_mmr_23, newdata = log_mass_grandmean) 

#adding the residuals calculated from our regressions to our table
tb_MRcalcs <- tb_MRcalcs %>%
  mutate(resid = ifelse(Temp_class == 18, residuals(lm_mmr_18), residuals(lm_mmr_23)))

#Adding new columns with mass standardized MMR calculations
tb_MRcalcs <- 
  tb_MRcalcs %>%
  mutate(
    log_MMR_mass = ifelse(Temp_class == 18, MMR_mean_predicted_18 + resid, MMR_mean_predicted_23 + resid), #use the 2 models to calc residuals + predicted values for each FishID/time,
    MMR_mass = as.numeric(10^log_MMR_mass),
    MMR_resid = resid,
    log_MMR_mass = NULL,
    log_MMR = NULL,
  ) %>%
  drop_na() %>%
  arrange(Resp_Day, ID_fish)

# Create the grouped variable
tb_MRcalcs$Month <- cut(
  tb_MRcalcs$Resp_Day,
  breaks = c(0, 13, seq(23, max(tb_MRcalcs$Resp_Day) + 10, by = 10)),
  right = FALSE,
  labels = c("April", "May", "June", "July", "August", "September")
)

setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/Chapter 2 (2023 Growth Performance)/Analysis/Figures")
write.csv(tb_MRcalcs, file = "MRcalcs.csv", col.names = TRUE, row.names = FALSE) #Change the name

############################# MR Diagnostic Plots #################################

setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/Chapter 2 (2023 Growth Performance)/Analysis/Raw Data/Respirometry Data/MR_Slopes")

file.listSMR <- list.files()
SMR_all <- lapply(file.listSMR, read_csv)
tb_SMRall <- bind_rows(SMR_all, .id="Resp_Day") %>%
  arrange(Resp_Day, Ind, Date.Time)

setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/Chapter 2 (2023 Growth Performance)/Analysis/Raw Data/Respirometry Data/MMR")

file.listMMR <- list.files()
MMR_all <- lapply(file.listMMR, read_excel)
tb_MMRall <- bind_rows(MMR_all, .id="Resp_Day") %>%
  drop_na()%>%
  left_join(tb_SMRall %>% dplyr::select(Ind, Resp_Day, Length), by = c("Ind", "Resp_Day")) %>%
  distinct() %>%
  arrange(Resp_Day, Ind, Date.Time)

tb_MRall <- bind_rows(tb_SMRall, tb_MMRall)

tb_MRall <- tb_MRall %>%
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
    Resp_Day       = as.numeric(Resp_Day)
      ) %>%
  mutate(
    volume_net = volume_ch - mass,
    MR_wBR     = abs(slope_wBR)*(volume_net/1000)*60*60, 
    BR         = BRSlope*(volume_ch/1000)*60*60,
    MR         = MR_wBR + BR,
    BR_perc    = (BR/MR_wBR)*100
  ) %>%
  drop_na() %>%
  arrange(Resp_Day, ID_fish, dateTime)

tb_MRall <- left_join(tb_MRall, tb_MRcalcs %>% dplyr::select(Resp_Day, ID_fish, time_start, time_end, length, SMR, RMR, MMR, SMR_mass, RMR_mass, MMR_mass, SMR_resid, RMR_resid, MMR_resid), by = c("ID_fish", "Resp_Day")) #a table merging individual MO2 slopes with calculated MR values

setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/Chapter 2 (2023 Growth Performance)/Analysis/Raw Data")

tb_final_samp <- read.csv("FinalBiometrics.csv")

tb_kitchensink <- left_join(tb_MRcalcs, tb_final_samp, by = c("ID_fish")) #a table with all resp data and metadata collected in dissection

tb_kitchensink$Month <- cut(
  tb_kitchensink$Resp_Day,
  breaks = c(0, 13, seq(23, max(tb_kitchensink$Resp_Day) + 10, by = 10)),
  right = FALSE,
  labels = c("April", "May", "June", "July", "August", "September")
)

setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/Chapter 2 (2023 Growth Performance)/Analysis/Figures")
write.csv(tb_kitchensink[which(tb_kitchensink$Month != "April"),], file = "KitchenSink.csv", col.names = TRUE, row.names = FALSE) #Change the name

write.csv(tb_kitchensink[which(tb_kitchensink$Month != "April" & !is.na(tb_kitchensink$Final_Mass_g) & tb_kitchensink$ID_fish != 122),], file = "KitchenSink_Survivors.csv", col.names = TRUE, row.names = FALSE)
##################################################################################

library(lubridate)
library(hms)
library(dplyr)
library(lme4)


#don't forget to set the appropriate working directory
setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/Chapter 2 (2023 Growth Performance)/Analysis/Figures")

data <- read.csv("MRcalcs.csv") %>%
  mutate(
    time_start = as.Date(time_start)
  )

data <- data %>%
  arrange(ID_fish, time_start)

# Calculate mass_prev
data <- data %>%
  group_by(ID_fish) %>%
  mutate(mass_prev = lag(mass)) %>% 
  mutate(length_prev = lag(length)) %>%
  ungroup()

# Calculate SGR between each measurement
sgr_calculations <- data %>%
  group_by(ID_fish) %>%
  mutate(SGR = (log(mass) - lag(log(mass))) / as.numeric(difftime(time_start, lag(time_start), units = "days")) * 100,
         Month = Month,
         Temp_class = Temp_class,
         mass = mass,
         length = length,
         Period_start = lag(time_start),
         Period_end = time_start,
         SMR_prev = lag(SMR),
         SMR = SMR,
         SMR_mass_prev = lag(SMR_mass),
         SMR_mass = SMR_mass,
         RMR_prev = lag(RMR),
         RMR = RMR,
         RMR_mass_prev = lag(RMR_mass),
         RMR_mass = RMR_mass,
         MMR_prev = lag(MMR),
         MMR = MMR,
         MMR_mass = MMR_mass,
         MMR_mass_prev = lag(MMR_mass)) %>%
  filter(!is.na(SGR))

# Calculate overall SGR for each fish
overall_sgr_calculations <- data %>%
  group_by(ID_fish) %>%
  summarize(Overall_SGR = (log(last(mass)) - log(first(mass))) / as.numeric(difftime(last(time_start), first(time_start), units = "days")) * 100)
# Combine SGR calculations into one table
tb_SGRcalcs <- sgr_calculations %>% dplyr::select(Resp_Day, ID_fish, Temp_class, Month, Period_start, Period_end, mass, mass_prev, length, length_prev, SGR, SMR_prev, SMR, SMR_mass_prev, SMR_mass, RMR_prev, RMR, RMR_mass_prev, RMR_mass, MMR_prev, MMR, MMR_mass_prev, MMR_mass) %>%
  left_join(overall_sgr_calculations, by = "ID_fish")


mod_SGR <- lmer(SGR ~ log(mass_prev) + (1|ID_fish), data = tb_SGRcalcs)
summary(mod_SGR)

tb_SGRcalcs$SGR_predict <- predict(mod_SGR, type = "response", allow.new.levels = TRUE)
tb_SGRcalcs$SGR_resid <- resid(mod_SGR)


write.csv(tb_SGRcalcs, file = "SGRcalcs.csv", col.names = TRUE, row.names = FALSE) 


##################################################################################

tb_master <- tb_kitchensink %>%
  left_join(tb_SGRcalcs %>% dplyr::select(Resp_Day, ID_fish, mass_prev, SGR, Period_start, Period_end, SMR_prev, SMR_mass_prev, RMR_prev, RMR_mass_prev, MMR_prev, MMR_mass_prev, Overall_SGR, SGR_predict, SGR_resid), by = c("Resp_Day", "ID_fish")) 

tb_new <- tb_master %>%
  mutate(
    day = as_date(round_date(time_start, unit = "day")),  # Ensure `day` is a Date object
    init = ymd("2023-05-17"),  # `init` as a Date object
    time = as.numeric(difftime(day, init, units = "days"))  # Compute difference in days
  ) %>%
  mutate(
    day = NULL,
    init = NULL,
    Month_int = case_when(
      Month == "April"   ~ 0,
      Month == "May"     ~ 1,
      Month == "June"    ~ 2,
      Month == "July"    ~ 3,
      Month == "August"  ~ 4,
      Month == "September" ~ 5 )
  )

setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/Chapter 2 (2023 Growth Performance)/Analysis/Metadata/Otoliths and biometrics")
gutratio <- read.csv("GutRatio.csv") %>% 
  dplyr::select(ID_fish, Ratio, stage)
tb_gutratio <- pivot_wider(gutratio, names_from = "stage", values_from = "Ratio")

tb_master <- left_join(tb_new, tb_gutratio, by = "ID_fish") %>%
  rename(
    "GutRatio_1" = a,
    "GutRatio_2" = b
  )

setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/Chapter 3")
write.csv(tb_master, file = "mastertable.csv", col.names = TRUE, row.names = FALSE) 

BinCounts <- read_excel("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/Chapter 2 (2023 Growth Performance)/Analysis/Metadata/BinCounts.xlsx", 
                        sheet = "data")

tb_master <- tb_master %>%
  mutate(
    Rack = Rack,
    Bin = Bin,
    Location = as.factor(paste(Rack, Bin, sep = "."))
  ) %>%
  left_join(BinCounts %>% dplyr::select(Month, Rack, Bin, Population_tagged, Population), by = c("Month", "Rack", "Bin")) %>%
  group_by(Rack, Bin) %>%
  mutate(
    Density_diff = Population - lag(Population)
  ) %>% 
  rename(
    Density = Population,
    Density_tagged = Population_tagged
  ) %>%
  ungroup() 


write.csv(tb_master, file = "workingtable.csv", col.names = T, row.names = F)

#Run until here before Step 5

##################################################################################

ggplot(data = tb_master) +
  geom_hline(yintercept = 0.74, linetype = "dashed", color = "black") +
  geom_boxplot(aes(x = as.numeric(factor(Temp_class)) - 0.2, y = GutRatio_1, fill = factor(Temp_class), group = factor(Temp_class)), 
               width = 0.3, position = position_dodge(width = 0.5)) +
  geom_boxplot(aes(x = as.numeric(factor(Temp_class)) + 0.2, y = GutRatio_2, fill = factor(Temp_class), group = factor(Temp_class)), 
               width = 0.3, position = position_dodge(width = 0.5)) +
  scale_x_continuous(breaks = 1:length(unique(tb_master$Temp_class)), labels = unique(tb_master$Temp_class)) +
  scale_fill_manual(values = c("#78B7C5", "#F21A00")) +
  theme_bw() +
  labs(x = "Temperature Treatment", y = "Gut:Width", fill = "Temperature")


##################################################################################

setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Figures")

#Length vs. mass plot
a <- ggplot(data = tb_MRall) +
  geom_point(aes(x = Length, y = mass)) +
  labs(x = "Length (mm)", y = "Mass (g)") +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, vjust=0, hjust=0))
 
ggsave(a, filename = "Lengthmass.png", bg = "transparent", width = 8.5, height = 11, units = "in")

#Length vs. mass by sex plot
b<- ggplot(data = tb_kitchensink[which(tb_kitchensink$Sex == "M" | tb_kitchensink$Sex == "F" | tb_kitchensink$Sex == "I"),]) +
  geom_point(aes(x = Length, y = mass, colour = Sex)) +
  geom_smooth(aes(x = Length, y = mass, color = Sex), method = "lm") +
  labs(x = "Length (mm)", y = "Mass (g)") +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, vjust=0, hjust=0))

ggsave(b, filename = "Lengthmass_bySex.png", bg = "transparent", width = 8.5, height = 11, units = "in")

#Length vs. mass by M/F sex plot
c<- ggplot(data = tb_kitchensink[which(tb_kitchensink$Sex == "M" | tb_kitchensink$Sex == "F"),]) +
  geom_point(aes(x = Length, y = mass, colour = Sex)) +
  geom_smooth(aes(x = Length, y = mass, color = Sex), method = "lm") +
  labs(x = "Length (mm)", y = "Mass (g)") +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, vjust=0, hjust=0))

ggsave(c, filename = "Lengthmass_byMatSex.png", bg = "transparent", width = 8.5, height = 11, units = "in")

##################################################################################

#MR of each RespDay faceted by mass and fishID in descending order

# Function to generate ggplot for a specific Resp_Day value
generate_plot <- function(data, resp_day) {
  ggplot(data = data[data$Resp_Day == resp_day,]) +
    geom_point(aes(dateTime, MR_wBR, colour="Metabolic rate with background")) +
    geom_point(aes(dateTime, MR, colour="Metabolic Rate")) +
    geom_hline(aes(yintercept = SMR, color = "Low 10%")) +
    facet_wrap(~factor(mass) + factor(ID_fish), scales = "free_x", labeller = label_parsed) +
    labs(x = "Time", y = "Metabolic Rate (mgO2/hr)", title = paste("Background comparison, RespDay", resp_day)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0))
}

setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Figures/Daily Plots Grid")

# Generate and save plots for all Resp_Day values
resp_days <- unique(tb_MRall$Resp_Day)
for (resp_day in resp_days) {
  gg_plot <- generate_plot(tb_MRall, resp_day)
  filename <- paste("RespDay", resp_day, ".png", sep = "")
  ggsave(gg_plot, filename = filename, bg = "transparent", width = 8.5, height = 11, units = "in")
}

##################################################################################

#MR of each fishID over all 6 RespDays

setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Figures/All Fish Plots")

# Filter unique ID_fish values
unique_fish <- unique(tb_MRall$ID_fish)

# Create a list to store plots
plots <- list()

for (fish_id in unique_fish) {
  fish_data <- tb_MRall %>%
    filter(ID_fish == fish_id) %>%
    arrange(dateTime)
  
  # Calculate the lowest 10% of MR values for each Resp_Day
  fish_data <- fish_data %>%
    group_by(Resp_Day) %>%
    mutate(
      mean_lowest_10_percent_MR = as.numeric(mean(MR[which(MR <= quantile(MR, 0.10))], na.rm = TRUE)),
      low10_MR = as.numeric(quantile(MR, 0.10, na.rm = TRUE)),
      sd_lowest_10_percent_MR = sd(MR[which(MR <= quantile(MR, 0.10))], na.rm = TRUE),
      xmin = dateTime[1],
      xmax = dateTime[n()],
      ymin = as.numeric(low10_MR - sd_lowest_10_percent_MR),
      ymax = as.numeric(low10_MR + sd_lowest_10_percent_MR)
    )
  
  plot <- ggplot(data = fish_data) + #[which((fish_data$MR <= mean(fish_data$MR) + 5 * sd(fish_data$MR))),]
      geom_point(aes(x = dateTime, y = MR, color = "MO2 (mg O2/hr)")) +
      geom_hline(aes(yintercept = SMR, color = "SMR (mg O2/hr)")) +
      geom_hline(aes(yintercept = MMR, color = "MMR (mg O2/hr)")) +
      facet_grid(cols = vars(Resp_Day),  scales = "free_x") +
      labs(x = "Time", y = "Metabolic Rate (mgO2/hr)") +
      theme_classic() +
      theme(legend.position = "bottom") +
      theme(axis.text.x = element_text(angle = 90, vjust=0, hjust=0)) +
      
      ggtitle(paste("Fish ID:", fish_id)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      
      # Annotate with mass values
      geom_text(aes(x = as.POSIXct(Inf), y = -Inf, label = paste("Mass:", mass, "g")),
                hjust = 1, vjust = 0, size = 3, color = "black", show.legend = FALSE) +
      
    # Add shading for the lowest 10% of MR values
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "blue", alpha = 0.005, linetype = 0, color = "blue") #+
    

  plots[[as.character(fish_id)]] <- plot
}

# Print and save the plots
for (fish_id in unique_fish) {
  plot <- plots[[as.character(fish_id)]]
  print(plot)
  ggsave(paste("plot_fish_", fish_id, ".png", sep = ""), plot, width = 16, height = 6, units = "in")
}

################################################################################

#MR of each RespDay faceted by mass and fishID in descending order (in a horizontal row)

setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Figures/Daily Plots")

tb_MRall <- tb_MRall %>%
  mutate(
    massID = paste(mass, ID_fish, sep = "g, ID:")
  )

# Filter unique ID_fish values
unique_day <- unique(tb_MRall$Resp_Day)

# Create a list to store plots
plots <- list()

for (day in unique_day) {
  fish_data <- tb_MRall %>%
    filter(Resp_Day == day) %>%
    arrange(mass)
 
  plot <- ggplot(data = fish_data) +
    geom_point(aes(x = dateTime, y = MR, color = "MO2 (mg O2/hr)")) +
    geom_hline(aes(yintercept = SMR, color = "SMR (mg O2/hr)")) +
    geom_hline(aes(yintercept = MMR, color = "MMR (mg O2/hr)")) +
    facet_grid(cols = vars(massID),  scales = "free_x") +
    labs(x = "Time", y = "Metabolic Rate (mgO2/hr)") +
    theme_classic() +
    theme(legend.position = "bottom") +
    theme(axis.text.x = element_text(angle = 90, vjust=0, hjust=0)) +
    
    ggtitle(paste("Resp day:", day)) +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  plots[[as.character(day)]] <- plot
}

# Print and save the plots
for (day in unique_day) {
  plot <- plots[[as.character(day)]]
  print(plot)
  ggsave(paste("plot_day_", day, ".png", sep = ""), plot, width = 16, height = 6, units = "in")
}

################################################################################

setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Figures")

# Create the grouped variable
tb_MRcalcs$Month <- cut(
  tb_MRcalcs$Resp_Day,
  breaks = c(0, 13, seq(23, max(tb_MRcalcs$Resp_Day) + 10, by = 10)),
  right = FALSE,
  labels = c("April", "May", "June", "July", "August", "September")
)

gg_SMR_byTempMonth <- ggplot(data=tb_MRcalcs) +
  geom_point(aes(x = mass, y = SMR, colour = as.factor(Temp_class))) +
  geom_smooth(aes(x = mass, y = SMR, colour = as.factor(Temp_class)), method = lm) +
  facet_wrap(~Month) +
  labs(x="Mass", y="Standard Metabolic Rate (mgO2/min)", title = "SMR" ) +
  theme_classic()+
  scale_color_manual(values = c("cadetblue3", "coral1"))+
  labs(colour="Temperature (degC)")

ggsave(gg_SMR_byTempMonth, filename = "SMR_byTempMonth.png", bg = "transparent", width = 8.5, height = 11, units = "in")

gg_MMR_byTempMonth <- ggplot(data=tb_MRcalcs) +
  geom_point(aes(x = mass, y = MMR, colour = as.factor(Temp_class))) +
  geom_smooth(aes(x = mass, y = MMR, colour = as.factor(Temp_class)), method = lm) +
  facet_wrap(~Month) +
  labs(x="Mass", y="Maximum Metabolic Rate (mgO2/min)", title = "MMR" ) +
  theme_classic()+
  scale_color_manual(values = c("cadetblue3", "coral1"))+
  labs(colour="Temperature (degC)")

ggsave(gg_MMR_byTempMonth, filename = "MMR_byTempMonth.png", bg = "transparent", width = 8.5, height = 11, units = "in")

gg_RMR_byTempMonth <- ggplot(data=tb_MRcalcs) +
  geom_point(aes(x = mass, y = RMR, colour = as.factor(Temp_class))) +
  geom_smooth(aes(x = mass, y = RMR, colour = as.factor(Temp_class)), method = lm) +
  facet_wrap(~Month) +
  labs(x="Mass", y="Routine Metabolic Rate (mgO2/min)", title = "RMR" ) +
  theme_classic()+
  scale_color_manual(values = c("cadetblue3", "coral1"))+
  labs(colour="Temperature (degC)")

ggsave(gg_RMR_byTempMonth, filename = "RMR_byTempMonth.png", bg = "transparent", width = 8.5, height = 11, units = "in")

gg_massSMR_byTempMonth <- ggplot(data = tb_MRcalcs) +
  geom_point(aes(x = mass, y = SMR_mass, colour = as.factor(Temp_class))) +
  geom_smooth(aes(x = mass, y = SMR_mass, colour = as.factor(Temp_class)), method = "lm") +
  facet_wrap(~Month, scales = "free_x") +
  labs(x = "Time", y = "SMR (MO2/hr/3.28g)") +
  theme_classic()+
  scale_color_manual(values = c("cadetblue3", "coral1"))+
  labs(colour="Temperature (degC)")

ggsave(gg_massSMR_byTempMonth, filename = "massSMR_byTempMonth.png", bg = "transparent", width = 8.5, height = 11, units = "in")

##################################################################################

#Still need to merge in the true MMR data for September and check it against the active MR data

setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Raw Data/Respirometry Data/Real MMR from last round")

file.listrealMMR <- list.files()
realMMR_all <- lapply(file.listrealMMR, read_excel)
tb_realMMRall <- bind_rows(realMMR_all) %>%
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
  drop_na() %>%
  mutate(
    volume_net = volume_ch - mass,
    MMR_wBR    = abs(slope_wBR)*(volume_net/1000)*60*60, #uncomment for mgO2/hr instead
    BR         = abs(BRSlope)*(volume_ch/1000)*60*60,
    MMR        = MMR_wBR - BR,
    BR_perc    = (BR/MMR_wBR)*100
)


MMR_validation <- left_join(tb_MRcalcs[which(tb_MRcalcs$Month == "September"),], tb_realMMRall %>% dplyr::select(ID_fish, MMR), by = "ID_fish") %>%   #table which combines both MMR measurements for September
  mutate(
    MaxMR_diff = MMR.y - MMR.x,
    MaxMR_ratio = MMR.y/MMR.x,
    log_mass = log10(mass)
  ) %>%
  rename(
    AMR = MMR.x,
    MMR = MMR.y,
    AMR_mass = MMR_mass,
    AMR_resid = MMR_resid
  )

MMR_comparison <- ggplot(data = MMR_validation, aes(AMR, MMR, colour = as.factor(Temp_class))) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(y=expression(Maximum~Metabolic~Rate~(mg~O[2]~h^-1)), x=expression(Active~Metabolic~Rate~(mg~O[2]~h^-1))) +
  theme_classic()+
  scale_color_manual(values = c("#78B7C5", "#F21A00")) +
  labs(colour=expression("Temperature ("*degree*C*")"))


mmr_comparison_18 <- lm(MMR ~ AMR, data = MMR_validation[which(MMR_validation$Temp_class == "18"),])
mmr_comparison_23 <- lm(MMR ~ AMR, data = MMR_validation[which(MMR_validation$Temp_class == "23"),])
mmr_comparison_mod <- lm(MMR ~ AMR*Temp_class, data = MMR_validation)

summary(mmr_comparison_18)
summary(mmr_comparison_23)
summary(mmr_comparison_mod)

mmr_comparison_mod.compare <- lstrends(mmr_comparison_mod, ~ Temp_class, var = "AMR")
mmr_comparison_mod.slope <- cld(mmr_comparison_mod.compare)

#now compare intercepts
mmr_comparison_mod.int.compare <- emmeans(mmr_comparison_mod, ~Temp_class, at = list(mass = 0))
mmr_comparison_mod.int <- cld(mmr_comparison_mod.int.compare)


setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Figures")

ggsave(MMR_comparison, filename = "MMR_comparison.png", bg = "transparent", width = 8.5, height = 11, units = "in")

ggplot(data = MMR_validation) +
  geom_point(aes(x = mass, y = MMR, color = factor(Temp_class)), shape = 4) +
  geom_point(aes(x = mass, y = AMR, color = factor(Temp_class))) +
 # geom_segment(aes(x = mass.x, xend = mass.x, y = MMR.y, yend = MMR.x, color = factor(Temp_class.x), group = ID_fish)) +
  theme_classic() +
  scale_color_manual(values = c("#78B7C5", "#F21A00")) +
  labs(colour = expression("Temperature ("*degree*C*")"), y = expression(Metabolic~Rate~(mg~O[2]~h^-1)), x = "mass (g)")

tb_mO2max <- MMR_validation %>% 
  dplyr::select(
    ID_fish, Temp_class, mass, log_mass, AMR, MMR) %>%
  pivot_longer(
    cols = 5:6, names_to = "method", values_to = "MO2max") %>%
  mutate(
    method = droplevels(as.factor(method)),
    Temp_class = droplevels(as.factor(Temp_class))
  ) %>% drop_na()

ggplot(data = tb_mO2max) +
  geom_boxplot(aes(x = method, y = MO2max, group = Temp_class, color = factor(Temp_class))) +
  theme_classic()

mmr_scaling_mod <- lm(log10(MO2max) ~ log_mass*Temp_class*method, data = tb_mO2max)
summary(mmr_scaling_mod)

mmr_scaling_mod.compare <- lstrends(mmr_scaling_mod, ~ Temp_class*method, var = "log_mass")
mmr_scaling_mod.slope <- cld(mmr_scaling_mod.compare)

#now compare intercepts
mmr_scaling_mod.int.compare <- emmeans(mmr_scaling_mod, ~Temp_class*method, at = list(log_mass = 0))
mmr_scaling_mod.int <- cld(mmr_scaling_mod.int.compare)


ggplot(data = MMR_validation) +
  geom_point(aes(y = MaxMR_diff, x = mass.x, color = factor(Temp_class.x))) +
  geom_hline(yintercept = 0) +
  theme_classic() +
  scale_color_manual(values = c("#78B7C5", "#F21A00")) +
  labs(colour = expression("Temperature ("*degree*C*")"), y = expression(MMR~-~AMR~(mg~O[2]~h^-1)), x = "mass (g)")

ggplot(data = MMR_validation) +
  geom_point(aes(y = MaxMR_ratio, x = mass.x, color = factor(Temp_class.x))) +
  geom_hline(yintercept = 1) +
  theme_classic() +
  scale_color_manual(values = c("#78B7C5", "#F21A00")) +
  labs(colour = expression("Temperature ("*degree*C*")"), y = "Ratio MMR:AMR", x = "mass (g)")

MMR_galaxiids <- read.csv("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Figures/MMR_validation.csv")

ggplot(data = MMR_galaxiids, aes(x = mass, y = MMR, color = factor(Temp_class), shape = factor(Source), group = factor(Source))) +
  geom_smooth(aes(linetype = factor(Source)), method = "lm", se = F, color = "lightgray") +
  geom_point() +
  scale_shape_manual(values = c(19, 18, 4, 1)) +
  scale_color_manual(values = c("#3B9AB2","#78B7C5","#E1AF00", "#F21A00"))+ 
  theme_classic() +
  labs(colour = expression("Temperature ("*degree*C*")"), shape = "Data source", linetype = "Data source", y = expression(Maximum~Metabolic~Rate~(mg~O[2]~h^-1)), x = "Mass (g)")

