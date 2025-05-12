rm(list=ls())

library(tidyverse) #loads ggplot2, dplyr, tidyr, tibble, stringr
library(readxl)
library(lubridate)
library(hms)
library(ggpubr)
library(wesanderson)
library(lsmeans)
library(multcomp)
library(multcompView)

extract_date <- function(files) {
  gsub(".*?([0-9]{8}).*", "\\1", basename(files))
}

#define pathways
active_folder <- "C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Raw Data/Respirometry Data/MMR"
mmr_folder <- "C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Raw Data/Respirometry Data/Real MMR from last round"

# Get the list of files in each folder
active_files <- list.files(active_folder, pattern = "_MMR.xlsx", full.names = TRUE)
mmr_files <- list.files(mmr_folder, pattern = "_MMR2.xlsx", full.names = TRUE)

# Extract date strings from file names
active_dates <- extract_date(active_files)
mmr_dates <- extract_date(mmr_files)

#Pairing matching dates between the folders
matching_files <- Map(function(date) {
  mmr_file <- mmr_files[grep(date, mmr_dates)]
  active_file <- active_files[grep(date, active_dates)]
  return(list(mmr = mmr_file, active = active_file))
}, unique(c(mmr_dates, active_dates)))


# Loop through matching files and perform your data processing
for (i in seq_along(matching_files)) {
  date <- unique(mmr_dates)[i] #taking only the September dates that match
  current_files <- matching_files[[i]]  # Renamed 'files' to 'current_files'
  
tb_mmr1 <- read_excel(current_files$active) %>%
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

tb_mmr1 <- tb_mmr1 %>%
  
  mutate(
    volume_net = volume_ch - mass,
    MMR_wBR    = abs(slope_wBR)*(volume_net/1000)*60*60, 
    BR         = abs(BRSlope)*(volume_ch/1000)*60*60,
    AMR        = MMR_wBR - BR,
    BR_perc    = (BR/MMR_wBR)*100
  ) 

tb_mmr2 <- read_excel(current_files$mmr) %>%
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

tb_mmr2 <- tb_mmr2 %>%
  mutate(
    volume_net = volume_ch - mass,
    MMR_wBR    = abs(slope_wBR)*(volume_net/1000)*60*60, 
    BR         = abs(BRSlope)*(volume_ch/1000)*60*60,
    MMR_chase  = MMR_wBR - BR,
    BR_perc    = (BR/MMR_wBR)*100
  ) 

# Combine the processed data, selecting only specific columns
tb_MMR_September <- tb_mmr1 %>%
  left_join(select(tb_mmr2, ID_fish, MMR_chase), by = "ID_fish") %>%
  select(-matches(".x$")) %>%
  rename_with(~gsub("\\.y$", "", .), matches(".y$"))

# Export data
setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Raw Data/Respirometry Data/Real MMR from last round/September")
write.csv(tb_MMR_September, file = paste0(date, "_MMR_September.csv"), col.names = NA, row.names = FALSE)
}

setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Raw Data/Respirometry Data/Real MMR from last round/September")

#read all "_MRcalcs.csv" files, bind into one table, and add columns with logged values 
file.list <- list.files()
calcs_all <- lapply(file.list, read_csv)
tb_MMR_compare <- bind_rows(calcs_all, .id="Resp_Day") %>%
  mutate(
    log_mass = log10(mass),
    log_MMRc = log10(MMR_chase),
    Resp_Day = as.numeric(Resp_Day)
  )

mean_log_mass = data.frame(log_mass = (mean(tb_MMR_compare$log_mass)))

#creating a linear regression between MMR and mass for both temperature treatments
lm_mmr_18 = lm(log_MMRc ~ log_mass, tb_MMR_compare[which(tb_MMR_compare$Temp_class == 18),]) 
lm_mmr_23 = lm(log_MMRc ~ log_mass, tb_MMR_compare[which(tb_MMR_compare$Temp_class == 23),]) 

#predicting the MMR value for "mass" using our regression
MMR_mean_predicted_18 = predict(lm_mmr_18, newdata = mean_log_mass) 
MMR_mean_predicted_23 = predict(lm_mmr_23, newdata = mean_log_mass) 

#adding the residuals calculated from our regressions to our table
tb_MMR_compare <- tb_MMR_compare %>%
  mutate(resid = ifelse(Temp_class == 18, residuals(lm_mmr_18), residuals(lm_mmr_23)))

#Adding new columns with mass standardized MMR calculations
tb_MMR_compare <- 
  tb_MMR_compare %>%
  mutate(
    log_MMRc_mass = ifelse(Temp_class == 18, MMR_mean_predicted_18 + resid, MMR_mean_predicted_23 + resid), #use the 2 models to calc residuals + predicted values for each FishID/time,
    MMRc_mass = as.numeric(10^log_MMRc_mass),
    resid = NULL,
    log_MMRc_mass = NULL,
    log_MMRc = NULL,
    log_mass = NULL,
  ) %>%
  drop_na() %>%
  arrange(Resp_Day, ID_fish)


p1 <- plot(tb_MMR_compare$AMR, tb_MMR_compare$MMR_chase, col = as.factor(tb_MMR_compare$Temp_class), abline(a=0, b=1))

lm_AMR_MMR = lm(MMR_chase ~ AMR, data = tb_MMR_compare)
summary(lm_AMR_MMR)

t.test(tb_MMR_compare$AMR, tb_MMR_compare$MMR_chase, paired = TRUE, alternative = "greater")

ggplot(data = tb_MMR_compare) +
  geom_abline(intercept = 0.50, slope = 0.29, size = 1) +
  geom_smooth(aes(x = mass, y = MMR_chase, color = as.factor(Temp_class)), method = "lm", se = T) +
  geom_smooth(aes(x = mass, y = AMR, color = as.factor(Temp_class)), method = "lm", linetype = "dashed", se = T) +
  geom_point(aes(x = mass, y = MMR_chase, color = as.factor(Temp_class)), shape = "circle") +
  geom_point(aes(x = mass, y = AMR, color = as.factor(Temp_class)), shape = "triangle") +
  scale_color_manual(values = c("cadetblue3", "coral1"))+
  #scale_x_log10() +
  #scale_y_log10() +
  labs(x = "Mass (g)", y = "MR (mgO2/hr)") +
  labs(colour="Temperature (degC)") +
  theme_classic()

setwd("C:/Users/s222141733/OneDrive - Deakin University/Beth Hoots/2023 Growth Performance/Analysis/Figures")
tb_MR <- read.csv("KitchenSink_Survivors.csv")

tb_MR_September <- tb_MR[which(tb_MR$Month == "September"),] 
step <- tb_MMR_compare[, c(3, 17,19, 20)]

tb_MR_September <- tb_MR_September %>%
  left_join(step, by = "ID_fish") %>%
  rename(
    AMR_mass = MMR_mass
  ) %>%
  mutate(
    logmass  = log10(mass),
    MMR      = NULL,
    RMR_var  = NULL,
    RMR_perc = NULL,
    AAS_c    = MMR_chase - SMR,
    AAS_exp  = AMR - SMR,
    SMR_mass = as.numeric(SMR_mass),
    AMR_mass = as.numeric(AMR_mass),
    AAS_mass_e = AMR_mass - SMR_mass,
    Otolith_age_y = as.numeric(Otolith_age_y),
    Gonad_mass_g = as.numeric(Gonad_mass_g),
    Perc_gonad_mass = Gonad_mass_g/Final_Mass_g*100
  )

tb_SGR_init <- read.csv("SGRcalcs.csv") 

tb_SGR <- tb_SGR_init[which(tb_SGR_init$Month == "September"),]  %>%
  mutate(
    G_abs       = mass - mass_prev,
    Length_diff = length - length_prev
  )

tb_SGR <- tb_SGR[,c(2, 21, 28:32)]

tb_MR_September <- tb_MR_September %>%
  left_join(tb_SGR, by = "ID_fish")

ggplot(data = tb_MR_September) +
  geom_smooth(aes(x = mass, y = SMR, color = as.factor(Temp_class)), method = "lm", se = T) +
  geom_smooth(aes(x = mass, y = AMR, color = as.factor(Temp_class)), method = "lm", linetype = "dashed", se = T) +
  geom_point(aes(x = mass, y = SMR, color = as.factor(Temp_class)), shape = "circle") +
  geom_point(aes(x = mass, y = AMR, color = as.factor(Temp_class)), shape = "triangle") +
  scale_color_manual(values = c("cadetblue3", "coral1"))+
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Mass (g)", y = "MO2 (mgO2/hr)") +
  labs(colour="Temperature (degC)") +
  theme_classic()

ggplot(data = tb_MR_September[which(tb_MR_September$Temp_class == "18"),]) +
  geom_smooth(aes(x = Perc_gonad_mass, y = AMR_mass, color = as.factor(Temp_class)), method = "lm", linetype = "dashed") +
  geom_smooth(aes(x = Perc_gonad_mass, y = SMR_mass, color = as.factor(Temp_class)), method = "lm") +
  geom_point(aes(x = Perc_gonad_mass, y = AMR_mass, color = as.factor(Temp_class)), shape = "triangle") +
  geom_point(aes(x = Perc_gonad_mass, y = SMR_mass, color = as.factor(Temp_class))) +
  scale_color_manual(values =  "cadetblue3") + #c("cadetblue3", "coral1")) +
  #scale_x_log10() +
  #scale_y_log10() +
  labs(x = "Perc. Gonad Mass (%)", y = "MO2 (mgO2/hr)") +
  labs(colour="Temperature (degC)") +
  theme_classic()

ggplot(data = tb_MR_September[which(tb_MR_September$Temp_class == "18"),]) +
  geom_smooth(aes(x = Perc_gonad_mass, y = AAS_mass_e, color = as.factor(Temp_class)), method = "lm") +
  geom_point(aes(x = Perc_gonad_mass, y = AAS_mass_e, color = as.factor(Temp_class))) +
  scale_color_manual(values =  "cadetblue3") + #c("cadetblue3", "coral1")) +
  #scale_x_log10() +
  #scale_y_log10() +
  labs(x = "Perc. Gonad Mass (%)", y = "MO2 (mgO2/hr)") +
  labs(colour="Temperature (degC)") +
  theme_classic()

ggplot(data = tb_MR_September) +
  geom_smooth(aes(x = SMR, y = G_abs, color = as.factor(Temp_class)), method = "lm") +
  geom_point(aes(x = SMR, y = G_abs, color = as.factor(Temp_class))) +
  scale_color_manual(values = c("cadetblue3", "coral1")) +
  #scale_x_log10() +
  #scale_y_log10() +
  labs(x = "SMR", y = "SGR (%/day)") +
  labs(colour="Temperature (degC)") +
  theme_classic()

ggplot(data = tb_MR_September[which(tb_MR_September$Temp_class == "18"),]) +
  geom_smooth(aes(x = Perc_gonad_mass, y = SMR_mass, color = "SMR"), method = "lm") +
  geom_point(aes(x = Perc_gonad_mass, y = SMR_mass, color = "SMR")) +
  geom_smooth(aes(x = Perc_gonad_mass, y = AMR_mass, color = "MMR"), method = "lm") +
  geom_point(aes(x = Perc_gonad_mass, y = AMR_mass, color = "MMR")) +
  geom_smooth(aes(x = Perc_gonad_mass, y = AAS_mass_e, color = "AAS"), method = "lm") +
  geom_point(aes(x = Perc_gonad_mass, y = AAS_mass_e, color = "AAS")) +
  scale_color_manual(values = wes_palette("BottleRocket2", n = 3)) +
  labs(x = "Perc. Gonad Mass (%)", y = "MO2 (mgO2/hr)") +
  theme_classic()

AASmod <- lm(log10(AAS_exp) ~ logmass*Temp_class, data = tb_MR_September)

plot(AASmod)
hist(AASmod$residuals)

summary(AASmod)
anova(AASmod)

AASmod.compare <- lstrends(AASmod, ~ Temp_class, var = "logmass")
AASmod.slope <- cld(AASmod.compare)

MMRmod <- lm(log10(AMR) ~ logmass*Temp_class, data = tb_MR_September)

plot(MMRmod)
hist(MMRmod$residuals)

summary(MMRmod)
anova(MMRmod)

MMRmod.compare <- lstrends(MMRmod, ~ Temp_class, var = "logmass")
MMRmod.slope <- cld(MMRmod.compare)

SMRmod <- lm(log10(SMR) ~ logmass*Temp_class, data = tb_MR_September)

plot(SMRmod)
hist(SMRmod$residuals)

summary(SMRmod)
anova(SMRmod)

SMRmod.compare <- lstrends(SMRmod, ~ Temp_class, var = "logmass")
SMRmod.slope <- cld(SMRmod.compare)

RMRmod <- lm(log10(RMR) ~ logmass*Temp_class, data = tb_MR_September)

plot(RMRmod)
hist(RMRmod$residuals)

summary(RMRmod)
anova(RMRmod)

RMRmod.compare <- lstrends(RMRmod, ~ Temp_class, var = "logmass")
RMRmod.slope <- cld(RMRmod.compare)


ggplot(data = tb_SGR_init, aes(x = MMR, fill = as.factor(Temp_class))) +
  geom_histogram(color = "black") +
  facet_grid(Temp_class~factor(Month, levels = c("May", "June", "July", "August", "September"))) +
  scale_fill_manual(values = c("cadetblue3", "coral1")) +
  labs(x = "SGR (%/day)", y = "Frequency", fill = expression("Temperature ("*degree*C*")")) +
  theme_classic()

slide22 <- ggplot(data = tb_SGR_init, aes(x = SGR, fill = as.factor(Temp_class))) +
  geom_histogram(color = "white") +
  facet_grid(Temp_class~factor(Month, levels = c("May", "June", "July", "August", "September"))) +
  scale_fill_manual(values = c("cadetblue3", "coral1")) +
  labs(x = "SGR (%/day)", y = "Frequency", fill = expression("Temperature ("*degree*C*")")) +
  theme_classic() +
  theme(plot.title = element_text(size = 22, color = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, colour = "white"), 
        axis.text = element_text(size = 17, colour = "white"), 
        axis.title = element_text(size = 20, colour = "white"), 
        legend.text = element_text(size = 20, colour = "white"), 
        legend.title = element_text(size = 20, colour = "white"), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(color = "white"), 
        plot.background = element_rect(fill = "transparent", color = NA), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        strip.text = element_text(size = 17, colour = "white"))

ggsave("slide22.png", plot = slide22, bg = "transparent", width = 11, height = 6)

slide23 <- ggplot(data = tb_SGR_init, aes(x = SGR, fill = as.factor(Temp_class))) +
  geom_histogram(color = "white") +
  facet_grid(.~Temp_class) +
  scale_fill_manual(values = c("cadetblue3", "coral1")) +
  labs(x = "SGR (%/day)", y = "Frequency", fill = expression("Temperature ("*degree*C*")")) +
  theme_classic() +
  theme(plot.title = element_text(size = 22, color = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, colour = "white"), 
        axis.text = element_text(size = 17, colour = "white"), 
        axis.title = element_text(size = 20, colour = "white"), 
        legend.text = element_text(size = 20, colour = "white"), 
        legend.title = element_text(size = 20, colour = "white"), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(color = "white"), 
        plot.background = element_rect(fill = "transparent", color = NA), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        strip.text = element_text(size = 17, colour = "white"))

ggsave("slide23.png", plot = slide23, bg = "transparent", width = 11, height = 7)

slide24 <- ggplot(data = tb_MR_September, aes(x = Perc_gonad_mass, fill = factor(Temp_class))) +
  geom_histogram(color = "white") +
  facet_grid(.~Temp_class) +
  scale_fill_manual(values = c("cadetblue3", "coral1")) +
  labs(x = "Gonad Mass (% total mass)", y = "Frequency", fill = expression("Temperature ("*degree*C*")")) +
  theme_classic() +
  theme(plot.title = element_text(size = 22, color = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, colour = "white"), 
        axis.text = element_text(size = 17, colour = "white"), 
        axis.title = element_text(size = 20, colour = "white"), 
        legend.text = element_text(size = 20, colour = "white"), 
        legend.title = element_text(size = 20, colour = "white"), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(color = "white"), 
        plot.background = element_rect(fill = "transparent", color = NA), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        strip.text = element_text(size = 17, colour = "white"))

ggsave("slide24.png", plot = slide24, bg = "transparent", width = 11, height = 7)

slide25 <- ggplot(data = tb_MR_September[which(tb_MR_September$Temp_class == "18"),]) +
  geom_smooth(aes(x = Perc_gonad_mass, y = SMR_mass, color = "SMR"), method = "lm") +
  geom_point(aes(x = Perc_gonad_mass, y = SMR_mass, color = "SMR")) +
  geom_smooth(aes(x = Perc_gonad_mass, y = AMR_mass, color = "MMR"), method = "lm") +
  geom_point(aes(x = Perc_gonad_mass, y = AMR_mass, color = "MMR")) +
  geom_smooth(aes(x = Perc_gonad_mass, y = AAS_mass_e, color = "AAS"), method = "lm") +
  geom_point(aes(x = Perc_gonad_mass, y = AAS_mass_e, color = "AAS")) +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 3)) +
  labs(x = "Perc. Gonad Mass (%)", y = "MO2 (mgO2/hr)") +
  theme_classic()  +
  theme(plot.title = element_text(size = 22, color = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, colour = "white"), 
        axis.text = element_text(size = 17, colour = "white"), 
        axis.title = element_text(size = 20, colour = "white"), 
        legend.text = element_text(size = 20, colour = "white"), 
        legend.title = element_text(size = 20, colour = "white"), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(color = "white"), 
        plot.background = element_rect(fill = "transparent", color = NA), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"))

ggsave("slide25.png", plot = slide25, bg = "transparent", width = 12, height = 9)

ggplot(data = tb_SGR_init, aes(x = SGR, fill = factor(Month, levels = c("May", "June", "July", "August", "September")))) +
  geom_histogram(color = "black") +
  facet_grid(.~Temp_class) +
  scale_fill_manual(values = wes_palette("Zissou1", n = 5)) +
  labs(x = "SGR (%/day)", y = "Frequency", fill = "Month") +
  theme_classic()
