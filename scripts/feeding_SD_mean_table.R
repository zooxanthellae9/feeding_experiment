#load packages
library(dplyr)
library(knitr)

# read in csv data 
feeding <- read.csv("~/for_analysis_feeding_data.csv")

# ensure correct data classes 
feeding$Surface_Area_02.08 <- as.numeric(feeding$Surface_Area_02.08)
feeding$Perimeter_02.08 <- as.numeric(feeding$Perimeter_02.08)
feeding$Surface_Area_11.08 <- as.numeric(feeding$Surface_Area_11.08)
feeding$Perimeter_11.08 <- as.numeric(feeding$Perimeter_11.08)
feeding$SA_Change <- as.numeric(feeding$SA_Change)
feeding$Perim_Change <- as.numeric(feeding$Perim_Change)
feeding$Treatment <- as.factor(feeding$Treatment)
feeding$Tank <- as.factor(feeding$Tank)
feeding$Substrate_Type <- as.factor(feeding$Substrate_Type)
feeding$Substrate_Label <- as.factor(feeding$Substrate_Label)
feeding$Polyp_Number <- as.factor(feeding$Polyp_Number)
feeding$Photo_Taken_02.08 <- as.factor(feeding$Photo_Taken_02.08)
feeding$Photo_Taken_11.08 <- as.factor(feeding$Photo_Taken_11.08)
feeding$Source_Parent <- as.factor(feeding$Source_Parent)
# check
summary(feeding)

nrow(feeding)
#remove NAs 
feeding_nona <- na.omit(feeding)
nrow(feeding_nona)

# make table 
summarised_feeding_nona <- feeding_nona %>% 
  # group data by tank
  group_by(Tank) %>%
  # calculate the summary statistics for each tank 
  summarize(mean_SA = mean(SA_Change),
            std_SA = sd(SA_Change),
            mean_Perim = mean(Perim_Change),
            std_Perim = sd(Perim_Change))

# save as a csv 
write.csv(summarised_feeding_nona, 
          file = "/Users/jessyephillips/Desktop/LRF (Bermuda)/Feeding Trial Jessye 2023/R analysis/summarised_feeding.csv", 
          row.names = FALSE)

