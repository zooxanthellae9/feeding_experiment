
# load packages 
library(ggplot2)
library(cowplot)

# read in csv data 
feeding <- read.csv("for_analysis_feeding_data")

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


## BOX AND WHISKER PLOTS

# TANK: box and whisker for change in surface area 
SA_Tank_boxplot <- ggplot(data = feeding_nona, 
       aes (x = SA_Change, 
            y = Tank, 
            fill = Tank)) + 
         geom_boxplot() + 
  labs(title = "Change in Spat Surface Area (Day 3 - Day 12)", x = "Surface Area Change (%)") +
  coord_flip() +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.title.x=element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") + 
  scale_fill_manual(values = c("#004d00", "#3d8f3d", "#b30000", "#FF6666", "#004080", "#66A3D2"))
SA_Tank_boxplot

# TANK: box and whisker for change in perimeter 
Perim_Tank_boxplot <- ggplot(data = feeding_nona, 
                     aes (x = Perim_Change, 
                          y = Tank, 
                          fill = Tank)) + 
  geom_boxplot() + 
  labs(title = "Change in Spat Perimeter (Day 3 - Day 12)", x = "Perimeter Change (%)") + 
  coord_flip() + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.title.x=element_blank()) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  scale_fill_manual(values = c("#004d00", "#3d8f3d", "#b30000", "#FF6666", "#004080", "#66A3D2")) 
Perim_Tank_boxplot

# TREATMENT: box and whisker for change in surface area 
SA_Treatment_boxplot <- ggplot(data = feeding_nona, 
                               aes (x = SA_Change, 
                                    y = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Change in Spat Surface Area (Day 3 - Day 12)", x = "Surface Area Change (%)") + 
  coord_flip() + 
  theme(legend.position="none") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") 
SA_Treatment_boxplot

# TREATMENT: box and whisker for change in perimeter 
Perim_Treatment_boxplot <- ggplot(data = feeding_nona, 
                                  aes (x = Perim_Change, 
                                       y = Treatment)) + 
  geom_boxplot() + 
  labs(title = "Change in Spat Perimeter (Day 3 - Day 12)", x = "Perimeter Change (%)") + 
  coord_flip() + 
  theme(legend.position="none") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") 
Perim_Treatment_boxplot



# plot all graphs together
all_boxplots <- plot_grid(SA_Tank_boxplot, SA_Treatment_boxplot,
          Perim_Tank_boxplot, Perim_Treatment_boxplot,
          nrow = 2, ncol = 2)
ggsave(filename="all_boxplots.png", path = "/Users/jessyephillips/Desktop/LRF (Bermuda)/Feeding Trial Jessye 2023/R analysis/graphs",  
       all_boxplots, device="png", dpi=700)


