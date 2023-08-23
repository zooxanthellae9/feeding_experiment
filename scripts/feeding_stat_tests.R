library(ggplot2)
library(car)
library(dplyr)
library(report)

# read in csv data 
feeding <- read.csv("~/for_analysis_feeding_data.csv")
feeding

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




## ONE WAY ANOVA (TREATMENT)

# H0: no significant difference in mean SA/perimeter change among treatments
# HA: mean SA/perimeter change is significantly different from the overall mean for at least one treatment



## SA vs Treatment

SA_Treatment_aov <- aov(SA_Change ~ Treatment, data = feeding_nona)


# Test assumptions 

# Testing normality assumption:

# visual tests:
par(mfrow = c(1,2))
# (1) histogram of residuals
hist(SA_Treatment_aov$residuals)
# (2) QQ-plot
plot(SA_Treatment_aov, which = 2)

# test normality formally with the Shapiro-Wilk test
# p-value = 3.114e-06 *
shapiro.test(SA_Treatment_aov$residuals)

# Testing homogeneity of variances assumption:

# NB also see boxplots to visually assess variances
# visual tests:  
par(mfrow = c(1,2))
# (1) Residuals vs Fitted
plot(SA_Treatment_aov, which = 1)
# (2) Scale-Location
plot(SA_Treatment_aov, which = 3)


# test homogeneity of variances formally using Levene's test
# p-value = 0.5721 
leveneTest(SA_Change ~ Treatment, data = feeding_nona)


# Parametric test (ANOVA)
# p-value = 0.667
summary(SA_Treatment_aov)
report(SA_Treatment_aov)
# p-value >0.05 -> cannot reject null hypothesis 



## Perimeter vs Treatment

Perim_Treatment_aov <- aov(Perim_Change ~ Treatment, data = feeding_nona)


# Test assumptions 

# Testing normality assumption:

# visual tests:
par(mfrow = c(1,2))
# (1) histogram of residuals
hist(Perim_Treatment_aov$residuals)
# (2) QQ-plot
plot(Perim_Treatment_aov, which = 2)

# test normality formally with the Shapiro-Wilk test
# p-value = 0.0003976 *
shapiro.test(Perim_Treatment_aov$residuals)

# Testing homogeneity of variances assumption:

# NB also see boxplots to visually assess variances
# visual tests:  
par(mfrow = c(1,2))
# (1) Residuals vs Fitted
plot(Perim_Treatment_aov, which = 1)
# (2) Scale-Location
plot(Perim_Treatment_aov, which = 3)

# test homogeneity of variances formally using Levene's test
# p-value = 0.7357 
leveneTest(Perim_Change ~ Treatment, data = feeding_nona)


# Parametric test (ANOVA)
# p-value = 0.121
summary(Perim_Treatment_aov)
report(Perim_Treatment_aov)
# p-value >0.05 -> cannot reject null hypothesis



# 2 SAMPLE T-TESTS (TANK)

HD_1 <- filter(feeding_nona, Tank == "HD_1")
HD_2 <- filter(feeding_nona, Tank == "HD_2")
HD <- filter(feeding_nona, Treatment == "HD")
LD_1 <- filter(feeding_nona, Tank == "LD_1")
LD_2 <- filter(feeding_nona, Tank == "LD_2")
LD <- filter(feeding_nona, Treatment == "LD")
C_1 <- filter(feeding_nona, Tank == "C_1")
C_2 <- filter(feeding_nona, Tank == "C_2")
C <- filter(feeding_nona, Treatment == "C")


# SA: HD_1 vs HD_2

# normality test
# p-value = 0.5093
shapiro.test(HD_1$SA_Change)
# p-value = 0.4484
shapiro.test(HD_2$SA_Change)
# test for homogeneity of variances  
# p-value = 0.7533
leveneTest(SA_Change ~ Tank, data = HD)

# t-test: p-value = 0.2346
t.test(HD_1$SA_Change, HD_2$SA_Change, var.equal = TRUE)


# SA: LD_1 vs LD_2

# normality test
# p-value = 0.07808
shapiro.test(LD_1$SA_Change)
# p-value = 0.6444
shapiro.test(LD_2$SA_Change)
# test for homogeneity of variances  
# p-value = 0.1194
leveneTest(SA_Change ~ Tank, data = LD)

# t-test: p-value =  0.1348
t.test(LD_1$SA_Change, LD_2$SA_Change, var.equal = TRUE)


# SA: C_1 vs C_2 

# normality test
# p-value = 0.0005318 * 
shapiro.test(C_1$SA_Change)
# p-value = 0.8027
shapiro.test(C_2$SA_Change)
# test for homogeneity of variances  
# p-value = 0.9565
leveneTest(SA_Change ~ Tank, data = C)

# Mann-Whitney U test: p-value = 0.9484
wilcox.test(C_1$SA_Change, C_2$SA_Change)


# Perim: HD_1 vs HD_2

# normality test
# p-value = 0.315
shapiro.test(HD_1$Perim_Change)
# p-value = 0.3032
shapiro.test(HD_2$Perim_Change)
# test for homogeneity of variances  
# p-value = 0.9565
leveneTest(Perim_Change ~ Tank, data = HD)

# t-test: p-value = 0.3923
t.test(HD_1$Perim_Change, HD_2$Perim_Change, var.equal = TRUE)


# Perim: LD_1 vs LD_2

# normality test
# p-value = 0.0116 * 
shapiro.test(LD_1$Perim_Change)
# p-value = 0.5727
shapiro.test(LD_2$Perim_Change)
# test for homogeneity of variances  
# p-value = 0.2095
leveneTest(Perim_Change ~ Tank, data = LD)

# Mann-Whitney U test: p-value = 0.7596
wilcox.test(LD_1$Perim_Change, LD_2$SA_Change)


# Perim: C_1 vs C_2 

# normality test
# p-value = 0.3107
shapiro.test(C_1$Perim_Change)
# p-value = 0.8832
shapiro.test(C_2$Perim_Change)
# test for homogeneity of variances  
# p-value = 0.293
leveneTest(Perim_Change ~ Tank, data = C)

# p-value = 0.5459
t.test(C_1$Perim_Change, C_2$Perim_Change, var.equal = TRUE)
