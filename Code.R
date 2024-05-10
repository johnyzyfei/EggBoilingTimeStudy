# =================================================================== #
# ============================= Preamble ============================== #
# =================================================================== #
# Please manually install any missing packages using install.package()
library(rstatix)
library(ggplot2)
library(ggpubr)
library(readxl)
library(pwr)

data = read_excel("STA305 Group Project Data.xlsx")

dat = data[,-1]
a = factor(rep(c('a1', 'a2', 'a3', 'a4'), each=10))
data_egg = data.frame(cbind(a, dat))
names(data_egg) = c('a', 'salted', 'water_temp', 'y')

# ========================================================= #
# ============== Calculating Sample Size ================== #
# ========================================================= #
pwr.anova.test(k = 4, f = 0.5, sig.level = 0.05, power = 0.70)
# from the output, a sample size of 10 per group is sufficient

# ========================================================== #
# ========== Randomization of Experimental Units =========== #
# ========================================================== #
set.seed(2000)
eggs = 1:40
sample(eggs, replace = FALSE, size = 40)

# ============================================================= #
# ================== Model Assumptions ======================== #
# ============================================================= #

##############################################
################ Test for Normality ##########
##############################################
# We split up the response for the groups, and test if each of the responses for
# the four groups are normally distributed or not.
g1 = data_egg$y[1:10]
g2 = data_egg$y[11:20]
g3 = data_egg$y[21:30]
g4 = data_egg$y[31:40]
#testing group 1
shapiro_test(g1)

#testing group 2
shapiro_test(g2)

#testing group 3
shapiro_test(g3)

#testing group 4
shapiro_test(g4)


#####################################
#### Test for constant variance #####
#####################################

## The normality assumption for the bartlett.test is not satisfied
bartlett.test(y ~ a, data=data_egg)

## Creating boxplot for the four treatments in order as codebook
ggplot(data_egg, aes(x = a, y = y)) +
  geom_boxplot() +
  labs(title = "Comparison of Egg Temperatures by Treatment",
       x = "Treatments",
       y = "Egg Temperature")

# ==============================================================
# ========================== Section 1 ========================= #
# ==============================================================

library(rstatix)
library(readr)
library(readxl)

data = read_excel("STA305 Group Project Data.xlsx")
dat = data[,-1]
a = factor(rep(c('a1', 'a2', 'a3', 'a4'), each=10))
data_egg = data.frame(cbind(a, dat))
names(data_egg) = c('a', 'salted', 'water_temp', 'y')
attach(data_egg)

#############################################
############### TWO WAY ANOVA ############### 
#############################################
#Means
with(data_egg, tapply(y, salted, mean))
with(data_egg, tapply(y, water_temp, mean))
with(data_egg, tapply(y, list(salted, water_temp), mean))


#Interaction Plot
with(data_egg, interaction.plot(salted, water_temp, y, col=c("red", "blue"),
                                main="Interaction Plot", xlab="Salted mean", ylab="Yield"))
with(data_egg, interaction.plot(water_temp, salted, y, col=c("red", "blue"),
                                main="Interaction Plot", xlab="Salted mean", ylab="Yield"))


#Model with interaction
model1 <- lm(y ~ salted*water_temp, data = data_egg)
summary(model1)
anova(model1)
# Reject H0, interaction not significant

#############################################
############### OVERALL TEST ################ 
#############################################

#Additive Model
model2 <- lm(y~ salted + water_temp, data = data_egg)
summary(model2)
anova(model2)
# Fail to reject H0, both factors on their own are significant 


#############################################
################# POST HOC ################## 
#############################################

# using Bonferroni coerrection to test general contrast
fit <- lm(y ~ a- 1, data=data_egg)
L <- matrix(c(
  -1, 0, 0, 1,
  -1, -1, -1, 3), byrow=T, nrow=2)
summary(glht(fit, L), test=adjusted('bonferroni'))

# the p-values are divided by 2 and we take note of the sign of estimates reported as we are drawing directional conclustions


