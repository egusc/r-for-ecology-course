library(agridat)

# Loading the dataset from agridat
apples <- agridat::archbold.apple
head(apples)
summary(apples)

theme.clean <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.position = "right")
}
apples$spacing2 <- as.factor(apples$spacing)

library(ggplot2)

apples.p <- ggplot(apples, aes(spacing2, yield)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    theme.clean() +  
    theme(axis.text.x = element_text(size = 12, angle = 0)) +
    labs(x = "Spacing (m)", y = "Yield (kg)")

apples.m <- lm(yield ~ spacing2, data = apples)
summary(apples.m)


#An explanation of the below: The coefficients section is split into estimate,
#standard error, t value and probability. (Intercept) is the first data point,
#then each row after is showing the expected different from the last row. T value
#is number of standard deviations away from 0. The further away from 0, the less
#likely a null hypothesis is. Probability should, generally, be lower than 0.05
#to be significant. 

#Multiple and adjusted R value show how likely the variable chosen, in this case yield,
#is explained by selector spacing 2. It goes 0 to 1, 1 being 100%. So in the below,
#it only explains 15# of the variation in yield.

#https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R

#Call:
#  lm(formula = yield ~ spacing2, data = apples)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-92.389 -30.577  -3.516  33.192 117.628 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  120.566      7.382  16.332  < 2e-16 ***
#  spacing210    35.924     11.073   3.244 0.001659 ** 
#  spacing214    44.107     10.966   4.022 0.000121 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 43.67 on 89 degrees of freedom
#(28 observations deleted due to missingness)
#Multiple R-squared:  0.1742,	Adjusted R-squared:  0.1556 
#F-statistic: 9.385 on 2 and 89 DF,  p-value: 0.0002003

sheep <- agridat::ilri.sheep   # load the data

library(dplyr)
sheep <- filter(sheep, ewegen == "R")   # there are confounding variables in this dataset that we don't want to take into account. We'll only consider lambs that come from mothers belonging to the breed "R".

head(sheep)  # overview of the data; we'll focus on weanwt (wean weight) and weanage

#So this is checking whether weanage causes variation in wearwt
sheep.m1 <- lm(weanwt ~ weanage, data = sheep)   # run the model
summary(sheep.m1)                                # study the output

#Result below. This shows that every day the lamb is weaned, the estimated
#weight change is 0.079711, the probability and t value are signficant, and it's 
#estimated to acccount for 19.81% of the variability. 

#Call:
#  lm(formula = weanwt ~ weanage, data = sheep)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-5.5504 -1.1315  0.0518  1.1899  5.4250 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 2.599514   0.892854   2.911  0.00389 ** 
#  weanage     0.079711   0.009499   8.392  2.4e-15 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.963 on 280 degrees of freedom
#(56 observations deleted due to missingness)
#Multiple R-squared:  0.201,	Adjusted R-squared:  0.1981 
#F-statistic: 70.42 on 1 and 280 DF,  p-value: 2.397e-15
  
sheep.m2 <- lm(weanwt ~ weanage*sex, data = sheep)
summary(sheep.m2)

#This is telling us that the weanage has a significant effect on weight, but sex
# gives an estimated decrease to weight game. However, the probability for this is 
#not significant, at 15%.

#Call:
#  lm(formula = weanwt ~ weanage * sex, data = sheep)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-5.7173 -1.1760  0.0179  1.2886  5.6429 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   3.65829    1.24107   2.948  0.00347 ** 
#  weanage       0.06469    0.01308   4.946 1.31e-06 ***
#  sexM         -2.51878    1.76234  -1.429  0.15406    
#weanage:sexM  0.03392    0.01874   1.810  0.07133 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.933 on 278 degrees of freedom
#(56 observations deleted due to missingness)
#Multiple R-squared:  0.2314,	Adjusted R-squared:  0.2231 
#F-statistic:  27.9 on 3 and 278 DF,  p-value: 8.446e-16

(sheep.p <- ggplot(sheep, aes(x = weanage, y = weanwt)) +
    geom_point(aes(colour = sex)) +                                # scatter plot, coloured by sex
    labs(x = "Age at weaning (days)", y = "Wean weight (kg)") +
    stat_smooth(method = "lm", aes(fill = sex, colour = sex)) +    # adding regression lines for each sex
    scale_colour_manual(values = c("#FFC125", "#36648B")) +
    scale_fill_manual(values = c("#FFC125", "#36648B")) +
    theme.clean() )

anova(apples.m) #Gives same p value as the lm


# Checking that the residuals are normally distributed
apples.resid <- resid(apples.m)              # Extracting the residuals
shapiro.test(apples.resid)                   # Using the Shapiro-Wilk test
# The null hypothesis of normal distribution is accepted: there is no significant difference (p > 0.05) from a normal distribution

# Checking for homoscedasticity
bartlett.test(apples$yield, apples$spacing2)
bartlett.test(yield ~ spacing2, data = apples)  # Note that these two ways of writing the code give the same results
# The null hypothesis of homoscedasticity is accepted
