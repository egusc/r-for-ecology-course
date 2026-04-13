# Coding Club Workshop 2 - Basic data manipulation
# Challenge exercise
# Written by Ewan Guscott 13/04/2026 Leeds

library(tidyr)             # load the package
library(dplyr)              # load the package

dragons <- read.csv("dragons.csv", header = TRUE) 

#Split dragons csv
dragons_long <- gather(dragons, Spice, Length,                          
                          c(tabasco, jalapeno, wasabi, paprika))  

boxplot(Length ~ Spice, data = dragons_long,
        xlab = "Spice", ylab = "Plume Size (cm)",
        main = "Length of fire plumes based on spice used")

#Mistake 1: The fourth treatment wasn’t paprika at all, it was turmeric.

dragons_long[dragons_long$Spice == 'paprika', 3] <- "turmeric"
#Post-challenge observation: would have been better to use dragons <- rename(dragons, turmeric = paprika)


boxplot(Length ~ Spice, data = dragons_long,
        xlab = "Spice", ylab = "Plume Size (cm)",
        main = "Length of fire plumes based on spice used")

#Mistake 2: There was a calibration error with the measuring device for the tabasco trial, but only for the Hungarian Horntail species. All measurements are 30 cm higher than they should be.

dragons_long[dragons_long$species == 'hungarian_horntail', 4] <- dragons_long[dragons_long$species == 'hungarian_horntail', 4] + 30

boxplot(Length ~ Spice, data = dragons_long,
        xlab = "Spice", ylab = "Plume Size (cm)",
        main = "Length of fire plumes based on spice used")

#Mistake 3: The lengths are given in centimeters, but really it would make sense to convert them to meters.

dragons_long[, 4] <- dragons_long[, 4] / 100

boxplot(Length ~ Spice, data = dragons_long,
        xlab = "Spice", ylab = "Plume Size (cm)",
        main = "Length of fire plumes based on spice used")
