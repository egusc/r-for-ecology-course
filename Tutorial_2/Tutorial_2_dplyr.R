# Coding Club Workshop 2 - Basic data manipulation
#Learn base R syntax for data manipulation
#Turn messy data into tidy data with tidyr
#Use efficient tools from the dplyr package to manipulate data
#This file covers dplyr package
# Written by Ewan Guscott 13/04/2026 Leeds

library(tidyr)             # load the package
library(dplyr)              # load the package

elongation <- read.csv("EmpetrumElongation.csv", header = TRUE) 

#Where elongation has columns for multiple years, this seperates them out into different rows for each year
elongation_long <- gather(elongation, Year, Length,                           # in this order: data frame, key, value
                          c(X2007, X2008, X2009, X2010, X2011, X2012))        # we need to specify which columns to gather

elongation_long <- rename(elongation_long, zone = Zone, indiv = Indiv, year = Year, length = Length)     # changes the names of the columns (getting rid of capital letters) and overwriting our data frame

elongation_long

# FILTER OBSERVATIONS

# Let's keep observations from zones 2 and 3 only, and from years 2009 to 2011

elong_subset <- filter(elongation_long, zone %in% c(2, 3), year %in% c("X2009", "X2010", "X2011")) # you can use multiple different conditions separated by commas

elong_subset

# SELECT COLUMNS

# Let's ditch the zone column just as an example

elong_no.zone <- dplyr::select(elongation_long, indiv, year, length)   # or alternatively
elong_no.zone <- dplyr::select(elongation_long, -zone) # the minus sign removes the column

# A nice hack! select() lets you rename and reorder columns on the fly
elong_no.zone <- dplyr::select(elongation_long, Year = year, Shrub.ID = indiv, Growth = length)

head(elongation_long)
head(elong_no.zone)

# CREATE A NEW COLUMN

elong_total <- mutate(elongation, total.growth = X2007 + X2008 + X2009 + X2010 + X2011 + X2012)

head(elong_total)

elong_grouped <- group_by(elongation_long, indiv)   # grouping our dataset by individual
elong_grouped

# SUMMARISING OUR DATA

summary1 <- summarise(elongation_long, total.growth = sum(length))
summary2 <- summarise(elong_grouped, total.growth = sum(length))

#Summar1 is total of all total.growth, Summary2 isa breakdown per individual
summary1
summary2

#Show breakdown of total, mean, and standard deviation
summary3 <- summarise(elong_grouped, total.growth = sum(length),
                      mean.growth = mean(length),
                      sd.growth = sd(length))

summary3

# Load the treatments associated with each individual

treatments <- read.csv("EmpetrumTreatments.csv", header = TRUE, sep = ";")
head(treatments)

# Join the two data frames by ID code. The column names are spelled differently, so we need to tell the function which columns represent a match. We have two columns that contain the same information in both datasets: zone and individual ID.

experiment <- left_join(elongation_long, treatments, by = c("indiv" = "Indiv", "zone" = "Zone"))

# We see that the new object has the same length as our first data frame, which is what we want. And the treatments corresponding to each plant have been added!

head(experiment)

boxplot(length ~ Treatment, data = experiment)
