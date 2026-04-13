# Coding Club Workshop 2 - Basic data manipulation
#Learn base R syntax for data manipulation
#Turn messy data into tidy data with tidyr
#Use efficient tools from the dplyr package to manipulate data
#This file covers tidying datasets
# Written by Ewan Guscott 13/04/2026 Leeds

library(tidyr)             # load the package


elongation <- read.csv("EmpetrumElongation.csv", header = TRUE)   



#Where elongation has columns for multiple years, this seperates them out into different rows for each year
elongation_long <- gather(elongation, Year, Length,                           # in this order: data frame, key, value
                          c(X2007, X2008, X2009, X2010, X2011, X2012))        # we need to specify which columns to gather

# Here we want the lengths (value) to be gathered by year (key)

# Let's reverse! spread() is the inverse function, allowing you to go from long to wide format
elongation_wide <- spread(elongation_long, Year, Length)

elongation
elongation_long[elongation_long$Indiv == 530, ]


#Use elongation_long to create box plot
boxplot(Length ~ Year, data = elongation_long,
        xlab = "Year", ylab = "Elongation (cm)",
        main = "Annual growth of Empetrum hermaphroditum")
#This graph shows us that growth was lowest in all stats in 2010, and highest in 2008. However, 2008 also has the highest IQR. 