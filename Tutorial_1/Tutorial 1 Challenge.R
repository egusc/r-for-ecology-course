Sparrows <- filter(bird_wingpans, bird_sp == "sparrow")
sparrowMean <- mean(Sparrows$wingspan)

Kingfishers <- filter(bird_wingpans, bird_sp == "kingfisher")
kingfisherMean <- mean(Kingfishers$wingspan)

Eagles <- filter(bird_wingpans, bird_sp == "eagle")
eagleMean <- mean(Eagles$wingspan)

Hummingbirds <- filter(bird_wingpans, bird_sp == "hummingbird")
hummingbirdMean <- mean(Hummingbirds$wingspan)

birds <- c(
      "Sparrows",
      "Kingfishers",
      "Eagles",
      "Hummingbirds"
)

meanWingspans <- c(sparrowMean, kingfisherMean, eagleMean, hummingbirdMean)
names(meanWingspans) <- birds
png("barplotChallenge.png", width=1600, height=600)  # look up the help for this function: you can customise the size and resolution of the image
barplot(meanWingspans, xlab="Bird", ylab="Mean Wingspan", ylim=c(0,200), cex.names= 1.5, cex.axis=1.5, cex.lab=1.5)
dev.off()