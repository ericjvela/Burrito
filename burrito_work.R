# Burritos Dataset
library(ggplot2)
# Replicate random sampling
set.seed(542)
setwd("C:/Users/ericv/Documents/GitHub/burrito")

# Read in csv
bur <- read.csv("burrito_psample.csv", na = "")

# Drop odd column from csv
bur <- bur[-c(1)]

# Create table of burritos
table(bur$Burrito)

# Sum the number of records (75)
num_of_burritos <- sum(table(bur$Burrito))

# Plot burrito type
barplot(table(bur$Burrito),
        ylim = c(0,45),
        main = "Counts per Burrito",
        col = terrain.colors(5))
ggplot(bur, aes(Burrito)) + geom_bar() + ggtitle("Counts per Burrito")

# Create a table of neighborhoods
table(bur$Neighborhood)

# Plot neighborhood
barplot(sort(table(bur$Neighborhood)),
        las = 2,
        ylim = c(0,11),
        main = "Burritos Per Neighborhood",
        col = topo.colors(25))

ggplot(bur, aes(Neighborhood)) + geom_bar() + ggtitle("Burritos per Neighborhood") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
table(bur$Neighborhood)

# Plot region
ggplot(bur, aes(Region)) + geom_bar() + ggtitle("Burritos per Neighborhood") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Plot neighborhood
barplot(table(bur$Region),
        las = 2,
        ylim = c(0,50),
        main = "Burritos Per Region",
        col = topo.colors(3))
# Boxplot of Overall score
boxplot(bur$Overall)

table(bur$Wrap)
plot(bur$Wrap, bur$Overall)
cor(bur$Wrap, bur$Overall)

count()