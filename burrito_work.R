# Burritos Dataset

# Replicate random sampling
set.seed(542)
setwd("C:/Users/ericv/OneDrive/Documents/MATH 303")

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

# Create a table of neighborhoods
table(bur$Neighborhood)

# Plot neighborhood
barplot(sort(table(bur$Neighborhood)),
        las = 2,
        ylim = c(0,11),
        main = "Burritos Per Neighborhood",
        col = topo.colors(25))

# Boxplot of Overall score
boxplot(bur$Overall)

