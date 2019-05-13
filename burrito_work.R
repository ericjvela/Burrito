# Burritos Dataset
library(ggplot2)
library(lattice)
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
histogram(bur$Burrito, xlab = "Burrito", scales = list(x=list(rot=45)), main = "Percent of Total per Burrito")

# Create a table of neighborhoods
table(bur$Neighborhood)

# Plot neighborhood
histogram(bur$Neighborhood, xlab = "Neighborhood", ylim = c(0,16), scales = list(x=list(rot=45)), main = "Percent of Total per Neighborhood")

histogram(bur$Circum, nint = 5, endpoint = c(17, 27))

barplot(sort(table(bur$Neighborhood)),
        las = 2,
        ylim = c(0,11),
        main = "Burritos Per Neighborhood",
        col = topo.colors(25))

bur$Circum[which(bur$Burrito == "California")]

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


table(bur$Wrap)
plot(bur$Wrap, bur$Overall, main = "Wrap vs Overall", xlab = "Wrap Quality", ylab = "Overall Quality")
abline(lm(bur$Overall ~ bur$Wrap), lwd = 2, col = "red")
cor(bur$Wrap, bur$Overall)

model <- summary(lm(bur$Overall ~ bur$Wrap))
sd(model$residuals)

a <- boxplot(bur$Overall ~ bur$Region, xlab = "Region", ylab = "Overall Score", main = "Overall Score Per Region")
a$stats 
table(bur$Synergy)
comp <- data.frame("Bins" = c("0-1", "1-2", "2-3", "3-4", "4-5"),
                   "SynergyFreq" = c(0, 1, 9, 28, 37),
                   "SynergyRfreq" = c(0/75, 1/75, 9/75, 28/75, 37/75),
                   "UniformityFreq" = c(0, 6, 17, 21, 31),
                   "UniformityRfreq" = c(0/75, 6/75, 17/75, 21/75, 31/75)
                   )

barplot(comp$SynergyRfreq, names.arg=comp$Bins, ylim = c(0,.50), xlab = "Ranges", ylab = "Relative Frequency", main = "Relative Frequency Bar Graph for Synergy")
barplot(comp$UniformityRfreq, names.arg=comp$Bins, ylim = c(0,.50), xlab = "Ranges", ylab = "Relative Frequency", main = "Relative Frequency Bar Graph for Uniformity")

# California burritos per region
histogram(bur$Region[which(bur$Burrito == "California")],
          xlab = "Region",
          main = "Relative Frequency of California Burritos per Region")
table(bur$Region[which(bur$Burrito == "California")])

prop.table(bur[bur$Burrito == "California",]$Region)
# Synergy Boxplot
boxplot(bur$Synergy ~ bur$Burrito)

boxplot(bur$Circum ~ bur$Burrito)



boxplot(bur$Region~bur$Burrito)

prop.table(table(bur$Burrito, bur$Cost),1)



# Volume
df <- aggregate(bur$Volume, by = list("Burrito" = bur$Burrito), FUN=function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
df <- do.call(data.frame, df) 
colnames(df) <- c("Burrito", "Mean", "SD", "n")
df$SE <- df$SD/sqrt(df$n)

ggplot(df, aes(x=Burrito, y=Mean, fill=Burrito)) +
  geom_bar(position=position_dodge(0.5), stat="identity",
           width = 0.5)+ 
  geom_errorbar(aes(ymin=Mean-2*SE, ymax=Mean+2*SE),
                size=0.5, width=0.2,
                position = position_dodge(0.5))+
  theme_classic()+
  # #geom_text(aes(x=Source, y=Mean+2*SE+1.75,
  #               label= out.aov.sodium$groups$groups),
  #           position = position_dodge(width = 0.9), size=4)+
  ylim(0,1)+
  ggtitle("Mean Volume per Burrito") +
  ylab("Volume (L)")+
  theme(
    plot.title = element_text(hjust = 0.5))

# Synergy
df2 <- aggregate(bur$Synergy, by = list("Burrito" = bur$Burrito), FUN=function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
df2 <- do.call(data.frame, df2) 
colnames(df2) <- c("Burrito", "Mean", "SD", "n")
df2$SE <- df2$SD/sqrt(df2$n)

ggplot(df2, aes(x=Burrito, y=Mean, fill=Burrito)) +
  geom_bar(position=position_dodge(0.5), stat="identity",
           width = 0.5)+ 
  geom_errorbar(aes(ymin=Mean-2*SE, ymax=Mean+2*SE),
                size=0.5, width=0.2,
                position = position_dodge(0.5))+
  theme_classic()+
  # #geom_text(aes(x=Source, y=Mean+2*SE+1.75,
  #               label= out.aov.sodium$groups$groups),
  #           position = position_dodge(width = 0.9), size=4)+
  ylim(0,5)+
  ggtitle("Mean Synergy per Burrito") +
  ylab("Rating (1-5)")+
  theme(
    plot.title = element_text(hjust = 0.5))

# Uniformity
df3 <- aggregate(bur$Uniformity, by = list("Burrito" = bur$Burrito), FUN=function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
df3 <- do.call(data.frame, df3) 
colnames(df3) <- c("Burrito", "Mean", "SD", "n")
df3$SE <- df3$SD/sqrt(df3$n)

ggplot(df3, aes(x=Burrito, y=Mean, fill=Burrito)) +
  geom_bar(position=position_dodge(0.5), stat="identity",
           width = 0.5)+ 
  geom_errorbar(aes(ymin=Mean-2*SE, ymax=Mean+2*SE),
                size=0.5, width=0.2,
                position = position_dodge(0.5))+
  theme_classic()+
  # #geom_text(aes(x=Source, y=Mean+2*SE+1.75,
  #               label= out.aov.sodium$groups$groups),
  #           position = position_dodge(width = 0.9), size=4)+
  ylim(0,5)+
  ggtitle("Mean Uniformity per Burrito") +
  ylab("Rating (1-5)")+
  theme(
    plot.title = element_text(hjust = 0.5))

plot(bur$Synergy, bur$Overall, main = "Synergy vs Overall", xlab = "Synergy Score", ylab = "Overall Quality")
abline(lm(bur$Overall ~ bur$Synergy), lwd = 2, col = "red")
fit_cost <- lm(bur$Overall ~ bur$Synergy)
summary(fit_cost)
fit_cost
sd(fit_cost$residuals)

# Meat mean bar graphs
meat <- aggregate(bur$Meat, by = list("Burrito" = bur$Burrito), FUN=function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
meat <- do.call(data.frame, meat) 
colnames(meat) <- c("Burrito", "Mean", "SD", "n")
meat$SE <- meat$SD/sqrt(meat$n)

ggplot(meat, aes(x=Burrito, y=Mean, fill=Burrito)) +
  geom_bar(position=position_dodge(0.5), stat="identity",
           width = 0.5)+ 
  geom_errorbar(aes(ymin=Mean-2*SE, ymax=Mean+2*SE),
                size=0.5, width=0.2,
                position = position_dodge(0.5))+
  theme_classic()+
  # #geom_text(aes(x=Source, y=Mean+2*SE+1.75,
  #               label= out.aov.sodium$groups$groups),
  #           position = position_dodge(width = 0.9), size=4)+
  ylim(0,5)+
  ggtitle("Mean Meat Quality per Burrito") +
  ylab("Rating (1-5)")+
  theme(
    plot.title = element_text(hjust = 0.5))


# Correlation matrix
cormat <- cor(subset(bur, select = -c(Burrito, Neighborhood, Region)))
cormat[,13]
# Best model for the data set
all <- lm(Overall ~ Synergy + Meat + Fillings + Uniformity,data = bur)
all
summary(all)

# Meat mean bar graphs
cost <- aggregate(bur$Cost, by = list("Region" = bur$Region), FUN=function(x) c(mean=mean(x),sd=sd(x),n=length(x)))
cost <- do.call(data.frame, cost) 
colnames(cost) <- c("Region", "Mean", "SD", "n")
cost$SE <- cost$SD/sqrt(cost$n)

ggplot(cost, aes(x=Region, y=Mean, fill=Region)) +
  geom_bar(position=position_dodge(0.5), stat="identity",
           width = 0.5)

ggplot(cost, aes(x=Region, y=Mean, fill=Region)) +
  geom_bar(position=position_dodge(0.5), stat="identity",
           width = 0.5)+ 
  geom_errorbar(aes(ymin=Mean-2*SE, ymax=Mean+2*SE),
                size=0.5, width=0.2,
                position = position_dodge(0.5))+
  theme_classic()+
  # #geom_text(aes(x=Source, y=Mean+2*SE+1.75,
  #               label= out.aov.sodium$groups$groups),
  #           position = position_dodge(width = 0.9), size=4)+
  ggtitle("Mean Cost per Burrito") +
  ylab("Mean Cost")+
  theme(
    plot.title = element_text(hjust = 0.5))

# boxplot for meat and fillings and wrap
boxplot(bur$Meat ~ bur$Burrito, las = 2, main = "Meat Score")
boxplot(bur$Fillings ~ bur$Burrito, las = 2, main = "Fillings")
boxplot(bur$Wrap ~ bur$Burrito, las = 2, main = "Wrap")



table(bur$Synergy, bur$Uniformity)
# create a column 
breaks <- seq(0,6, length = 7)
synergy_cut <- cut(bur$Synergy, breaks, right = FALSE)
uniformity_cut <- cut(bur$Uniformity, breaks, right = FALSE)
xtab <- table(synergy_cut, uniformity_cut)


barplot(prop.table(t(xtab),2), xlab = "Synergy", names.arg = row.names(xtab))
typeof(rownames(xtab))
