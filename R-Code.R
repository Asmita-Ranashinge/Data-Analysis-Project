# Importing merged data
Data1 = read.csv('C:/Big_Data/Safety-Equipment-Severity.csv', header=TRUE)
Data1 <- Data1[c("Year", "Age", "Sex", "Safety.Equipment", "Injury.Severity")]



Data1$Age <- cut(Data1$Age, breaks=c(17, 25, 31, 51, 71, 95))

Data1$Age = factor(Data1$Age, ordered = FALSE,
                   levels = c('(71,95]','(51,71]', '(31,51]', '(25,31]', '(17,25]'),
                   labels = c(4,3,2,1,0))
Data1$Sex = factor(Data1$Sex, ordered = FALSE,
                   levels = c('0','1'))
Data1 <- Data1[!(is.na(Data1$Age) | Data1$Age=="NA"), ];

Data1.sample <- Data1[sample(nrow(Data1),1000),]

Data1.sample.features = Data1.sample

set.seed(20)
results <- kmeans(Data1.sample.features[,5:4], 5)
Data1.sample$clusterValue <- as.factor(results$cluster)
str(results)
library(ggmap)
geom_point(aes(x= Injury.Severity, y= Safetly.Equipment, colour= as.factor(results)), data=Data1.sample)
ggtitle("NYC Vehicle Crashes using KMean")
results$centers
results$size
results$cluster
plot(Data1.sample[c("Injury.Severity", "Safety.Equipment")], col= results$cluster)

ggplot(Data1.sample, aes(Safety.Equipment, Injury.Severity, color = Age)) + geom_point()
plot(Data1.sample[results$cluster ==1,],
     col= "red")
points(Data1.sample [results$cluster == 2, ],
       col = "blue")
points(Data1.sample [results$cluster == 3, ],
       col = "seagreen")
points(Data1.sample [results$cluster == 4, ],
       col = "yellow")
points(Data1.sample [results$cluster == 5, ],
       col = "pink")
points(results$cluster, pch=2, col = "green")
library(cluster.datasets)
data(Data1.sample)
summary(Data1.sample)
summary(lm(year ~ Age, data = Data1.sample))
plot(Year-Age, data=Data1.sample)

reg = lm(Year ~ Sex + Age, data=Data1.sample)
summary (reg)

regression<-data.frame(NOMVAR=names(coefficients(reg)),
                      + COEF=as.numeric(coefficients(reg)))
merge(names,regression,all.x=TRUE)
Data1 <- Data1[c("Year", "Sex", "Age", "Safety.Equipment", "Injury.Severity")]
Data1.sample <- Data1.sample[c("Year", "Sex", "Age", "Safety.Equipment", "Injury.Severity")]
View(Data1)
write.csv(Data1.sample, "C:/Big_Data/Data1.sample.csv");
setwd("C:/Big_Data")
getwd()

Data1 = read.csv('C:/Big_Data/Data1.csv')

library("ggplot2")
library("ggpubr")
ggscatter(Data1.sample, x = "Age", y = "Safety.Equipment", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Safety.Equipment")


Data2 = read.csv('C:/Big_Data/crashes_information.csv', header=TRUE)
Data2$Year[Data2$Year=="2014"] <- "2013";
Data2$Year[Data2$Year=="2015"] <- "2014";
Data2$Year[Data2$Year=="2016"] <- "2015"

Data2$Year <- NULL
Data2$Police.Report <- NULL
Data2$Municipality <- NULL
Data2$Collision.Type.Descriptor <- NULL
Data2$Road.Descriptor <- NULL
Data2$Traffic.Control.Device <- NULL
Data2$Road.Surface.Conditions <- NULL
Data2$DOT.Reference.Marker.Location <- NULL
Data2$Pedestrian.Bicyclist.Action <- NULL
Data2$Event.Descriptor <- NULL
Data2$Number.of.Vehicles.Involved <- NULL

Data2 <- Data2[!(is.na(Data2$Year) | Data2$Year=="" | Data2$Year=="Unknown"), ];
Data2 <- Data2[!(is.na(Data2$Crash.Descriptor) | Data2$Crash.Descriptor=="" | Data2$Crash.Descriptor=="Unknown"), ];
Data2 <- Data2[!(is.na(Data2$Time) | Data2$Time=="" | Data2$Time=="Unknown"), ];
Data2 <- Data2[!(is.na(Data2$Date) | Data2$Date=="" | Data2$Date=="Unknown"), ];
Data2 <- Data2[!(is.na(Data2$Day.of.Week) | Data2$Day.of.Week=="" | Data2$Day.of.Week=="Unknown"), ];
Data2 <- Data2[!(is.na(Data2$Lighting.Conditions) | Data2$Lighting.Conditions=="" | Data2$Lighting.Conditions=="Unknown"), ];
Data2 <- Data2[!(is.na(Data2$County.Name) | Data2$County.Name=="" | Data2$County.Name=="Unknown"), ];
Data2 <- Data2[!(is.na(Data2$Weather.Conditions) | Data2$Weather.Conditions=="" | Data2$Weather.Conditions=="Unknown"), ];


Data1 <- Data1[1:825441,]
library(dplyr)
data14 <- bind_cols(Data1, Data2)
write.csv(data14, "C:/Big_Data/data14.csv")

data14.sample <- data14[sample(nrow(data14),20000),]
write.csv(data14.sample, "C:/Big_Data/data14.sample.csv")