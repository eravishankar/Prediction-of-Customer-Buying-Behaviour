library(scatterplot3d)

data<-read.csv(file.choose())
rfmdata<-data
hist(rfmdata$Value)
str(rfmdata)
rfmdata$Date = as.Date(rfmdata$Date,"%d-%m-%Y")
rfmdata$days_since       = as.numeric(difftime(time1 = "2017-02-15",
                                               time2 = rfmdata$Date,
                                               units = "days"))
summary(rfmdata)
head(rfmdata)
hist(rfmdata$days_since)
hist(rfmdata$Freq)
new_data = rfmdata[,c(1,3,4,5)]

# Remove customer id as a variable, store it as row names
head(new_data)
row.names(new_data) = new_data$Cust.id
new_data$Cust.id = NULL
head(new_data)
str(new_data)
head(new_data)

new_data$Value = log(new_data$Value)
summary(new_data)
head(new_data)
hist(new_data$Value)

new_data$Freq = log(new_data$Freq)
hist(new_data$Freq)

original_data<-new_data

# Standardize variables


new_data = scale(new_data)
head(new_data)

summary(new_data)
d = dist(new_data)
newdata1=
          # Perform hierarchical clustering on distance metrics
          c = hclust(d, method="ward.D2")

# Plot de dendogram
plot(c)

# Cut at 9 segments
members = cutree(c, k = 8)

# Show 30 first customers, frequency table
members[1:30]
table(members)

plot(members)


# Show profile of each segment
aggregate(new_data, by = list(members), mean)

new_data_final<-data.frame(original_data,members)

head(new_data_final)
new_data_final$pcolor[new_data_final$members==1] <- "red"
new_data_final$pcolor[new_data_final$members==2] <- "blue"
new_data_final$pcolor[new_data_final$members==3] <- "green3"
new_data_final$pcolor[new_data_final$members==4] <- "yellow"
new_data_final$pcolor[new_data_final$members==5] <- "black"
new_data_final$pcolor[new_data_final$members==6] <- "brown"
new_data_final$pcolor[new_data_final$members==7] <- "orange"
new_data_final$pcolor[new_data_final$members==8] <- "violet"
scatterplot3d(new_data_final$days_since, new_data_final$Freq,new_data_final$Value, pch=17,
              highlight.3d=FALSE,
              color = new_data_final$pcolor,
              main="3D Scatterplot")

