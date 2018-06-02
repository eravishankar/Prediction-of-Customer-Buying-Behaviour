


snssegment_1<-read.csv(file.choose())
snsdata<-snssegment_1
colnames(snsdata) = c('customer_id', 'purchase_amount', 'date_of_purchase')
snsdata$date_of_purchase = as.Date(snsdata$date_of_purchase, "%d-%m-%Y")
snsdata$year_of_purchase = as.numeric(format(snsdata$date_of_purchase, "%Y"))
snsdata$days_since       = as.numeric(difftime(time1 = "2017-01-01",
                                               time2 = snsdata$date_of_purchase,
                                               units = "days"))
snsdata<-filter(snsdata,snsdata$purchase_amount>0)
########################2016 Segmentation###########################################3
snsdata
library(sqldf)
customers_2016 = sqldf("SELECT customer_id,
                       MIN(days_since) AS 'recency',
                       MAX(days_since) AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount',MAX(purchase_amount) AS 'max_amount'
                       
                       FROM snsdata GROUP BY 1")
head(customers_2016)
library(dplyr)

sns_customers_2016<-filter(customers_2016,amount>0)

summary(sns_customers_2016)
hist(sns_customers_2016)

# customers_2015$segment = ifelse(test = customers_2015$recency > 365*3,
#                                 yes = "inactive",
#                                 no = ifelse(test = customers_2015$recency > 365*2,
#                                             yes = "cold",
#                                             no = "NA"))
# table(customers_2015$segment)
# aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)
sns_customers_2016$segment<-"NA"
sns_customers_2016$segment<-ifelse(test=sns_customers_2016$recency>365*2,yes="inactive",no=ifelse(test=sns_customers_2016$recency>365*1,yes="cold",no="NA"))
summary(sns_customers_2016)


aggregate(x = sns_customers_2016[, 2:5], by = list(sns_customers_2016$segment), mean)

sns_customers_2016$segment[which(sns_customers_2016$recency>365*2)]="Inactive"
sns_customers_2016$segment[which(sns_customers_2016$recency<=365*2 & sns_customers_2016$recency>365)]="Cold"
sns_customers_2016$segment[which(sns_customers_2016$recency<=365 & sns_customers_2016$recency>365/2)]="Warm"
sns_customers_2016$segment[which(sns_customers_2016$recency<365/2)]="Active"
sns_customers_2016$segment[which(sns_customers_2016$segment == "Warm" & sns_customers_2016$amount < 3000)] = "Warm low value"
sns_customers_2016$segment[which(sns_customers_2016$segment == "Warm" & sns_customers_2016$amount >= 3000)] = "Warm high value"
sns_customers_2016$segment[which(sns_customers_2016$segment == "Active" & sns_customers_2016$first_purchase <= 365/3)] = "New active"
sns_customers_2016$segment[which(sns_customers_2016$segment == "Active" & sns_customers_2016$amount < 3000)] = "Active low value"
sns_customers_2016$segment[which(sns_customers_2016$segment == "Active" & sns_customers_2016$amount >= 3000)] = "Active high value"

sns_customers_2016$segment = factor(x = sns_customers_2016$segment, levels = c("Inactive", "Cold",
                                                                               "Warm high value", "Warm low value", 
                                                                               "Active high value", "Active low value", "New active"))

table(sns_customers_2016$segment)
pie(table(sns_customers_2016$segment), col = rainbow(24),main = "2016")
aggregate(x = sns_customers_2016[, 2:5], by = list(sns_customers_2016$segment), mean)

#######################################2015########################################33
customers_2015 = sqldf("SELECT customer_id,
                       MIN(days_since) - 365 AS 'recency',
                       MAX(days_since) - 365 AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount',MAX(purchase_amount) AS 'max_amount'
                       FROM snsdata
                       WHERE days_since > 365
                       GROUP BY 1")

sns_customers_2015<-filter(customers_2015,amount>0)

sns_customers_2015$segment="NA"
sns_customers_2015$segment[which(sns_customers_2015$recency>365*1.5)]="Inactive"
sns_customers_2015$segment[which(sns_customers_2015$recency<=365*1.5 & sns_customers_2015$recency>365)]="Cold"
sns_customers_2015$segment[which(sns_customers_2015$recency<=365 & sns_customers_2015$recency>365/2)]="Warm"
sns_customers_2015$segment[which(sns_customers_2015$recency<365/2)]="Active"
sns_customers_2015$segment[which(sns_customers_2015$segment == "Warm" & sns_customers_2015$amount < 3000)] = "Warm low value"
sns_customers_2015$segment[which(sns_customers_2015$segment == "Warm" & sns_customers_2015$amount >= 3000)] = "Warm high value"
sns_customers_2015$segment[which(sns_customers_2015$segment == "Active" & sns_customers_2015$first_purchase <= 365/3)] = "New active"
sns_customers_2015$segment[which(sns_customers_2015$segment == "Active" & sns_customers_2015$amount < 3000)] = "Active low value"
sns_customers_2015$segment[which(sns_customers_2015$segment == "Active" & sns_customers_2015$amount >= 3000)] = "Active high value"

sns_customers_2015$segment = factor(x = sns_customers_2015$segment, levels = c("Inactive", "Cold",
                                                                               "Warm high value", "Warm low value", 
                                                                               "Active high value", "Active low value", "New active"))
table(sns_customers_2015$segment)
pie(table(sns_customers_2015$segment), col = rainbow(24),main = "2015")
aggregate(x = sns_customers_2015[, 2:5], by = list(sns_customers_2015$segment), mean)

###############################################2014########################################
customers_2014 = sqldf("SELECT customer_id,
                       MIN(days_since) - 730 AS 'recency',
                       MAX(days_since) - 730 AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount',
                       MAX(purchase_amount) AS 'max_amount'
                       FROM snsdata
                       WHERE days_since > 730
                       GROUP BY 1")
###################No Inactive segment################################33
sns_customers_2014<-filter(customers_2014,amount>0)
sns_customers_2014$segment="NA"
sns_customers_2014$segment[which(sns_customers_2014$recency>365/2)]="Cold"
sns_customers_2014$segment[which(sns_customers_2014$recency<=365/2 & sns_customers_2014$recency>365/4)]="Warm"
sns_customers_2014$segment[which(sns_customers_2014$recency<365/4)]="Active"
sns_customers_2014$segment[which(sns_customers_2014$segment == "Warm" & sns_customers_2014$amount < 1500)] = "Warm low value"
sns_customers_2014$segment[which(sns_customers_2014$segment == "Warm" & sns_customers_2014$amount >= 1500)] = "Warm high value"
sns_customers_2014$segment[which(sns_customers_2014$segment == "Active" & sns_customers_2014$first_purchase <= 365/3)] = "New active"
sns_customers_2014$segment[which(sns_customers_2014$segment == "Active" & sns_customers_2014$amount < 1500)] = "Active low value"
sns_customers_2014$segment[which(sns_customers_2014$segment == "Active" & sns_customers_2014$amount >= 1500)] = "Active high value"


table(sns_customers_2014$segment)
pie(table(sns_customers_2014$segment), col = rainbow(24),main = "2014")
aggregate(x = sns_customers_2014[, 2:5], by = list(sns_customers_2014$segment), mean)

sns_customers_2015$segment = factor(x = sns_customers_2015$segment, levels = c("Cold",
                                                                               "Warm high value", "Warm low value", 
                                                                               "Active high value", "Active low value", "New active"))

###################################Revenue################################################
revenue_2016 = sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2016'
                     FROM snsdata
                     WHERE year_of_purchase = 2016
                     GROUP BY 1")
summary(revenue_2016)




actual = merge(sns_customers_2016, revenue_2016, all.x = TRUE)
actual$revenue_2016[is.na(actual$revenue_2016)] = 0

b<-aggregate(x = actual$revenue_2016, by = list(sns_customers_2016$segment), mean)

forward = merge(sns_customers_2015, revenue_2016, all.x = TRUE)
forward$revenue_2016[is.na(forward$revenue_2016)] = 0


r<-aggregate(x = forward$revenue_2016, by = list(sns_customers_2015$segment), mean)


r = r[order(r$x, decreasing = FALSE), ]
print(r)
barplot(r$x, names.arg = r$Group.1)




#################################Prediction##########################
# Merge 2015 customers and 2016 revenue

in_sample = merge(customers_2015, revenue_2016, all.x = TRUE)
colnames(in_sample)[5]="avg_amount"

in_sample$revenue_2016[is.na(in_sample$revenue_2016)] = 0
in_sample$active_2016 = as.numeric(in_sample$revenue_2016 > 0)


# Calibrate probability model
library(nnet)
prob.model = multinom(formula = active_2016 ~ recency + first_purchase + frequency + avg_amount + max_amount,
                      data = in_sample)
coef = summary(prob.model)$coefficients
std  = summary(prob.model)$standard.errors
print(coef)

print(std)
print(coef / std) #Coef/std>2 or Coeff <-2 --- Good
###### Write in report that the first purchase is very important in grocery because the customers who buy high first are generally bulk/ monthly purchasers who buy evry month rather than a top up


z = which(in_sample$active_2016 == 1)
head(in_sample[z, ])
summary(in_sample[z, ])


active_cust_2016<-filter(in_sample,active_2016==1)
head(active_cust_2016)

amount.model = lm(formula = log(revenue_2016) ~ log(avg_amount) + log(max_amount), data = active_cust_2016)
summary(amount.model)


amount.model1 = lm(formula = revenue_2016 ~ avg_amount + max_amount, data = active_cust_2016)
summary(amount.model1)


plot(x = log(active_cust_2016$revenue_2016), y = amount.model$fitted.values)

plot(x = active_cust_2016$revenue_2016, y = amount.model1$fitted.values)

colnames(customers_2016)[5]="avg_amount"


# Predict the target variables based on today's data
customers_2016$prob_predicted    =round(predict(object = prob.model, newdata = customers_2016, type = "probs"),2)



customers_2016$revenue_predicted = exp(predict(object = amount.model, newdata = customers_2016))
customers_2016$score_predicted   = customers_2016$prob_predicted * customers_2016$revenue_predicted
summary(customers_2016$prob_predicted)
summary(customers_2016$revenue_predicted)
summary(customers_2016$score_predicted)
hist(customers_2016$score_predicted)

z1 = which(customers_2016$score_predicted > 1500)
print(length(z1))
z1
##################THese people who has the high score- both probability of buying and buying value..... these people are the ones whom we have to target by spending your marketing dollars


# --- COMPUTE TRANSITION MATRIX ----------------------------


# Compute transition matrix
new_data = merge(x = sns_customers_2015, y = sns_customers_2016, by = "customer_id", all.x = TRUE)
head(new_data)
transition = table(new_data$segment.x, new_data$segment.y)
print(transition)

transition<-transition/rowSums(transition)



########## to make predictions#############3

segments<-matrix(nrow = 7,ncol = 11)
segments[,1]=table(sns_customers_2016$segment)
segments
colnames(segments)<-2016:2026
row.names(segments)<-levels(sns_customers_2016$segment)
for (i in 2:11) {
          segments[, i] = segments[, i-1] %*% transition
}
segments

barplot(segments[2,])
print(round(segments))
yearly_revenue<-b$x

revenue_per_segment<-yearly_revenue*segments
yearly_revenue1<-colSums(revenue_per_segment)

cumulative_sum<-cumsum(yearly_revenue1)
discount_rate = 0.10
discount = 1 / ((1 + discount_rate) ^ ((1:11) - 1))
print(discount)

# Compute discounted yearly revenue
disc_yearly_revenue = yearly_revenue1 * discount
print(round(disc_yearly_revenue))
barplot(disc_yearly_revenue)
lines(yearly_revenue)

# Compute discounted cumulated revenue
disc_cumulated_revenue = cumsum(disc_yearly_revenue)
print(round(disc_cumulated_revenue))
barplot(disc_cumulated_revenue)

# What is the database worth?
print(disc_cumulated_revenue[11] - yearly_revenue[1])

