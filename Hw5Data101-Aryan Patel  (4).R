# Hypothesis 1 : 
# Create Subsets for Samples being tested 

Male_Riders <- subset(citibike.May20.50K, citibike.May20.50K$gender == "1")
Female_Riders <- subset(citibike.May20.50K, citibike.May20.50K$gender == "2")


# Make trip duration in minutes, so it is easier to work with 
citibike.May20.50K$Trip_Duration_in_Minutes <- as.double(citibike.May20.50K$tripduration/60)

# Focusing on Trip Duration 
# Case 1 Alt Hypothesis: Female Riders have a higher average trip duration than Males. 
# Case 1 Null Hypothesis : Female and Male riders on average have the same trip duration. 
Male_Riders1 <- sample(Male_Riders$Trip_Duration_in_Minutes,10000)
Female_Riders2 <- sample(Female_Riders$Trip_Duration_in_Minutes,10000)

head(Female_Riders2)
head(Male_Riders1)


#Standard Deviation of Two Samples 
sd.Male_Riders1 <- sd(Male_Riders1)
sd.Female_Riders2 <- sd(Female_Riders2)

sd.Male_Riders1
sd.Female_Riders2

#Length 

Len_Male_Riders1 <- length(Male_Riders1)
Len_Female_Riders2 <-length(Female_Riders2)

Len_Male_Riders1
Len_Female_Riders2

#standard deviation of difference traffic
sd.Male_Female <- sqrt(sd.Female_Riders2^2/Len_Female_Riders2 + sd.Male_Riders1^2/Len_Male_Riders1)


#Means of Samples 

Mean.Male <- mean(Male_Riders1)
Mean.Female <- mean(Female_Riders2)


# Z-Score 

Zeta <- (Mean.Female-Mean.Male)/sd.Male_Female


# Get P 
P_Value = 1-pnorm(Zeta)

Plot1 <- plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=Zeta, col='red')

# Conclusion : P Value = 0.1554 > 0.05 therefore, we fail to reject the null hypothesis and can conclude that both Female and Male Riders on Average have the same trip duration. 

###############################################################################################################################################################

# Hypothesis 2 : 
# Null - On average both Customer and Subscriber type riders have the same trip duration. 
# Alternate - On average Customer type riders  have higher average trip duration than Subscriber type riders. 

Subscriber_Riders <- subset(citibike.May20.50K, citibike.May20.50K$usertype == "Subscriber")
Customer_Riders <- subset(citibike.May20.50K, citibike.May20.50K$usertype == "Customer")

#Take Sample of Data 

Subscriber_Riders1 <- sample(Subscriber_Riders$Trip_Duration_in_Minutes, 90)
Customer_Riders2 <- sample(Customer_Riders$Trip_Duration_in_Minutes, 90)
#Quick Check 
head(Subscriber_Riders1)
head(Customer_Riders2)

# Find the mean 
Mean.Subscriber <- mean(Subscriber_Riders1)
Mean.Customer <- mean(Customer_Riders2)
#Quick View to ensure no data errors
plot(Customer_Riders2)
plot(Subscriber_Riders1)

#Find Length 

Len_Customer <- length(Customer_Riders2)
Len_Subscriber <- length(Subscriber_Riders1)

# Find S.D
sd.Customer <- sd(Customer_Riders2)
sd.Subscriber <- sd(Subscriber_Riders1)

#Standard Deviation of Difference Traffic 

sd.Customer_Subscriber <- sqrt(sd.Customer^2/Len_Customer + sd.Subscriber^2/Len_Subscriber)

# Z score 

Zeta <- (Mean.Customer - Mean.Subscriber)/sd.Customer_Subscriber

#P Value 

P <- 1-pnorm(Zeta)
P
Plot1 <- plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=Zeta, col='red')
# Conclusion :  P Value = 3.79e-06 < 0.05, therefore, we reject the null hypothesis and can conlude that Customer rider types on average have a higher Subscriber ride type.  