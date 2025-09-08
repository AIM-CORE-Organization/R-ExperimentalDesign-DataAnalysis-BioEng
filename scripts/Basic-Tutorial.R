#importing the data
acs <- read.csv(url("http://stat511.cwick.co.nz/homeworks/acs_or.csv"))

#visualizing the acs data 
View(acs)

#accessing a particular columns
acs$age_husband

#accessing a particular data
acs[1,3]

#perfomring queries on data where age_husband > age_wife
a <- subset(acs,age_husband > age_wife)
View(a)

#getting statistical averages

mean(acs$age_husband)
median(acs$age_husband)
quantile(acs$age_wife)
var(acs$age_wife)
sd(acs$age_wife)

summary(acs)

#Plotting the data
plot(x = acs$age_husband, y=acs$age_wife,type='p')

hist(acs$number_children)

#bar plots
counts <- table(acs$bedrooms)
barplot(counts,main="Bedrooms Distibution",xlab="Number of Bedrooms")
