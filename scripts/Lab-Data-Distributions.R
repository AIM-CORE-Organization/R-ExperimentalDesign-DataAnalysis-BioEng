#####
# Lab Code Adopted from IPS9 in R: Looking at Data-Distribution
# BME 710 , Experimental Design and data analyis in Biomedical Engineering 
#
#####


#Load the Packages that are necessary for the documents 

library(mosaic)
library(readr)
#_________________________________________
#Displaying distributions with graphs 
#__________________________________________

#Data includes the counts of preferences for online resources of 552 first-year college students

Online <-read_csv("https://nhorton.people.amherst.edu/ips9/data/chapter01/EG01-07ONLINE.csv")

Online 

Online_data <- data.frame(Source = c(rep("Google", 406), rep("Library", 75),
                                     rep("Wikipedia", 52), rep("Other", 19)))
tally(~ Online_data, margins = TRUE)

#displaying the data using histogram, by default printing the index alphabetically 
gf_histogram(~ Source, data = Online_data, stat = "count")

#displaying the histogram using additional arguments 
gf_histogram(~ forcats::fct_infreq(Source), data = Online_data, stat = "count") %>%
  gf_labs(x = "Source")


#Representing a pie chart 
Online_Percent <-read_csv("https://nhorton.people.amherst.edu/ips9/data/chapter01/EG01-08ONLINE.csv")
Online_Percent <- Online %>%
  mutate(Count = 100 * Count/sum(Count)) %>%
  rename(Percent = Count)
Preferences <- ggplot(Online_Percent, aes(x = "", y = Percent, fill = factor(Source))) +
  geom_bar(width = 1, stat = "identity")
Preferences + coord_polar(theta="y") + labs(fill = "Source") + theme_void()

## Generate Stem and Leaf plot 
SCF <- read_csv("https://nhorton.people.amherst.edu/ips9/data/chapter01/EG01-11SCF.csv")
SCF_Treatment <- SCF %>%
  filter(Treatment == "SCF")
with(data = SCF_Treatment, stem(Absorption))

#IQ score of students choosen at random
IQ <-read_csv("https://nhorton.people.amherst.edu/ips9/data/chapter01/EG01-14IQ.csv")
levels <- c(75, 85, 95, 105, 115, 125, 135, 145, 155)
labels <- as.factor(seq(from = 80, to = 150, by = 10))
IQ_Count <- IQ %>%
  mutate(Class = cut(IQ, levels, labels = labels))
gf_bar(~ Class, data = IQ_Count)


## the lengths of the first 80 calls 
Customer_Calls <-read_csv("https://nhorton.people.amherst.edu/ips9/data/chapter01/EG01-16CALLS.csv")
head(Customer_Calls, 80)

#histogram of customer calls 
Customer_Calls %>%
  filter(length <= 1200) %>%
  gf_histogram(~ length, binwidth = 5) %>%
  gf_labs(x = "Service time (seconds)", y = "Count of calls")

#use the favstats() function to find the center and spread:
favstats(~ IQ, data = IQ)
favstats(~ length, data = Customer_Calls)


#Data on undergraduate students in colledge 
College <-read_csv("https://nhorton.people.amherst.edu/ips9/data/chapter01/EG01-19COLLEGE.csv")
gf_dhistogram(~ Undergrads, data = College)

College %>%
  filter(Undergrads == max(Undergrads))

#The UGradPerThou variable in the dataset takes into account the variation in state populations and expresses
#the number of undergraduate students per 1000 people in each state

College %>%
  filter(UGradPerThou > 76)

##parathyroid hormone levels (PTH) 
PTH <- read_csv("https://nhorton.people.amherst.edu/ips9/data/chapter01/EG01-21PTH.csv")
with(PTH, stem(PTH))

#e serum levels of vitamin have been plotted against time of year for samples of subjects from Switzerland
VITDS <-read_csv("https://nhorton.people.amherst.edu/ips9/data/chapter01/EG01-22VITDS.csv")
gf_point(VitaminD ~ Months, data = VITDS) %>%
  gf_smooth(se = FALSE) %>%
  gf_labs(x = "Months", y= "Vitamin D(nmol/1)")

#_________________________________________
#Describing Distributions with Numbers 
#_________________________________________

# display the data collected on the time, in days, that businesses took to complete all of the starting procedures
TTS24 <- read_csv("https://nhorton.people.amherst.edu/ips9/data/chapter01/EG01-23TTS24.csv")
with(TTS24, stem(Time, scale = 2))
median(~ Time, data = TTS24)
mean(~ Time, data = TTS24)

#side-by-side boxplots for age of the death of writers in the three categories.
POETS <- read_csv("https://nhorton.people.amherst.edu/ips9/data/chapter01/EG01-31POETS.csv")
gf_boxplot(Age ~ Type, data = POETS)

#distribution of the time it takes to start a business, as a density plot overlain a histogram
TTS <- read_csv("https://nhorton.people.amherst.edu/ips9/data/chapter01/EG01-36TITANIC.csv")
gf_histogram( ~ Time, data = TTS, binwidth = 20, center = 10) %>%
  gf_dens()

#__________________________________________
#Density Curves and Normal Distributions
#__________________________________________

#Normal quantile plot of the 60 fifth-grade IQ
IQ <- read_csv("https://nhorton.people.amherst.edu/ips9/data/chapter01/EG01-46IQ.csv")
gf_qq(~ IQ, data = IQ) %>%
  gf_qqline(linetype = "solid", color = "red") %>%
  gf_labs(x = "Normal Score", y = "IQ")

