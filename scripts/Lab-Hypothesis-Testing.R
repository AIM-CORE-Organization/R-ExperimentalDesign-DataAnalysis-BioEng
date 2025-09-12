#####
# Lab Code Adopted from An Introduction To BioStatistics Using R
# BME 710 , Experimental Design and data analyis in Biomedical Engineering 
#
#####


#_____________________________________
###ONE SAMPLE TESTS OF HYPOTHESIS 
#_____________________________________

#_____________________________________
###Hypotheses Involving the Mean (µ) 
#_____________________________________


#####____________________________________________________________________________________
#EXAMPLE 6.1. A forest ecologist, studying regeneration of rainforest communities in gaps
#caused by large trees falling during storms, read that stinging tree, Dendrocnide excelsa,
#seedlings will grow 1.5 m/yr in direct sunlight in such gaps. In the gaps in her study plot
#she identified 9 specimens of this species and measured them in 2005 and again 1 year
#later. Listed below are the changes in height for the 9 specimens. Do her data support the
#published contention that seedlings of this species will average 1.5 m of growth per year in
#direct sunlight?
#  1.9 2.5 1.6 2.0 1.5 2.7 1.9 1.0 2.0
####3

data.Ex06.1 <- read.table(
  "http://waveland.com/Glover-Mitchell/Example06-1.txt",
  header = TRUE
)

data.Ex06.1

#Carry out T-test 
t.test(data.Ex06.1$Difference, mu = 1.5)
#####____________________________________________________________________________________


#####____________________________________________________________________________________
#EXAMPLE 6.2. Documents from the whaling industry of the Windward Islands in the Lesser
#Antilles indicate that in the 1920s short-finned pilot whales weighed an average of 360 kg.
#Cetologists believe that overhunting and depletion of prey species has caused the average
#weight of these whales to drop in recent years. A modern survey of this species in the same
#waters found that a random sample of 25 individuals had a mean weight of 336 kg and a
#standard deviation of 30 kg. Are pilot whales significantly lighter today than during the
#1920s?
####

source("http://waveland.com/Glover-Mitchell/t.test2.txt")
t.test2(mx = 336, sx = 30, nx = 25, mu = 360, alternative = "less")
#####____________________________________________________________________________________


#####____________________________________________________________________________________
#EXAMPLE 6.3. To test the effectiveness of a new spray for controlling rust mites in orchards,
#a researcher would like to compare the average yield for treated groves with the average
#displayed in untreated groves in previous years. A random sample of 16 one-acre groves
#was chosen and sprayed according to a recommended schedule. The average yield for
#this sample of 16 groves was 814 boxes of marketable fruit with a standard deviation of 40
#boxes. Yields from one acre groves in the same area without rust mite control spraying have
#averaged 800 boxes over the past 10 years. Do the data present evidence to indicate that the
#mean yield is sufficiently greater in sprayed groves than in unsprayed groves?
###

t.test2(mx = 814, sx = 40, nx = 16, mu = 800, alternative = "greater")

#####____________________________________________________________________________________

#_____________________________________
###Hypotheses Involving the Variance (σ2)
#_____________________________________

#####____________________________________________________________________________________
#EXAMPLE 6.5. Economy-sized boxes of cornflakes average 40 oz with a standard deviation
#of 4.0 oz. In order to improve quality control a new packaging process is developed that
#you hope will significantly decrease variability. Thirty boxes packed by the new process are
#weighed and have a standard deviation of 3.0 oz. Is there evidence that the cornflakes boxes
#are significantly more uniform when packed by the new process?
#####____________________________________________________________________________________

chi.sq <- (30-1)*9/16
chi.sq
pchisq(chi.sq, df = 29)

#####___________________________________________________________________________________
#EXAMPLE 6.6. Dugongs are large, docile marine mammals that normally occur in herds of
#about 50 animals. Because the populations are being restricted by human activity along the
#southeastern coast of Queensland the groups are now somewhat smaller and, therefore,
#thought to be more highly inbred. Inbreeding often leads to an increase in the variability
#of the size of animals among the various herds. Healthy dugong populations have a mean
#weight of 350 kg and a variance of 900 kg2. A sample of 25 Moreton Bay dugongs had
#a variance of 1600 kg2. Is the Moreton Bay population significantly more variable than
#standard populations?
#####___________________________________________________________________________________
chi.sq <- (25-1)*1600/900
chi.sq

pchisq(chi.sq, df = 24, lower.tail = FALSE)



#_____________________________________
###The One-Sample WilcoxonSigned-Rank Test
#_____________________________________

#####___________________________________________________________________________________
##EXAMPLE 6.11. The bridled goby, Arenigobius frenatus, occupies burrows beneath rocks
#and logs on silty or muddy tidal flats in Moreton Bay. Ventral fins join at the base to form
#a single disc—characteristic of the family Gobiidae. The median length is thought to be
#80 mm, and the distribution of lengths is thought to be symmetric. Suppose a small sample
#was collected and the following lengths (in mm) were recorded.
#Researchers assumed because the sample was collected at a location adjacent to a ferry
#port where there were higher than normal levels of disturbance and pollution that the bridled 
#gobies would be somewhat stunted, that is, shorter than 80 mm. Do the data collected
#support this contention?
#  63.0 82.1 81.8 77.9 80.0 72.4 69.5 75.4 80.6 77.9

#####___________________________________________________________________________________

data.Ex06.11 <- read.table(
  "http://waveland.com/Glover-Mitchell/Example06-11.txt",
  header = TRUE)
wilcox.test(data.Ex06.11$Length, mu = 80, alternative = "less", exact = FALSE)



#_____________________________________
###TESTS OF HYPOTHESIS INVOLVING TWO SAMPLES 
#_____________________________________

#_____________________________________
###Comparing Two Variances
#_____________________________________


#####___________________________________________________________________________________
#EXAMPLE 7.1. Among the few reptilian lineages to emerge from the Mesozoic extinction are
#today’s reptiles. One of these, the tuatara, Sphenodon punctatum, of New Zealand, is the sole
#survivor of a group that otherwise disappeared 100 million years ago. The mass (in g) of
#random samples of adult male tuatara from two islets in the Cook Strait are given below. Is
#the variability in mass of adult males different on the two islets?
#  A B
#510 650
#773 600
#3836 600
#505 575
#765 452
#780 320
#235 660
#790 
#440 
#435 
#815 
#460 
#690 
#####___________________________________________________________________________________

data.Ex07.1 <- read.table(
  "http://waveland.com/Glover-Mitchell/Example07-1.txt",
  header = TRUE)
data.Ex07.1
var.test(data.Ex07.1$A, data.Ex07.1$B)


#_____________________________________
###Testing the Difference Between Two Means of Independent Samples
#_____________________________________

#####___________________________________________________________________________________

#EXAMPLE 7.3. Returning to the New Zealand tuataras in Example 7.1, we now ask the
#question: Does the average mass of adult males differ between Location A and Location B?
#####___________________________________________________________________________________

t.test(data.Ex07.1$A, data.Ex07.1$B, var.equal = TRUE)

#_____________________________________
###Confidence Intervals for µ1−µ2
#_____________________________________

#####___________________________________________________________________________________

# An ornithologist studying various populations of the Australian magpie,
# Gymnorhina tibicen, mist-netted 25 adults from an isolated rural area 
# and 36 from an urban picnic area. 
# She measured the total body lengths in centimeters and reported 
# the following summary statistics:
#
#            Rural   Urban
# Mean (X̄)    38 cm   35 cm
# SD (s)        4 cm    3 cm
# Sample size n  25      36
  
#####___________________________________________________________________________________

t.test2(mx = 38, sx = 4.0, nx = 25, my = 35, sy = 3.0, ny = 36,
        alternative = "two.sided", var.equal = TRUE)

