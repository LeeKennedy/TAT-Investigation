# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(lubridate)
# Data Input -------------------------------------------------------------
data.in <- read_excel("~/Desktop/June_2017.xlsx")
data.in <- data.in[,-5]

dept <- read_excel("~/Desktop/analyses.xlsx")

# Data Cleaning ----------------------------------------------------------

data.in <- data.in %>% 
  filter(STATUS == "A") %>% 
  filter(LAB == "CHEMISTRY")

colnames(data.in)[3] <- "LOGGED"
colnames(data.in)[4] <- "RECEIVED"
colnames(data.in)[7] <- "STARTED"
colnames(data.in)[8] <- "RELEASED"
colnames(data.in)[9] <- "CHANGED"
colnames(data.in)[10] <- "REVIEWED"

data.in$hour <- hour(data.in$RELEASED)


t0 <- sapply(data.in[,4], as.numeric)/(3600*24)
w <- sapply(data.in[,7], as.numeric)/(3600*24)
x <- sapply(data.in[,8], as.numeric)/(3600*24)
y <- sapply(data.in[,9], as.numeric)/(3600*24)
z <- sapply(data.in[,10], as.numeric)/(3600*24)
set <- as.data.frame(cbind(t0, w, x, y, z))

set[2:5] <- set[2:5]-set[,1]
set <- set[,2:5]
set <- cbind(data.in[,c(1,5, 15)],set)
set <- set %>% 
  mutate(altered = CHANGED - REVIEWED) %>% 
  filter(altered < 1) 

new_set <- merge(set, dept, by.x="ANALYSIS", by.y = "Test_Code", all = TRUE)

new_set2 <- new_set %>% 
  filter(Subgroup == "CHROM_2") %>% 
  filter(RELEASED <=10) %>% 
  filter(RELEASED >=0)

hist(new_set2$RELEASED,
     breaks = 240)

hist(new_set2$hour)
