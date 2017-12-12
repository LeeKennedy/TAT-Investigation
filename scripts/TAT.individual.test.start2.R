# clean up ---------------------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)

# Data In ----------------------------------------------------------------
data.in <- read_excel("~/Documents/GitHub/TAT-Investigation/data/PEST08_2017a.xlsx", 
                      col_types = c("numeric", "text", "date","date", "date", "date", "date", "date", "text", "text"))



colnames(data.in)[3] <- "Logged"
colnames(data.in)[4] <- "Received"
colnames(data.in)[5] <- "Started"
colnames(data.in)[6] <- "Released"
colnames(data.in)[7] <- "Changed"
colnames(data.in)[8] <- "Reviewed"

data.in <- data.in %>% 
  filter(Logged > "2017/06/01")

a <- sapply(data.in[,3], as.numeric)/(3600*24)
b <- sapply(data.in[,4], as.numeric)/(3600*24)
c <- sapply(data.in[,5], as.numeric)/(3600*24)
d <- sapply(data.in[,6], as.numeric)/(3600*24)
e <- sapply(data.in[,7], as.numeric)/(3600*24)
f <- sapply(data.in[,8], as.numeric)/(3600*24)

set <- as.data.frame(cbind(a,b,c,d,e,f))

set <- cbind(data.in[,c(1,2,9,10)],set)

set$Lab_Duration <- set$Started-set$Received

set <- set %>% 
  filter(Lab_Duration >= 0)

set$TAT <- set$Reviewed - set$Logged


set <- na.omit(set)



set_plot_1 <- ggplot(set, aes(x=Lab_Duration)) +
        geom_histogram(binwidth = 0.2, fill = "cornflowerblue", colour = "grey85")+
        labs(title = "Start Times for PEST08", subtitle = "Days from Receipt at the Laboratory to Commencing Testing", x="Days", y="Samples") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              text = element_text(size = 10))
set_plot_1

set_plot_2 <- ggplot(set, aes(x=TAT)) +
  geom_histogram(binwidth = 0.2, fill = "cornflowerblue", colour = "grey85")+
  labs(title = "TAT for PEST08", subtitle = "Days from Logging the Sample to Reviewing the Sample", x="Days", y="Samples") +
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 10))
set_plot_2
