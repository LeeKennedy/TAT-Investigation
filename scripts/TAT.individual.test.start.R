# clean up ---------------------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)

# Data In ----------------------------------------------------------------
data.in <- read_excel("~/Desktop/VITD05_start_time.xlsx")
data.in <- data.in[,c(1,3,4,8:11)]
colnames(data.in)[2] <- "Logged"
colnames(data.in)[3] <- "Received"
colnames(data.in)[4] <- "Started"
colnames(data.in)[5] <- "Released"
colnames(data.in)[6] <- "Changed"
colnames(data.in)[7] <- "Reviewed"


x <- sapply(data.in[,3], as.numeric)/(3600*24)
y <- sapply(data.in[,4], as.numeric)/(3600*24)


set <- as.data.frame(cbind(x, y))

set <- cbind(data.in[,c(1,3)],set)

set$Duration <- set$Started-set$Received


set <- na.omit(set)
set <- set %>%
        filter(Duration < 10)


set_plot <- ggplot(set, aes(x=Duration)) +
        geom_histogram(binwidth = 0.2, fill = "cornflowerblue", colour = "grey85")+
        labs(title = "Histogram of Start Times for Vitamin D", subtitle = "Days from Receipt at the Laboratory to Commencing Testing", x="Days", y="Samples") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              text = element_text(size = 10))
set_plot


