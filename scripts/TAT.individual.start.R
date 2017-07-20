# clean up ---------------------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)

# Data In ----------------------------------------------------------------
data.in <- read_excel("FATS01.xlsx")
#data.in <- data.in[,c(-9, -7)]
colnames(data.in)[6] <- "STARTED"
colnames(data.in)[5] <- "LOGGED"


#t0 <- sapply(data.in[,2], as.numeric)/3600
x <- sapply(data.in[,5], as.numeric)/(3600*24)
y <- sapply(data.in[,6], as.numeric)/(3600*24)


set <- as.data.frame(cbind(x, y))

#set[2:3] <- set[2:3]-set[,1]
#set <- set[,2:3]
set <- cbind(data.in[,c(1,3)],set)

set$Duration <- set$STARTED-set$LOGGED



#set <- set[,c(1,3,5)]

set <- na.omit(set)
set <- set %>%
        filter(Duration < 10)


set_plot <- ggplot(set, aes(x=Duration)) +
        geom_histogram(binwidth = 0.5, fill = "cornflowerblue", colour = "grey85")+
        labs(x="Days until testing commenced") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              text = element_text(size = 10))
set_plot
