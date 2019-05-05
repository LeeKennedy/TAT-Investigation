# clean up ---------------------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)

# Data In ----------------------------------------------------------------
data.in2 <- read_excel("~/Desktop/TAT/June_2017.xlsx")
data.in2 <- data.in2[,-5]

data.in <- data.in2 %>% 
  #filter(LAB == "CHEMISTRY") %>% 
  filter(SAMPLE_NUMBER == "10048076")

data.in$ANALYSIS <-paste(data.in$ANALYSIS,"_", data.in$REPLICATE_COUNT, sep="")

colnames(data.in)[3] <- "LOGGED"
colnames(data.in)[4] <- "RECEIVED"
colnames(data.in)[7] <- "STARTED"
colnames(data.in)[8] <- "RELEASED"
colnames(data.in)[10] <- "REVIEWED"

t0 <- sapply(data.in[,3], as.numeric)/3600
x <- sapply(data.in[,4], as.numeric)/3600
y <- sapply(data.in[,7], as.numeric)/3600
z <- sapply(data.in[,8], as.numeric)/3600
w <- sapply(data.in[,10], as.numeric)/3600

set <- as.data.frame(cbind(t0, x, y, z, w))

delay <- set[1,2]-set[1,1]

set[2:5] <- set[2:5]-set[,1]
set <- set[,2:5]
set <- cbind(data.in[,c(1,5,6)],set)

n <- nrow(set) + 1

set[n,1] <- set[1,1]
set[n,c(2,3)] <- "RELEASE"
set[n,5] <- max(set$RELEASED, na.rm=TRUE)
set[n,6] <- set[1,7]
set <- set[,c(2,5,6)]
set$Duration <- set$RELEASED-set$STARTED


set <- set[,c(1,2,4)]
set


long_set <- gather(set, Time, Hours, -ANALYSIS)

set_plot <- ggplot(long_set, aes(x=reorder(ANALYSIS, -Hours), y=Hours, fill=Time)) +
        geom_bar(stat='identity') +
        scale_fill_manual(values = c("cornflowerblue","burlywood1")) +
        geom_hline(aes(yintercept=delay+72), lty=2, colour = "blue") +
        geom_hline(aes(yintercept=delay+120), lty=2, colour = "blue") +
        geom_hline(aes(yintercept=delay), lty=1, lwd=1, colour = "red") +
        coord_flip() +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              text = element_text(size = 14))
set_plot
