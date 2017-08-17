# clean up ---------------------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)

# Data In ----------------------------------------------------------------
data.in <- read_excel("9154410.xlsx")
data.in <- data.in[,c(1:8)]
colnames(data.in)[6] <- "COMPLETED"
colnames(data.in)[7] <- "SAMPLE_COMPLETED"
colnames(data.in)[5] <- "STARTED"


t0 <- sapply(data.in[,2], as.numeric)/3600
x <- sapply(data.in[,5], as.numeric)/3600
y <- sapply(data.in[,6], as.numeric)/3600
z <- sapply(data.in[,7], as.numeric)/3600
w <- sapply(data.in[,8], as.numeric)/3600

set <- as.data.frame(cbind(t0, x, y, z, w))

set[2:5] <- set[2:5]-set[,1]
set <- set[,2:5]
set <- cbind(data.in[,c(1,3,4)],set)

n <- nrow(set) + 1

set[n,1] <- set[1,1]
set[n,c(2,3)] <- "RELEASE"
set[n,4] <- set[1,6]
set[n,5] <- set[1,7]
set <- set[,c(2,4,5)]
set$Duration <- set$COMPLETED-set$STARTED


set <- set[,c(1,2,4)]
set


long_set <- gather(set, Time, Hours, -ANALYSIS)

set_plot <- ggplot(long_set, aes(x=reorder(ANALYSIS, -Hours), y=Hours, fill=Time)) +
        geom_bar(stat='identity') +
        scale_fill_manual(values = c("cornflowerblue","burlywood1")) +
        geom_hline(aes(yintercept=72), lty=2, colour = "red") +
        geom_hline(aes(yintercept=120), lty=2, colour = "red") +
        coord_flip() +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              text = element_text(size = 14))
set_plot
