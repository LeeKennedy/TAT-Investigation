#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------

library(readxl)
library(tidyverse)
library(LK.Toolbox)
library(here)


#### Data Input -----------------------------

here()


# Data In ----------------------------------------------------------------
data.in <- read_excel("data/12100630.xlsx", 
                       col_types = c("numeric", "date", "date", 
                                     "text", "numeric", "date", "date", 
                                     "date", "date", "date"))

data.in$ANALYSIS <-paste(data.in$ANALYSIS,"_", data.in$REPLICATE_COUNT, sep="")

sample_no <- as.character(data.in[1,1])

data.in <- data.in %>% 
        rename(LOGGED = LOGIN_DATE,
               RECEIVED = DATE_RECEIVED,
               STARTED = DATE_STARTED,
               RELEASED = RELEASED_ON,
               REVIEWED = DATE_REVIEWED
               )


t0 <- sapply(data.in[,"LOGGED"], as.numeric)/3600
x <- sapply(data.in[,"RECEIVED"], as.numeric)/3600
y <- sapply(data.in[,"STARTED"], as.numeric)/3600
z <- sapply(data.in[,"RELEASED"], as.numeric)/3600
w <- sapply(data.in[, "REVIEWED"], as.numeric)/3600

set <- as.data.frame(cbind(t0, x, y, z, w))

delay <- set[1,"RECEIVED"]-set[1,"LOGGED"]

set[2:5] <- set[2:5]-set[,1]
set <- set[,2:5]
set <- cbind(data.in[,c(1,4)],set)

n <- nrow(set) + 1

set[n,1] <- set[1,1]
set[n,2] <- "RELEASE"
set[n,4] <- max(set$RELEASED, na.rm=TRUE)
set[n,5] <- set[1,6]
set <- set[,c(2,4,5)]
set$Duration <- set$RELEASED-set$STARTED


set <- set[,c(1,2,4)]
set


long_set <- gather(set, Time, Hours, -ANALYSIS)

set_plot <- ggplot(long_set, aes(x=reorder(ANALYSIS, -Hours), y=Hours, fill=Time)) +
        geom_bar(stat='identity', col = "blue") +
        scale_fill_manual(values = c("cornflowerblue","beige")) +
        labs(title = paste("Sample", sample_no, "Timeline", sep=" "), x = " ", caption = "Blue dotted line = 5 days from receipt in Lab")+
        geom_hline(aes(yintercept=delay+120), lty=2, colour = "blue") +
        geom_hline(aes(yintercept=delay), lty=1, lwd=0.5, colour = "red") +
        coord_flip() +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              text = element_text(size = 14))
set_plot
