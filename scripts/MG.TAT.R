# clean up ---------------------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)

# Data In ----------------------------------------------------------------

mg.data <- read_excel("~/Desktop/MG_June.xlsx", 
                      col_types = c("numeric", "numeric", "date", 
                                    "date", "text", "date", "date", "date", 
                                    "text", "text", "numeric", "text"))
mg.data <- mg.data[,-4]

mg.data <- mg.data %>% filter(STATUS == "A")

colnames(mg.data)[5] <- "PREPPED"
colnames(mg.data)[6] <- "STARTED"
colnames(mg.data)[7] <- "REVIEWED"

t0 <- sapply(mg.data[,3], as.numeric)/(3600*24)
x <- sapply(mg.data[,5], as.numeric)/(3600*24)
y <- sapply(mg.data[,6], as.numeric)/(3600*24)
z <- sapply(mg.data[,7], as.numeric)/(3600*24)


set <- as.data.frame(cbind(t0, x, y, z))

set[2:4] <- set[2:4]-set[,1]
set <- set[,2:4]
set <- cbind(mg.data[,c(1,4)],set)


set$Duration <- set$REVIEWED-set$STARTED

set <- na.omit(set)

summary.set <- set %>%
        group_by(ANALYSIS) %>%
        summarise(n = n(), 
                  max.prep = round(max(PREPPED),2), 
                  max.start = round(max(STARTED),2), 
                  max.duration = round(max(Duration),2),
                  max.completed = round(max(REVIEWED),2),
                  `90.pct.completed` = round(quantile(REVIEWED,0.90),2),
                  `99.pct.completed` = round(quantile(REVIEWED,0.99),2))
summary.set

write.csv(summary.set, "MG_TATS2.csv")

