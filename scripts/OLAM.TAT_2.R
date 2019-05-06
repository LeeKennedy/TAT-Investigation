#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(lubridate)
library(LK.Toolbox)
library(here)


# Data In ----------------------------------------------------------------
here::here()
mg.data <- read_excel("data/OLAM.xlsx", col_types = c("numeric", 
                                                      "numeric", "date", "date", "date", "text", 
                                                      "text", "date", "date", "date", "date", 
                                                      "text", "numeric", "text", "text"))
mg.data <- mg.data[,-5]

mg.data <- mg.data %>% 
  filter(STATUS == "A") %>% 
  filter(LAB == "CHEMISTRY")

colnames(mg.data)[3] <- "LOGGED"
colnames(mg.data)[4] <- "RECEIVED"
colnames(mg.data)[7] <- "STARTED"
colnames(mg.data)[8] <- "RELEASED"
colnames(mg.data)[9] <- "CHANGED"
colnames(mg.data)[10] <- "REVIEWED"

mg.data <- mg.data %>% 
  filter(LOGGED > "2017-06-01")


t0 <- sapply(mg.data[,4], as.numeric)/(3600*24)
w <- sapply(mg.data[,7], as.numeric)/(3600*24)
x <- sapply(mg.data[,8], as.numeric)/(3600*24)
y <- sapply(mg.data[,9], as.numeric)/(3600*24)
z <- sapply(mg.data[,10], as.numeric)/(3600*24)
set <- as.data.frame(cbind(t0, w, x, y, z))

set[2:5] <- set[2:5]-set[,1]
set <- set[,2:5]
set <- cbind(mg.data[,c(1,5)],set)
set <- set %>% 
  mutate(altered = CHANGED - REVIEWED) %>% 
  filter(altered < 1)

hist(set$RELEASED,
     breaks = 100)


set$Duration <- set$RELEASED-set$STARTED



set <- na.omit(set)

summary.set <- set %>%
        group_by(ANALYSIS) %>%
        summarise(n = n(), 
                  ave.start = round(mean(STARTED),2), 
                  `95.pct.started` = round(quantile(STARTED, 0.95),2),
                  ave.duration = round(mean(Duration),2),
                  `95.pct.test.TAT` = round(quantile(RELEASED,0.95),2),
                  `95.pct.project.TAT` = round(quantile(REVIEWED,0.95),2))
                   
summary.set

# write.csv(summary.set, "~/Desktop/OLAM.csv")

sets <- set %>% 
  filter(ANALYSIS == "AFLA090615")

hist(sets$REVIEWED,
     breaks = 40)

hist(sets$RELEASED,
     breaks = 40)

