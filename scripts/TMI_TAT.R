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
tmi_data <- read_excel("data/SPC_621.xlsx", 
                      col_types = c("numeric", "text", "text", 
                                    "date", "date", "date", "date", "date", 
                                    "date", "date", "numeric", "text", 
                                    "date"))
tmi_data <- tmi_data[,c(1:8, 10,12)]

tmi_data <- tmi_data %>% 
  filter(STATUS == "A") 

colnames(tmi_data)[4] <- "LOGGED"
colnames(tmi_data)[5] <- "RECEIVED"
colnames(tmi_data)[6] <- "STARTED"
colnames(tmi_data)[7] <- "COMPLETED"
colnames(tmi_data)[8] <- "RELEASED"
colnames(tmi_data)[9] <- "REVIEWED"



t0 <- sapply(tmi_data[,4], as.numeric)/(3600*24)
v <- sapply(tmi_data[,5], as.numeric)/(3600*24)
w <- sapply(tmi_data[,6], as.numeric)/(3600*24)
x <- sapply(tmi_data[,7], as.numeric)/(3600*24)
y <- sapply(tmi_data[,8], as.numeric)/(3600*24)
z <- sapply(tmi_data[,9], as.numeric)/(3600*24)

set <- as.data.frame(cbind(t0,v, w, x, y, z))

set[2:6] <- set[2:6]-set[,1]
set <- set[,2:6]


summary.set <- set %>%
        summarise(n = n(), 
                  ave.completed = round(mean(COMPLETED),2), 
                  `95.pct.completed` = round(quantile(COMPLETED, 0.95),2),
                 `95.pct.test.TAT` = round(quantile(RELEASED,0.95),2),
                  `95.pct.project.TAT` = round(quantile(REVIEWED,0.95),2))
summary.set

set <- set %>% 
        filter(RELEASED < 7.5)

tmi_hist <- ggplot(set, aes(x=RELEASED)) +
        geom_histogram(binwidth = 0.05, fill="cornflowerblue", col = "black") +
        labs(x = "Days to release, from log in", y = "", title = "Standard Plate Count, Jan - Apr, 2019")+
        theme_bw()+
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14), axis.text.x = element_text(angle = 0, hjust = 1))
        
tmi_hist
