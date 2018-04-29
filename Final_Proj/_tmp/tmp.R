library(dplyr)
library(tidyverse)
library(ggplot2)
library(directlabels)

getwd()
setwd("/Users/jorgerojas/Downloads/") 

df <- read.table ("events_cleaned_v3_full_utf8.txt", header = TRUE, sep = "|", nrows = 1000000, 
                  colClasses = c(
                    'NULL' 			,
                    'integer' 	  ,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'integer' 	  ,
                    'integer' 	  ,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'integer' 	  ,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'integer' 	  ,
                    'NULL' 			,
                    'NULL' 			,
                    'integer' 	  ,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'NULL' 			,
                    'integer' 	  ,
                    'integer' 	  ))
df[df < 0]  <- NA

df_1 <- df
df_1 <- df_1[complete.cases(df_1), ]

df_1$gender <- as.factor(df_1$gender)
df_1$race <- as.factor(df_1$race)
df_1$incident.location.type <- as.factor(df_1$incident.location.type)
df_1$primary.symptom <- as.factor(df_1$primary.symptom)
df_1$cause.of.injury <- as.factor(df_1$cause.of.injury)


df_ilt.freq <- df_1 %>% group_by(age.in.years, incident.location.type) %>% summarise(Freq=n())
df_coi.freq <- df_1 %>% group_by(age.in.years, cause.of.injury) %>% summarise(Freq=n())


ggplot(df_ilt.freq) + geom_line(aes(x=age.in.years, y=Freq, color=incident.location.type)) +
  geom_line(aes(x=age.in.years, y=Freq, col=incident.location.type)) + 
  scale_color_discrete(name="Legend") + 
  labs(title="incident.location.type") +
  #geom_dl(aes(x=age.in.years, y=Freq,label = incident.location.type),  method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8))
  facet_wrap( ~ incident.location.type , ncol=4) 

ggplot(df_coi.freq) + geom_line(aes(x=age.in.years, y=Freq, color=cause.of.injury)) +
  geom_line(aes(x=age.in.years, y=Freq, col=cause.of.injury)) + 
  scale_color_discrete(name="Legend") +
  labs(title="cause.of.injury") +
  #geom_dl(aes(x=age.in.years, y=Freq,label = cause.of.injury),  method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  facet_wrap( ~ cause.of.injury , ncol=4) 

ir.pca <- prcomp(df,
                 center = TRUE,
                 scale. = TRUE) 


