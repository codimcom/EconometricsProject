#install.packages("haven")
#install.packages("tidyverse")

library(haven)
library(tidyverse)
library(modelsummary)
library(fixest)
library(ggplot2)
getwd()
setwd("~/Downloads")

data <- read_sav("r32iall_71_1.sav")
datal <- select(data, bb_age, bbh5, bbl5.1, 
                bbj6.1a, bbm3, bbm71, bbm80.0, bbm113b, bbl90, bbj8)

data1 <- drop_na(datal,bbl90,bbj8)
y <- subset(data1, bbl90<99999997&bbj8<999)
y[is.na(y)]<-0
y[y > 100000] <- NA
y <- drop_na(y)
y$bbh5 <- as.factor(y$bbh5)
#добавление рабочих часов за год и средних больничных за месяц
y <- mutate(y,bbl90_mnth= round(bbl90/12, 2))
y <- mutate(y, bbj8_year = bbj8*12)
summary(y)

#удаление выбросов
y <- y %>%
  filter(#bbl90 >= quantile(bbl90, 0.25) - 1.5 * IQR(bbl90),
         bbl90 <= quantile(bbl90, 0.75) + 1.5 * IQR(bbl90))
#y <- y %>%
#  filter(bbj8 >= quantile(bbj8, 0.25) - 1.5 * IQR(bbj8),
#    bbj8 <= quantile(bbj8, 0.75) + 1.5 * IQR(bbj8))




#boxplots
qplot(bbj8_year, bbl90, data = y, geom = "point")
boxplot(y$bb_age)
boxplot(y$bbl90)
boxplot(y$bbj8)


#regressions

#пропущенные дни от часов работы
disease_work <- feols(bbl90 ~ bbj8_year, data = y, vcov = "hetero")
summary(disease_work)
ggplot(y, aes(bbj8_year, bbl90, colour = bbh5))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, color="blue")
#сложная модель
disease_work_cmplx <- feols(bbl90 ~ bbj8_year + bb_age + bbh5 + bbl5.1 + bbm3 + 
                              bbm80.0 + bbm71 + bbm113b, data = y, vcov = "hetero")
summary(disease_work_cmplx)


#часы работы от возраста
hours_age <- feols(bbj8 ~ bb_age, data = y, vcov = "hetero")
ggplot(y, aes(bb_age, bbj8, colour = bbh5)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue")
summary(hours_age)

#работа и здоровье
#y <- subset(y, bbm3<9)
ggplot(y, aes(bbj8, bbm3, colour = bbh5)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue")
