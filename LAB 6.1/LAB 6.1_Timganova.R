#Дисперсионный анализ. Пример "Типы диет"

#Загрузим данные (папку data "Set As A Working Directory")
data = read.csv("data/diet.csv", row.names = 1)
summary(data)
#Ознакомимся со структурой и переименуем колонки, как нам удобно
#data/Diet_data_description.docx
#data/diet.csv

colnames(data) <- c("gender", "age", "height", "initial.weight", 
                    "diet.type", "final.weight")
data$diet.type <- factor(c("A", "B", "C")[data$diet.type])

#Добавим новую колонку - Похудение
data$weight.loss = data$initial.weight - data$final.weight

#Проанализиуем есть ли различия по типам диет
boxplot(weight.loss~diet.type,data=data,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

#Проверим сбалансированные ли данные
table(data$diet.type)

#График групповых средних
###install.packages("gplots")
library(gplots)
plotmeans(weight.loss ~ diet.type, data=data)
aggregate(data$weight.loss, by = list(data$diet.type), FUN=sd)

#Для подгонки ANOVA модели используем функцию aov, частный случай линейной модели lm
#тест на межгрупповые различия
fit <- aov(weight.loss ~ diet.type, data=data)
summary(fit)

#Попарные различия между средними значениями для всех групп
TukeyHSD(fit)

#Tukey honest significant differences test
###install.packages("multcomp")
library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(diet.type="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")

#ЗАДАНИЕ

#Добавить проверку на выбросы и избавиться от них
##Удалим строки с N/A значениями
data <- na.omit(data) 
##Удалим строки с выбросами по boxplot
data <- data[!(data$diet.type == "A" & data$weight.loss > 8), ]
data <- data[!(data$diet.type == "B" & data$weight.loss < -2), ]

#Повторно провести все тесты и сравнить результаты с выбросами и без
###[Повторить все строчки выше - получатся следующие результаты:]

# Данные распределялись по типам диет следующим образом
# до удаления выбросов:
# A - 24,  B - 27,  C - 27 
# после удаления выбросов:
# A - 22,  B - 24,  C - 27 

# Среднеквадратичное отклонение:
# до удаления выбросов:
# Group.1        x
# 1       A 2.240148
# 2       B 2.523367
# 3       C 2.395568
# после удаления выбросов:
# Group.1        x
# 1       A 1.550569
# 2       B 2.243428
# 3       C 2.395568

# Значимость поднялась с ** до ***. 
# до удаления выбросов:
# Pr(>F) = 0.00323 **
# после удаления выбросов:
# Pr(>F) = 0.000719 ***

# TukeyHSD(fit)
# $diet.type
# до удаления выбросов:
#           diff        lwr      upr     p adj
# B-A -0.2740741 -1.8806155 1.332467 0.9124737
# C-A  1.8481481  0.2416067 3.454690 0.0201413
# C-B  2.1222222  0.5636481 3.680796 0.0047819
#
# после удаления выбросов:
#          diff        lwr      upr     p adj
# B-A 0.6871212 -0.8132814 2.187524 0.5193612
# C-A 2.3436027  0.8836100 3.803595 0.0007631
# C-B 1.6564815  0.2304030 3.082560 0.0188174

#Открыть документ Diet_data_description.docx и попытаться выполнить задания:

### По графику видно, что типы диет A и B были отнесены к a, а тип диеты C к b. 
### По объему сбрасываемого веса тип диеты C кажется самой эффективной, однако
### утверждать однозначно, благодаря чему происходит похудение, пока нельзя.
 
#Проанализируем, зависит ли похудение от пола
boxplot(weight.loss ~ gender, data = data, col="light gray",
        ylab = "Weight loss (kg)", xlab = "Gender")
aggregate(data$weight.loss, by = list(data$gender), FUN = summary)

# 1W.ANOVA
fit <- aov(weight.loss ~ gender, data = data)
summary(fit)
### Для пола Pr(>F) = 0.903, т.е. пол не влияет на похудение

# 2W.ANOVA
fit <- aov(weight.loss ~ diet.type + gender, data = data)
summary(fit)
### Тип диеты*** влияет, а пол нет

# Графики зависимости похудения от пола и типа диеты
diet.types <- split(data, f = data$diet.type)
par(mfrow=c(1,3))
boxplot(weight.loss ~ gender, data = diet.types$A, ylim = c(-1, 10), main = "Diet type A")
boxplot(weight.loss ~ gender, data = diet.types$B, ylim = c(-1, 10), main = "Diet type B")
boxplot(weight.loss ~ gender, data = diet.types$C, ylim = c(-1, 10), main = "Diet type C")

# ANCOVA - проанализируем зависимость похудения от диеты, пола, роста, возраста
fit <- aov(weight.loss ~ diet.type + gender + height, data = data)
summary(fit)
fit <- aov(weight.loss ~ diet.type + gender + height + age, data = data)
summary(fit)

### Никакие из параметров не влияют на похудение, кроме типов диет, поэтому
### диету C можно назвать самой эффективной, а диету А - самой неэффективной.
