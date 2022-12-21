#Пользуясь примером из лекции файл (6.0.R) проанализируйте данные
#о возрасте и физ. характеристиках молюсков
#https://archive.ics.uci.edu/ml/datasets/abalone

data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header=TRUE, sep=",")
summary(data)
colnames(data)
colnames(data) <- c("sex", "length", "diameter", "height", 
                "whole_weight", "shucked_weight",
                "viscera_weight", "shell_weight", "rings")
colnames(data)

data$sex <- as.factor(data$sex)
par(mfrow=c(1,3))
hist(data$diameter, main = "Диаметр, мм")
hist(data$height, main = "Высота, мм")
hist(data$whole_weight, main = "Полный вес, гр")
#Видим ассиметрию (https://en.wikipedia.org/wiki/Skewness) и выбросы

#Найдём строки с выбросами
data <- data[!(data$diameter < 0.155), ]
data <- data[!(data$height > 0.240)&!(data$height < 0.04), ]
data <- data[!(data$whole_weight > 2.19), ]

#plot(data$diameter)
boxplot(data$diameter)
boxplot(data$height)
boxplot(data$whole_weight)

#Визулизируем возможные зависимости
par(mfrow=c(1,2)) 
plot(data$diameter, data$whole_weight,'p',main = "Зависимость веса от диаметра")
plot(data$height, data$whole_weight,'p',main = "Зависимость веса от высоты")
###plot(data$height, data$diameter,'p',main = "Зависимость высоты от диаметра")
#Хорошо видна зависимость, нужно её исследовать

#Построить линейные модели при помощи функции lm, посмотреть их характеристики
linear.model.1 <- lm(whole_weight ~ diameter, data=data)
linear.model.1
summary(linear.model.1)
###p-value < 2.2e-16, значит p-value < 0.001!

linear.model.2 <- lm(whole_weight ~ height, data=data)
linear.model.2
summary(linear.model.2)
###p-value < 2.2e-16, значит p-value < 0.001!


#Избавиться от выбросов, построить ещё модели и проверить их
boxplot(data$length)
boxplot(data$rings)
##Избавляемся от выбросов в length и rings
data <- data[!(data$length < 0.215), ]
data <- data[!(data$rings > 15)&!(data$rings < 4), ]
##Визулизируем возможные зависимости
plot(data$length, data$whole_weight,'p',main = "Зависимость длины от высоты")
plot(data$rings, data$whole_weight,'p',main = "Зависимость веса от высоты")

linear.model.wwl <- lm(whole_weight ~ length, data=data)
linear.model.wwl
summary(linear.model.wwl)
#plot(linear.model.wwl)
###Опять p-value < 2.2e-16, значит p-value < 0.001!

linear.model.wwr <- lm(whole_weight ~ rings, data=data)
linear.model.wwr
summary(linear.model.wwr)
#plot(linear.model.wwr)
###Опять p-value < 2.2e-16, значит p-value < 0.001!


#Деление массива на две части и прогнозирование значений
data.noout <- data
odds <- seq(1, nrow(data.noout), by=2)
data.in <- data.noout[odds,]
data.out <- data.noout[-odds,]

linear.model.half <- lm (whole_weight ~ ., data=data.in)
summary(linear.model.half)
###Опять p-value < 2.2e-16, значит p-value < 0.001!

##Подогнать модель по первой части
data.predict <- predict(linear.model.half)
cor(data.in$whole_weight, data.predict)
plot(data.in$whole_weight, data.predict)
### Высокая корреляция

##Спрогнозировать (функция predict) значения во второй части
data.predict.out <- predict(linear.model.half, data.out)
cor(data.out$whole_weight, data.predict.out)
plot(data.out$whole_weight, data.predict.out)
### Высокая корреляция

### Корреляция (0.9961196 и 0.9951115) почти равная, что также видно по графикам
### Значения предсказаны качественно.