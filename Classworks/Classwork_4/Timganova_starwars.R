#СПРАВКА https://dplyr.tidyverse.org/reference/
#Установить пакеты
#install.packages("tidyverse")
#install.packages("dplyr")
library(dplyr)

#Загрузим тестовый набор данных
data(starwars)

#Прочитайте описание массива данных
?starwars

#узнайте количество строк и колонок в наборе с помощью функций nrow и summary
starwars %>% nrow
summary(starwars)

#С помощью функции distinct выберите уникальные цвета волос
starwars %>% distinct(hair_color)

#Сгруппируйте по цвету волос и посчитайте сколько всего строк каждого цвета
hcn <- starwars %>%
  group_by(hair_color) %>%
  summarise(n())

#Отсортируйте по убыванию то, что получили выше
hcn %>% arrange(desc(across(.cols = ncol(.):1))) 

#Посчитайте среднюю массу всех представителей
mean(starwars$mass)
#нужно отфильтровать, чтобы получилось правильное значение?

#Теперь найдите самого высокого, самого низкого
mh <- starwars %>%
  filter(!is.na(height))
max(mh$height)
min(mh$height)

#Отфильтруйте их и снова посчитайте среднюю массу
mm <- starwars %>%
  filter(!is.na(mass))
mean(mm$mass)

#Найдите средний рост жителя каждой из планет
mhoep <- mh %>%
  select(homeworld, height) %>%
  filter(!is.na(homeworld)) %>%
  group_by(homeworld) %>%
  summarize(mean_height = mean(height))
mhoep