msg <- readLines('putin-2012.txt')

#посчитаем кол-во символов
length(msg)
l<-lapply(msg, nchar)
Reduce(sum,l)

#можно не заморачиваться
txt <- paste(msg, collapse = " ")
nchar(txt)

r <- grep('мы',msg)
r
msg[r]

r <- grep('Мы', msg)
r
msg[r]

words <- unlist(strsplit(txt, ' '))
wc <- table(words)
wc <- sort(wc, decreasing = TRUE)
head(wc,20)

# ДОПОЛНЕНИЕ

#удаляем пунктуационные знаки
words <- words
words <- gsub("[[:punct:] ]+",' ', words)
#удаляем пробелы
words <- trimws(words)
#удаляем пустые значения
words <- words[words != ""]

#Количество слов
length(words)

#таблица самых часто встречающихся слов
wc <- table(words)
wc <- sort(wc, decreasing = TRUE)
head(wc, 20)

#Процентное сравнение
fr = round(wc/length(words)*100, 2)
head(fr, 20)