# конкатенация элементов

sites <- c("a", "a", "b", "c")
n <- c(100, 150, 300, 80)


# обращение к элементу по номеру

sites[3]

# использование отдельных элементов в операциях

n[2] * 2 

# создание числовых векторов
# оператор :

1:5
7:2

# длина вектора

length(n)

# индексирование вектора

n[2]
index <- 1:3
n[index]
n[3:4]
n[c(1,3)]
n[-1]
n[c(-1,-4)]

# присваивание отдельным элементам

n[5] <- 1000
n

# действия над векторами

sum(n)
max(n)
min(n)
mean(n)
sd(n)
var(n)

# векторизованные вычисления

density <- c(2.8, 3.2, 1.5, 3.8)
area <- c(3, 5, 1.9, 2.7)
total_number <- density * area

# сравнения и выбор элементов

total_number[sites == 'a']
area[area > 2]

#--- Самостоятельно ---#

# Ниже приведены значения длины, ширины и высоты кустов тиса ягодного Taxus baccata, 
# сформированные в виде векторов

length <- c(2.2, 2.1, 2.7, 3.0, 3.1, 2.5, 1.9, 1.1, 3.5, 2.9)
width <- c(1.3, 2.2, 1.5, 4.5, 3.1, 2.8, 1.8, 0.5, 2.0, 2.7)
height <- c(9.6, 7.6, 2.2, 1.5, 4.0, 3.0, 4.5, 2.3, 7.5, 3.2)

# Сколько имеется кустов?



# Рассчитайте объем каждого куста


# Рассчитайте общий объем всех кустов



# Выберите в отдельный вектор значения ширины кустов, высота которых больше 2.5 м



#---------------------------------------------------------------------#

# Фрейм данных

survey <- data.frame(sites, density, area)

# Структура и размерность фрейма

str(survey)
nrow(survey)
ncol(survey)
dim(survey)

# Индексирование фрейма

survey[1, 3]
survey[2, ]
survey[, 3]
survey["density"]
survey$density
survey$density[4]

#---------------------------------------------------------------------#

# Загрузка данных по птицам. Способ первый. Импорт текстового файла

birds <- read.table("birds.txt", header = TRUE, dec = ',')

# Загрузка данных по птичкам. Способ второй. Буфер обмена

rm(birds)

birds <- read.table("clipboard", header = TRUE, dec = ',')

#--- Самостоятельно --------------------------------------------------#

# Выгрузите данные по кальмарам в текстовый файл. Импортируйте данные во фрейм squid

# Рассчитайте число наблюдений в данных по кальмарам

# Рассчитайте средний гонадосомный индекс кальмаров

#---------------------------------------------------------------------#

# Установка пакетов дома

#install.packages("readxl")
#install.packages("writexl")


#указываем путь к скачанным библиотекам (только в университете!! В 320-й и др. терминалах)

#.libPaths("C:/CustomR/library") 
# подключение пакета
library(readxl)

# Загрузка данных по птицам напрямую из файла .xlsx через пакет readxl

rm(birds)

birds <- read_excel(path = "birds.xlsx", sheet = 1, col_names = T, na = "NA")

#--- Самостоятельно --------------------------------------------------#

# Удалите фрейм squid



# Загрузите данные во фрейм squid повторно путем импорта из Excel



#---------------------------------------------------------------------#

# Экспорт данных

library(writexl)

write_xlsx(survey, path = "res.xlsx")
