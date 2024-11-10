#---------------------------------#
# Линейная регрессия и корреляция #
#---------------------------------#

#.libPaths("C:/CustomR/library") #только в университете!

library(openintro)

data(bdims)

mdims <- subset(bdims, sex == 1)

# Регрессионный анализ осуществляется путем построения линейной модели
# Сохраняем модель в отдельный объект и извлекаем подробности

reg.model <- lm(wgt ~ wri_gi, data = mdims)

reg.model

# Строим график и накладываем линию регрессии

plot(wgt ~ wri_gi, data = mdims, pch = 19, col = rgb(0,0.5,0,0.3),
     xlab = "Окружность запястья, см", ylab = "Вес, кг")
abline(reg.model, col = "darkgreen", lwd = 2)

# Предположения регрессионного анализа: нормальность и гомоскедастичность остатков

reg.res <- resid(reg.model)
reg.res <- reg.model$residuals

shapiro.test(reg.res)
library(nortest)
lillie.test(reg.res)
ad.test(reg.res)

hist(reg.res, prob = T)
xx <- seq(-30, 30, len = 100)
yy <- dnorm(xx, mean(reg.res), sd(reg.res))
lines(xx, yy, lwd = 2, col = "coral")

# Критерий Бройша-Пагана

library(car)
ncvTest(reg.model)

# Статистический анализ модели

summary(reg.model)

reg.model$coefficients
coef(reg.model)

# Доверительные интервалы для коэффициентов регрессии

confint(reg.model)
confint(reg.model, level = 0.99)

# Прогнозы для новых данных и доверительные интервалы для линии и для данных

pred.frame <- data.frame(wri_gi = c(15, 17, 20))

predict(reg.model, newdata = pred.frame)

predict(reg.model, newdata = pred.frame, interval = "confidence")
predict(reg.model, newdata = pred.frame, interval = "prediction")

pred.frame <- data.frame(wri_gi = seq(min(mdims$wri_gi), max(mdims$wri_gi), length = 100))

conf.line <- predict(reg.model, newdata = pred.frame, interval = "confidence")
conf.data <- predict(reg.model, newdata = pred.frame, interval = "prediction")

# Строим итоговый график

plot(wgt ~ wri_gi, data = mdims,  pch = 20, cex = 0.8, col = "blue",
        xlab = "Окружность запястья, см", ylab = "Вес, кг")
abline(reg.model, lwd = 2)
lines(pred.frame$wri_gi, conf.line[, 2], lty = 2, lwd = 2, col = "darkgreen")
lines(pred.frame$wri_gi, conf.line[, 3], lty = 2, lwd = 2, col = "darkgreen")
lines(pred.frame$wri_gi, conf.data[, 2], lty = 3, lwd = 2, col = "red")
lines(pred.frame$wri_gi, conf.data[, 3], lty = 3, lwd = 2, col = "red")
legend("topleft", legend = c("Данные", "Линия регрессии", 
                             "Доверительный интервал для линии регрессии", 
                             "Доверительный интервал для новых данных"), 
       col = c("blue", "black", "darkgreen", "red"), 
       pch = c(20, NA, NA, NA), lty = c(NA, 1, 2, 3),
       cex = .8, lwd = c(NA, 2, 2, 2), bty = "n")


# Коэффициент корреляции Пирсона

cor(mdims$kne_gi, mdims$kne_di)

cor(mdims[, 20:24])

# Анализ коэффициента корреляции

cor.test(mdims$kne_gi, mdims$kne_di)

cor.test(mdims$kne_gi, mdims$kne_di, conf.level = 0.99)


# Ранговый коэффициент корреляции Спирмена

cor.test(mdims$kne_gi, mdims$kne_di, method = "spearman")

# Непараметрический коэффициент корреляции Кенделла

cor.test(mdims$kne_gi, mdims$kne_di, method = "kendall")



# --- Самостоятельная работа --- #

library(ISwR)

# 1. Загрузите набор данных rmr: данные об уровне метаболизма (metabolic.rate) и 
# весе (body.weight) 44 женщин. Постройте график зависимости метаболизма от веса. 
# Постройте регрессионную модель. Имеет ли место зависимость (обоснуйте ответ 
# количественно)? Найдите доверительный интервал для наклона зависимости.
# Каков предсказанный уровень метаболизма для веса в 70 кг в соответствии с 
# построенной моделью?



# 2. Загрузите набор данных juul: данные по концентрации инсулин-подобного фактора 
# роста (igf1) и описательные данные пациентов (sex - пол, age - возраст). Постройте 
# регрессионную модель зависимости квадратного корня концентрации IGF-I от возраста 
# для пациентов старше 25 лет. Постройте график рассеяния, нарисуйте на нем линию 
# регрессии и доверительные интервалы для линии регрессии и предсказанных значений



# 3. Загрузите набор данных malaria: 100 детей из Ганы от 3 до 15 лет(age), уровень 
# антител в крови (ab) и наличие симптомов малярии (mal). Постройте регрессионную модель 
# зависимости логарифма уровня антител от возраста. Постройте график рассеяния с линией 
# регрессии и доверительными интервалами. Рассчитайте коэффициент корреляции Пирсона. 
# Значима ли зависимость? Найдите доверительный интервал коэффициента корреляции.
# Рассчитайте непараметрические коэффициенты корреляции, значима ли зависимость?


