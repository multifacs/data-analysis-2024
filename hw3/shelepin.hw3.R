# Установка пакета ISwR (если у тебя его еще нет)
if (!require(ISwR)) {
  install.packages("ISwR")
}

# Загрузка пакета
library(ISwR)

# Загрузка данных juul
data(juul)

# Просмотр первых строк данных
head(juul)

# Просмотр структуры данных
str(juul)

# Просмотр описательной статистики
summary(juul)

# Создание новой переменной sqrt_igf1, содержащей квадратный корень из переменной igf1
juul$sqrt_igf1 <- sqrt(juul$igf1)

# Просмотр первых строк обновлённого фрейма данных
head(juul)

# Отфильтруем данные, оставив только пациентов старше 25 лет
juul_over_25 <- subset(juul, age > 25)

# Построим регрессионную модель зависимости sqrt_igf1 от возраста
model <- lm(sqrt_igf1 ~ age, data = juul_over_25)

# Посмотрим на результаты модели
summary(model)

# Вычисляем остатки регрессионной модели
residuals_model <- residuals(model)

# Построение гистограммы остатков
hist(residuals_model, breaks = 20, main = "Гистограмма остатков", xlab = "Остатки", col = "lightblue")

# Тест Шапиро-Уилка
shapiro_test <- shapiro.test(residuals_model)
shapiro_p_value <- shapiro_test$p.value

# Стандартизация остатков для теста Колмогорова-Смирнова
residuals_standardized <- (residuals_model - mean(residuals_model)) / sd(residuals_model)

# Тест Колмогорова-Смирнова
ks_test <- ks.test(residuals_standardized, "pnorm")
ks_p_value <- ks_test$p.value

# Тест Андерсона-Дарлинга
if (!require(nortest)) {
  install.packages("nortest")
}
library(nortest)
ad_test <- ad.test(residuals_model)
ad_p_value <- ad_test$p.value

# Вывод p-значений
cat("p-значение теста Шапиро-Уилка:", shapiro_p_value, "\n")
cat("p-значение теста Колмогорова-Смирнова:", ks_p_value, "\n")
cat("p-значение теста Андерсона-Дарлинга:", ad_p_value, "\n")

# Итоговый вывод
if (shapiro_p_value > 0.05 && ks_p_value > 0.05 && ad_p_value > 0.05) {
  cat("Остатки распределены нормально (по всем тестам p > 0.05).\n")
} else {
  cat("Остатки НЕ распределены нормально (по крайней мере один из тестов показал p < 0.05).\n")
}

# Установка и загрузка пакета lmtest (если не установлен)
if (!require(lmtest)) {
  install.packages("lmtest")
}
library(lmtest)

# Проведение теста Бройша-Пагана для проверки гомоскедастичности
bp_test <- bptest(model)
bp_p_value <- bp_test$p.value

# Вывод результата
cat("p-значение теста Бройша-Пагана:", bp_p_value, "\n")

# Итоговый вывод
if (bp_p_value > 0.05) {
  cat("Гомоскедастичность подтверждена (p > 0.05). Модель удовлетворяет условию равенства дисперсий остатков.\n")
} else {
  cat("Гетероскедастичность обнаружена (p < 0.05). Модель не удовлетворяет условию равенства дисперсий остатков.\n")
}

# Извлекаем коэффициенты из модели
coefficients_model <- coef(model)

# Коэффициент при переменной age
beta_1 <- coefficients_model['age']

# Рассчитываем изменение ожидаемого значения sqrt_igf1 при увеличении возраста на 5 месяцев
change_in_response <- beta_1 * (5 / 12)

# Выводим результат
cat("Изменение ожидаемого значения sqrt_igf1 при увеличении возраста на 5 месяцев:", change_in_response, "\n")

# Заданные значения возраста
ages <- c(41.77, 41.88, 43.84, 46.08, 54.91)

# Извлекаем коэффициенты из модели
coefficients_model <- coef(model)

# Интерсепт (свободный член)
beta_0 <- coefficients_model['(Intercept)']

# Коэффициент при переменной age
beta_1 <- coefficients_model['age']

# Рассчитываем ожидаемые значения sqrt_igf1 для заданных возрастов
expected_values <- beta_0 + beta_1 * ages

# Выводим результаты
cat("Ожидаемые значения sqrt_igf1 для заданных возрастов:\n")
data.frame(Age = ages, Expected_sqrt_igf1 = expected_values)


# Рассчитаем 95%-ный доверительный интервал для всех коэффициентов модели
conf_intervals <- confint(model, level = 0.95)

# Выводим доверительный интервал для коэффициента при переменной age
cat("95%-ный доверительный интервал для коэффициента наклона (age):\n")
conf_intervals['age', ]

# Интерпретация:
# Если доверительный интервал не включает 0 (например, если оба конца интервала строго положительные или строго отрицательные), это указывает на статистически значимый эффект возраста на переменную отклика.
# Если доверительный интервал включает 0, это означает, что влияние возраста на переменную отклика может быть статистически незначимым на уровне 95%-ного доверия.

