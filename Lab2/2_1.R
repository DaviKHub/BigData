library(readxl)
library(dplyr)
library(tidyr)

setwd("/Users/david/BigData/Lab2")

df <- read_excel("ответы.xlsx", sheet = 1)
print("Названия колонок:")
print(colnames(df))

df_cleaned <- df[, -c(1,2)]  # фио и время
head(df_cleaned)

# 1. Вычислить max, min, mean по каждому столбцу
max_values <- apply(df_cleaned, 2, max, na.rm=TRUE)
min_values <- apply(df_cleaned, 2, min, na.rm=TRUE)
mean_values <- apply(df_cleaned, 2, mean, na.rm=TRUE)

result <- data.frame(Max=max_values, Min=min_values, Mean=mean_values)
print(result)

# 2. Подсчитать количество людей, отдавших предпочтение выбранному элементу >0.7 и <0.3 (составить вектор);
count_above_7 <- colSums(df_cleaned > 7, na.rm=TRUE)
count_below_3 <- colSums(df_cleaned < 3, na.rm=TRUE)

preference_counts <- data.frame(More7 = count_above_7, Low3 = count_below_3)

print(preference_counts)

# 3. Вывод рейтинга напитков в порядке убывания среднего значения
ranking <- df_cleaned %>%
  summarise_all(mean, na.rm=TRUE) %>%
  pivot_longer(cols = everything(), names_to = "Вид спорта", values_to = "Средний_рейтинг") %>%
  arrange(desc(Средний_рейтинг))
print("Рейтинг видов спорта:")
print(ranking)

# 4. Работа с пропущенными данными
df_no_na <- na.omit(df_cleaned) # NA
df_filled_na <- df_cleaned %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm=TRUE), .))

print("Таблица без пропущенных значений:")
print(df_no_na)

print("Таблица с замененными NA на средние:")
print(df_filled_na)

# 5. Выбор строк (Хоббихорсинг > 8)
filtered_df <- df_cleaned %>% filter(`Хоббихорсинг` > 8)
print("Фильтрованные данные (Хоббихорсинг > 8):")
print(filtered_df)

# 6. Построение столбчатой диаграммы
barplot(colMeans(df_cleaned, na.rm=TRUE), 
        main="Средние оценки видов спорта (1-10)", 
        col=rainbow(ncol(df_cleaned)), 
        names.arg=names(df_cleaned), 
        las=2)