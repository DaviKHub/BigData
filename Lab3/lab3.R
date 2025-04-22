library(readxl)
data <- read_excel("/Users/david/BigData/Lab3/results.xlsx")

par(mar = c(11, 4, 4, 1))
barplot(t(data_p_combined), beside = TRUE, col = heat.colors(3),
        main = "Количество мест 1-3 по каждой Олимпиаде (мужчины и женщины)",
        ylab = "Количество мест",
        legend.text = paste("Место", 1:3), args.legend = list(x = "topright"),
        las = 2)

first <- data_p_combined[, 1]
without_zeros <- first[data_only_first != 0]
colors <- terrain.colors(length(without_zeros))
pie(without_zeros,
    labels = without_zeros,
    col = colors,
    main = "Количество первых мест России по вольной борьбе")
legend("right",
       legend = names(without_zeros),
       fill = colors,
       title = "Олимпиада")

#тенденции изменения
male_trends <- data[, c(2)]
female_trends <- data[, c(11)]
years_trends <- sapply(data[, 1], function(x) as.numeric(gsub("[^0-9]", "", x)))
years_trends <- as.numeric(years_trends)

plot(years_trends, male_trends$Мужчины, type = "o", col = "blue",
     main = "Тенденции изменения количества призовых мест России по спортивной гимнастике",
     xlab = "Год", ylab = "Количество призовых мест", ylim = c(0, 10), xaxt = "n")
lines(years_trends, female_trends$Женщины, type = "o", col = "red")
legend("topright", legend = c("Мужчины", "Женщины"), col = c("blue", "red"), lty = 1)
axis(1, at = years_trends, labels = years_trends, las = 1)

#столбчатая диаграмма (мужчины и женщины)
par(mfrow=c(1,2))
par(mar = c(11, 4, 4, 1))
barplot(t(data_p_men[,c(1,2,3)]), beside = TRUE, col = topo.colors(3),
        main = "Количество мест 1-3 по каждой Олимпиаде (Мужчины)",
        ylab = "Количество мест",
        legend.text = paste("Место", 1:3), args.legend = list(x = "topright"),
        las = 2)

barplot(t(data_p_women[,c(1,2,3)]), beside = TRUE, col = topo.colors(3),
        main = "Количество мест 1-3 по каждой Олимпиаде (Женщины)",
        ylab = "Количество мест",
        legend.text = paste("Место", 1:3), args.legend = list(x = "topright"),
        las = 2)

#круговые диаграммы раздельно (мужчины и женщины)
par(mfrow=c(1,2))

mw <- data[, c(2,11)]
mw_matrix <- as.matrix(mw)
rownames(mw_matrix) <- data$Олимпиада

male_without_zeros <- mw_matrix[,1]
male_without_zeros <- male_without_zeros[data_male_without_zeros != 0]

pie(male_without_zeros, main = "Призовые места (Мужчины)",
    col = heat.colors(length(male_without_zeros)),
    labels = paste(names(male_without_zeros), " (", male_without_zeros, ")", sep = ""))

female_without_zeros <- mw_matrix[,2]
female_without_zeros <- female_without_zeros[data_female_without_zeros != 0]

pie(female_without_zeros, main = "Призовые места (Женщины)",
    col = heat.colors(length(female_without_zeros)),
    labels = paste(names(female_without_zeros), " (", female_without_zeros, ")", sep = ""))

#графики по всем странам — золотые медали
data_table_all <- read_excel("/Users/david/BigData/Lab3/summer.xlsx")
filtered_all <- subset(data_table_all, Medal == "Gold")
medals_by_year_country <- aggregate(Medal ~ Year + Country, data = filtered_all, FUN = length)
countries <- unique(medals_by_year_country$Country)

plot(NULL,
     xlim = range(medals_by_year_country$Year),
     ylim = c(0, max(medals_by_year_country$Medal)),
     xlab = "Год",
     ylab = "Количество медалей",
     main = "Золотые медали по странам")

for (country in countries) {
  country_data <- subset(medals_by_year_country, Country == country)
  lines(country_data$Year, country_data$Medal, type = "o", col = which(country == countries), lwd = 2, pch = 10)
}

legend("topright", legend = countries, col = seq_along(countries), lwd = 2, pch = 10, title = "Страна")

#графики по всем странам — все призовые
medals_by_year_country_all <- aggregate(Medal ~ Year + Country, data = data_table_all, FUN = length)
countries_all <- unique(medals_by_year_country_all$Country)

plot(NULL,
     xlim = range(medals_by_year_country_all$Year),
     ylim = c(0, max(medals_by_year_country_all$Medal)),
     xlab = "Год",
     ylab = "Количество медалей",
     main = "Призовые места по странам")

for (country in countries_all) {
  country_data_all <- subset(medals_by_year_country_all, Country == country)
  lines(country_data_all$Year, country_data_all$Medal, type = "o", col = which(country == countries_all), lwd = 2, pch = 10)
}

legend("topright", legend = countries_all, col = seq_along(countries_all), lwd = 2, pch = 10, title = "Страна")