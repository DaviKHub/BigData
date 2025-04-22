library(rvest)

years <- 2014:2021
urls <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", years)

all_data <- list()

for (i in seq_along(urls)) {
  page <- read_html(urls[i])
  tables <- html_nodes(page, "table")
  if (length(tables) >= 2) {
    df <- html_table(tables[[2]], fill = TRUE)
    df <- as.data.frame(df)
    rownames(df) <- df[, 2]
    df <- df[, 3:11]
    all_data[[as.character(years[i])]] <- df
  }
}

country <- c("Russia", "Germany", "Sweden", "France", "Finland")
colors <- c("blue", "green", "red", "brown", "grey")

indices <- c("Quality of Life Index", "Purchasing Power Index", "Safety Index", "Health Care Index",
             "Cost of Living Index", "Property Price to Income Ratio", "Traffic Commute Time Index",
             "Pollution Index", "Climate Index")

year_range <- list(
  "Quality of Life Index" = 2014:2021,
  "Purchasing Power Index" = 2014:2021,
  "Safety Index" = 2014:2021,
  "Health Care Index" = 2014:2021,
  "Cost of Living Index" = 2014:2021,
  "Property Price to Income Ratio" = 2014:2021,
  "Traffic Commute Time Index" = 2014:2021,
  "Pollution Index" = 2014:2021,
  "Climate Index" = 2016:2021
)

for (index in indices) {
  idx_years <- year_range[[index]]
  data_mat <- matrix(NA, nrow = length(idx_years), ncol = length(country))
  rownames(data_mat) <- idx_years
  colnames(data_mat) <- country

  for (i in seq_along(idx_years)) {
    year <- as.character(idx_years[i])
    if (!is.null(all_data[[year]])) {
      df <- all_data[[year]]
      for (j in seq_along(country)) {
        cname <- country[j]
        if (cname %in% rownames(df) && index %in% colnames(df)) {
          val <- df[cname, index]
          val <- as.numeric(gsub(",", ".", val))
          data_mat[i, j] <- val
        }
      }
    }
  }

  mn <- min(data_mat, na.rm = TRUE)
  mx <- max(data_mat, na.rm = TRUE)

  plot(idx_years, data_mat[, 1], xlab = "Года", ylab = index, ylim = c(mn - 5, mx + 5),
       main = paste("Оценка:", index), col = colors[1], type = "b", lty = 1, pch = 1, lwd = 2)

  for (j in 2:length(country)) {
    lines(idx_years, data_mat[, j], type = "b", col = colors[j], lty = 1, pch = 1, lwd = 2)
  }

  legend("topright", cex = 0.7, legend = country, fill = colors)
}





# WEB скрэпинг
library(rvest)

page <- read_html("https://www.culture.ru/museums/institutes/location-krasnodarskiy-kray-krasnodar")
cards <- html_nodes(page, ".CHPy6")

titles <- c()
addresses <- c()
links <- c()
descriptions <- c()

for (card in cards) {
  title <- html_node(card, ".p1Gbz") %>% html_text(trim = TRUE)
  address <- html_node(card, ".TxTRy") %>% html_text(trim = TRUE)

  href <- html_node(card, "a") %>% html_attr("href")
  full_link <- paste0("https://www.culture.ru", href)

  museum_page <- read_html(full_link)
  description <- html_node(museum_page, "meta[name='description']") %>% html_attr("content")

  titles <- c(titles, title)
  addresses <- c(addresses, address)
  links <- c(links, full_link)
  descriptions <- c(descriptions, description)
}

df <- data.frame(
  Название = titles,
  Адрес = addresses,
  Ссылка = links,
  Описание = descriptions,
  stringsAsFactors = FALSE
)

View(df)
