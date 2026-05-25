# ============================================================
# PELNY WORKFLOW PROJEKTU
# ============================================================
# Uruchomienie: source("workflow.R")

library(devtools)
load_all(".")

library(dplyr)
library(ggplot2)

sales_data <- load_sales_data()
validation <- validate_sales_ts(sales_data)
print(validation$summary)

clean_data <- clean_sales_ts(sales_data)
metrics <- compute_sales_metrics(clean_data)
print(head(metrics$metrics, 10))

trend_plot <- plot_sales_trends(clean_data, store = 1, category = c("GROCERY I", "BEVERAGES"))
print(trend_plot)

logic_result <- sales_ts_logic(clean_data, city = "Quito", type = "D", category = "GROCERY I", date_from = "2017-01-01", date_to = "2017-08-15")
print(head(logic_result$metrics$metrics, 10))
print(logic_result$plot)

manager <- create_management_summary(clean_data)
cat(manager$text, "\n")
print(head(manager$store_ranking, 10))
print(head(manager$category_ranking, 10))
print(manager$promotion_effect)

fast_prognosis <- create_prognosis(clean_data, store = 1, category = "GROCERY I", horizon = 30, use_prophet = FALSE)
print(fast_prognosis$comparison)
print(fast_prognosis$plot)

# Pelna prognoza poziomu 3: ARIMA + Prophet.
# Prophet moze dzialac wolniej, dlatego jest zakomentowany w workflow.
# full_prognosis <- create_prognosis(clean_data, store = 1, category = "GROCERY I", horizon = 30, use_prophet = TRUE)
# print(full_prognosis$comparison)
# print(full_prognosis$plot)

# Generowanie raportu PDF:
# rmarkdown::render("raport/raport.Rmd")
