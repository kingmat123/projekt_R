#' Wczytanie danych sprzedażowych
#'
#' Funkcja wczytuje trzy pliki CSV: dane sprzedaży, dane sklepów oraz dane o świętach. Następnie łączy je w jedną tabele analityczną.
#'
#' @param train_path ścieżka do pliku train.csv
#' @param stores_path ścieżka do pliku stores.csv
#' @param holidays_path ścieżka do pliku holidays_events.csv
#' @return tabela z danymi sprzedażowymi
#' @export
load_sales_data <- function(train_path = "data/train.csv", stores_path = "data/stores.csv", holidays_path = "data/holidays_events.csv") {
  if (!file.exists(train_path)) stop("Nie znaleziono pliku: ", train_path, call. = FALSE)
  if (!file.exists(stores_path)) stop("Nie znaleziono pliku: ", stores_path, call. = FALSE)
  if (!file.exists(holidays_path)) stop("Nie znaleziono pliku: ", holidays_path, call. = FALSE)
  train <- readr::read_csv(train_path, show_col_types = FALSE)
  stores <- readr::read_csv(stores_path, show_col_types = FALSE)
  holidays <- readr::read_csv(holidays_path, show_col_types = FALSE)
  train <- train |>
    dplyr::mutate(date = lubridate::as_date(date), store_nbr = as.integer(store_nbr), family = as.character(family), sales = as.numeric(sales), onpromotion = as.numeric(onpromotion))
  stores <- stores |> dplyr::mutate(store_nbr = as.integer(store_nbr))
  holidays <- holidays |>
    dplyr::mutate(date = lubridate::as_date(date)) |>
    dplyr::rename(holiday_type = type, holiday_locale = locale, holiday_name = description)
  train |>
    dplyr::left_join(stores, by = "store_nbr") |>
    dplyr::left_join(holidays, by = "date")
}

#' Sprawdzenie jakości danych
#'
#' Funkcja sprawdza braki, duplikaty, błędne daty, ujemną sprzedaż i brakujące dni w szeregach czasowych.
#' @param data dane sprzedażowe
#' @return lista z wynikami walidacji
#' @export
validate_sales_ts <- function(data) {
  missing_values <- colSums(is.na(data))
  duplicates <- data |> dplyr::count(date, store_nbr, family, name = "n") |> dplyr::filter(n > 1)
  negative_sales <- data |> dplyr::filter(!is.na(sales), sales < 0)
  invalid_promotions <- data |>
    dplyr::filter(!is.na(onpromotion) & onpromotion < 0)
  invalid_dates <- data |> dplyr::filter(is.na(date))
  date_gaps <- data |>
    dplyr::filter(!is.na(date)) |>
    dplyr::distinct(store_nbr, family, date) |>
    dplyr::group_by(store_nbr, family) |>
    dplyr::summarise(first_date = min(date, na.rm = TRUE), last_date = max(date, na.rm = TRUE), observed_days = dplyr::n(), expected_days = as.integer(last_date - first_date) + 1, missing_days = expected_days - observed_days, .groups = "drop") |>
    dplyr::filter(missing_days > 0)
  summary <- tibble::tibble(problem = c("braki danych", "duplikaty", "ujemna sprzedaż", "błędne daty", "brakujace dni", "niepoprawne promocje"), liczba = c(sum(missing_values), nrow(duplicates), nrow(negative_sales), nrow(invalid_dates), nrow(date_gaps), nrow(invalid_promotions)))
  list(summary = summary, missing_values = missing_values, duplicates = duplicates, negative_sales = negative_sales, invalid_dates = invalid_dates, date_gaps = date_gaps)
}

#' Czyszczenie danych sprzedażowych
#'
#' Funkcja łączy duplikaty przez sumowanie sprzedaży, uzupełnia brakujące dni wartością 0 i sortuje dane.
#' @param data dane sprzedażowe
#' @return wyczyszczona tabela
#' @export
clean_sales_ts <- function(data, aggregation = "day"){
  data |>
    dplyr::mutate(date = lubridate::as_date(date), sales = as.numeric(sales), onpromotion = as.numeric(onpromotion)) |>
    dplyr::group_by(date, store_nbr, family) |>
    dplyr::summarise(sales = sum(sales, na.rm = TRUE), onpromotion = sum(onpromotion, na.rm = TRUE), city = dplyr::first(city), state = dplyr::first(state), type = dplyr::first(type), cluster = dplyr::first(cluster), holiday_type = dplyr::first(holiday_type), holiday_name = dplyr::first(holiday_name), .groups = "drop") |>
    dplyr::group_by(store_nbr, family) |>
    tidyr::complete(date = seq(min(date), max(date), by = "day"), fill = list(sales = 0, onpromotion = 0)) |>
    tidyr::fill(city, state, type, cluster, .direction = "downup") |>
    dplyr::ungroup() |>
    dplyr::arrange(store_nbr, family, date)
  
  if (aggregation == "week") {
    data <- data |>
      dplyr::mutate(date = lubridate::floor_date(date, "week")) |>
      dplyr::group_by(date, store_nbr, family) |>
      dplyr::summarise(
        sales = sum(sales, na.rm = TRUE),
        onpromotion = sum(onpromotion, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  if (aggregation == "month") {
    data <- data |>
      dplyr::mutate(date = lubridate::floor_date(date, "month")) |>
      dplyr::group_by(date, store_nbr, family) |>
      dplyr::summarise(
        sales = sum(sales, na.rm = TRUE),
        onpromotion = sum(onpromotion, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  data
}

#' Obliczenie metryk biznesowych
#'
#' Funkcja liczy sprzedaż całkowitą, średnią, zmienność, udział promocji, średnią kroczącą, szczyty i zmiane procentową.
#' @param data wyczyszczone dane
#' @param window liczba dni do średniej kroczącej
#' @return lista z tabelą metryk i szeregiem czasowym
#' @export
compute_sales_metrics <- function(data, window = 7) {
  time_series <- data |>
    dplyr::arrange(store_nbr, family, date) |>
    dplyr::group_by(store_nbr, family) |>
    dplyr::mutate(moving_average = slider::slide_dbl(sales, mean, .before = window - 1, .complete = FALSE, na.rm = TRUE), previous_sales = dplyr::lag(sales), sales_change = sales - previous_sales, sales_change_pct = dplyr::if_else(previous_sales == 0 | is.na(previous_sales), NA_real_, 100 * sales_change / previous_sales), promotion_day = onpromotion > 0, peak = sales > dplyr::lag(sales, default = -Inf) & sales > dplyr::lead(sales, default = -Inf)) |>
    dplyr::ungroup()
  metrics <- time_series |>
    dplyr::group_by(store_nbr, family) |>
    dplyr::summarise(total_sales = sum(sales, na.rm = TRUE), average_sales = mean(sales, na.rm = TRUE), max_sales = max(sales, na.rm = TRUE), min_sales = min(sales, na.rm = TRUE), sales_sd = stats::sd(sales, na.rm = TRUE), promotion_share = mean(promotion_day, na.rm = TRUE), number_of_peaks = sum(peak, na.rm = TRUE), last_sales = dplyr::last(sales), first_sales = dplyr::first(sales), total_change_pct = dplyr::if_else(first_sales == 0, NA_real_, 100 * (last_sales - first_sales) / first_sales), .groups = "drop")
  peak_distance <- time_series |>
    dplyr::filter(peak) |>
    dplyr::group_by(store_nbr, family) |>
    dplyr::mutate(days_between_peaks = as.numeric(date - dplyr::lag(date))) |>
    dplyr::summarise(average_days_between_peaks = mean(days_between_peaks, na.rm = TRUE), .groups = "drop")
  metrics <- metrics |> dplyr::left_join(peak_distance, by = c("store_nbr", "family"))
  list(metrics = metrics, time_series = time_series)
}

#' Narysowanie trendu sprzedaży
#'
#' Funkcja tworzy wykres trendu sprzedaży z 14-dniową średnią kroczącą.
#' @param data dane sprzedażowe
#' @param store wybrany numer sklepu
#' @param category wybrana kategoria albo kategorie
#' @return wykres ggplot
#' @export
plot_sales_trends <- function(data, store = NULL, category = NULL) {
  plot_data <- data
  if (!is.null(store)) plot_data <- plot_data |> dplyr::filter(store_nbr == store)
  if (!is.null(category)) plot_data <- plot_data |> dplyr::filter(family %in% category)
  plot_data <- plot_data |>
    dplyr::group_by(date, family) |>
    dplyr::summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(family) |>
    dplyr::mutate(moving_average = slider::slide_dbl(sales, mean, .before = 13, .complete = FALSE, na.rm = TRUE)) |>
    dplyr::ungroup()
  ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = moving_average, color = family)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::labs(title = "Trend sprzedazy", subtitle = "14-dniowa srednia kroczaca", x = "Data", y = "Sprzedaz", color = "Kategoria") +
    ggplot2::theme_minimal()
}

#' Analiza dla wybranych metadanych
#'
#' Funkcja filtruje dane, liczy metryki i tworzy wykres.
#' @param data dane sprzedażowe
#' @param city miasto
#' @param state stan albo region
#' @param type typ sklepu
#' @param category kategoria
#' @param date_from data początkowa
#' @param date_to data końcowa
#' @return lista: dane, metryki, wykres
#' @export
sales_ts_logic <- function(data, city = NULL, state = NULL, type = NULL, category = NULL, date_from = NULL, date_to = NULL) {
  selected <- data
  if (!is.null(city)) selected <- selected |> dplyr::filter(city %in% !!city)
  if (!is.null(state)) selected <- selected |> dplyr::filter(state %in% !!state)
  if (!is.null(type)) selected <- selected |> dplyr::filter(type %in% !!type)
  if (!is.null(category)) selected <- selected |> dplyr::filter(family %in% !!category)
  if (!is.null(date_from)) selected <- selected |> dplyr::filter(date >= lubridate::as_date(date_from))
  if (!is.null(date_to)) selected <- selected |> dplyr::filter(date <= lubridate::as_date(date_to))
  if (nrow(selected) == 0) stop("Brak danych dla wybranych filtrow.", call. = FALSE)
  metrics <- compute_sales_metrics(selected)
  plot <- plot_sales_trends(selected)
  list(data = selected, metrics = metrics, plot = plot)
}

#' Utworzenie podsumowania dla menagera
#'
#' Funkcja tworzy ranking sklepów, ranking kategorii, kategorie rosnące i spadające oraz ocenę promocji.
#' @param data dane sprzedażowe
#' @return lista z rankingami i tekstem podsumowania
#' @export
create_management_summary <- function(data) {
  store_ranking <- data |>
    dplyr::group_by(store_nbr, city, state, type) |>
    dplyr::summarise(total_sales = sum(sales, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total_sales))
  
  category_ranking <- data |>
    dplyr::group_by(family) |>
    dplyr::summarise(total_sales = sum(sales, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total_sales))
 
  monthly_category <- data |>
    dplyr::mutate(month = lubridate::floor_date(date, "month")) |>
    dplyr::group_by(family, month) |>
    dplyr::summarise(monthly_sales = sum(sales, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(family, month) |>
    dplyr::group_by(family) |>
    dplyr::mutate(previous_month = dplyr::lag(monthly_sales), change_pct = dplyr::if_else(previous_month == 0 | is.na(previous_month), NA_real_, 100 * (monthly_sales - previous_month) / previous_month)) |>
    dplyr::ungroup()
  
  fastest_growing_category <- monthly_category |> dplyr::filter(!is.na(change_pct)) |> dplyr::arrange(dplyr::desc(change_pct)) |> dplyr::slice(1)
  
  largest_drop <- monthly_category |> dplyr::filter(!is.na(change_pct)) |> dplyr::arrange(change_pct) |> dplyr::slice(1)
  
  promotion_effect <- data |>
    dplyr::mutate(promotion_day = onpromotion > 0) |>
    dplyr::group_by(promotion_day) |>
    dplyr::summarise(average_sales = mean(sales, na.rm = TRUE), total_sales = sum(sales, na.rm = TRUE), observations = dplyr::n(), .groups = "drop")
  
  last_30_days_avg <- data |>
    dplyr::filter(date >= max(date, na.rm = TRUE) - 30) |>
    dplyr::summarise(
      avg_sales = mean(sales, na.rm = TRUE)
    ) |>
    dplyr::pull(avg_sales)
  
  text <- paste(
    "Podsumowanie menedżerskie:",
    paste0("• Najlepszy sklep: ", store_ranking$store_nbr[1]),
    paste0("• Najsłabszy sklep: ", store_ranking$store_nbr[nrow(store_ranking)]),
    paste0("• Top kategoria: ", category_ranking$family[1]),
    paste0("• Najszybszy wzrost: ", fastest_growing_category$family[1]),
    paste0("• Największy spadek: ", largest_drop$family[1]),
    paste0("• Śr. sprzedaż 30 dni: ", round(last_30_days_avg, 2)),
    sep = "\n"
  )
  
  list(
    text = text,
    last_30_days_avg = last_30_days_avg,
    store_ranking = store_ranking,
    category_ranking = category_ranking,
    fastest_growing_category = fastest_growing_category,
    largest_drop = largest_drop,
    promotion_effect = promotion_effect
  )
}

#' Tworzenie prognozy sprzedażowej ARIMA i Prophet
#'
#' Funkcja prognozuje sprzedaż dla wybranego sklepu i kategorii. Domyślnie wykorzystuje ARIMA oraz Prophet
#' @param data dane sprzedażowe
#' @param store numer sklepu
#' @param category kategoria produktu
#' @param horizon liczba dni prognozy
#' @param use_prophet czy uruchamiać model Prophet
#' @param history_days liczba ostatnich dni historii na wykresie
#' @return lista z danymi, modelami, prognozami, porównaniem i wykresem
#' @export
create_prognosis <- function(data, store, category, horizon = 30, use_prophet = TRUE, history_days = 120) {
  selected <- data |>
    dplyr::filter(store_nbr == store, family == category) |>
    dplyr::group_by(date) |>
    dplyr::summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(date)
  if (nrow(selected) < 60) stop("Za malo danych do prognozy. Wybierz inny sklep albo kategorie.", call. = FALSE)
  ts_data <- stats::ts(selected$sales, frequency = 7)
  arima_model <- forecast::auto.arima(ts_data)
  arima_forecast <- forecast::forecast(arima_model, h = horizon)
  future_dates <- seq(max(selected$date) + 1, by = "day", length.out = horizon)
  arima_result <- tibble::tibble(date = future_dates, method = "ARIMA", forecast = as.numeric(arima_forecast$mean))
  prophet_model <- NULL
  forecasts <- arima_result
  if (isTRUE(use_prophet)) {
    prophet_data <- selected |> dplyr::transmute(ds = as.Date(date), y = sales)
    prophet_model <- prophet::prophet(prophet_data, weekly.seasonality = TRUE, yearly.seasonality = TRUE, daily.seasonality = FALSE)
    future <- prophet::make_future_dataframe(prophet_model, periods = horizon)
    prophet_forecast <- stats::predict(prophet_model, future)
    prophet_result <- prophet_forecast |>
      dplyr::filter(ds > max(prophet_data$ds)) |>
      dplyr::transmute(date = as.Date(ds), method = "Prophet", forecast = as.numeric(yhat))
    forecasts <- dplyr::bind_rows(arima_result, prophet_result)
  }
  comparison <- forecasts |>
    dplyr::group_by(method) |>
    dplyr::summarise(total_forecast = sum(forecast, na.rm = TRUE), average_forecast = mean(forecast, na.rm = TRUE), .groups = "drop")
  history_for_plot <- selected |> dplyr::filter(date >= max(date) - history_days)
  plot <- ggplot2::ggplot() +
    ggplot2::geom_line(data = history_for_plot, ggplot2::aes(x = date, y = sales), color = "grey50", linewidth = 0.8) +
    ggplot2::geom_line(data = forecasts, ggplot2::aes(x = date, y = forecast, color = method), linewidth = 1.2) +
    ggplot2::geom_point(data = forecasts, ggplot2::aes(x = date, y = forecast, color = method), size = 1.4) +
    ggplot2::labs(title = paste("Prognoza sprzedazy:", category, "- sklep", store), subtitle = paste("Ostatnie", history_days, "dni historii i", horizon, "dni prognozy"), x = "Data", y = "Sprzedaz", color = "Metoda") +
    ggplot2::theme_minimal()
  list(data = selected, arima_model = arima_model, prophet_model = prophet_model, forecasts = forecasts, comparison = comparison, plot = plot)
}
