# Γράψε καθαρά εισαγωγικά σχόλια για κάθε βιβλιοθήκη
library(dplyr)      # Χρησιμοποίησε dplyr για μετασχηματισμούς δεδομένων
library(purrr)      # Εφάρμοσε συναρτήσεις σε lists για functional programming
library(tidyr)      # Οργάνωσε δεδομένα σε tidy μορφή
library(timetk)     # Διαχειρίσου χρονοσειρές σε tidy περιβάλλον
library(forecast)   # Εφάρμοσε παραδοσιακά μοντέλα πρόβλεψης
library(fable)      # Χρησιμοποίησε μοντέλα για tidy forecasting
library(fable.prophet) # Ενσωμάτωσε Prophet σε fable workflow
library(fabletools) # Παρείχε υποδομή για μοντέλα fable
library(tsibble)    # Μετέτρεψε δεδομένα σε tsibble για χρονοσειρές

# Ορισε συνάρτηση για να εντοπίσεις την τελευταία πώληση κάθε σειράς
last_sale <- function(sales) {
  tmp <- unnest(sales, cols = c(date, quantity)) %>%   # Ξεδίπλωσε nested πίνακες
    as_tsibble(index = date) %>%                      # Μετέτρεψε σε tsibble με index την ημερομηνία
    filter(quantity > 0) %>%                          # Κράτησε μόνο ημέρες με πωλήσεις
    arrange(date) %>%                                 # Ταξινόμησε χρονικά
    slice_tail() %>%                                  # Επίλεξε την τελευταία ημέρα με πώληση
    select(date) %>%                                  # Απόδωσε μόνο την ημερομηνία
    unlist() %>%                                      # Μετέτρεψε σε vector
    as.Date.numeric(origin = "1970-01-01")          # Μετέτρεψε από numeric σε ημερομηνία
  
  a <- unnest(sales, cols = c(date, quantity)) %>%    # Ξεδίπλωσε ξανά τα δεδομένα
    as_tsibble(index = date) %>%                       # Δήλωσε χρονοσειρά
    filter(date <= tmp)                                # Κράτησε μόνο μέχρι την τελευταία πώληση
  
  return(nest(a))                                     # Φώλιασε ξανά τα δεδομένα
}

# Ορισε συνάρτηση για εφαρμογή μοντέλων πρόβλεψης
applyforecastmodels <- function(sales) {
  tmp <- unnest(sales_TRAINING[[2]][[1]], cols = c(series_id, quantity, dayofweek, monthname)) %>%
    as_tsibble(index = series_id) %>%                 # Δήλωσε index χρονοσειράς
    model(                                            # Εφάρμοσε πολλαπλά μοντέλα
      ets = ETS(quantity),                            # Τρέξε ETS
      arima = ARIMA(quantity),                        # Τρέξε ARIMA
      tslm = TSLM(quantity ~ trend(4) + season(12)),  # Τρέξε TSLM
      cronston = CROSTON(quantity),                   # Εφάρμοσε Croston για αραιές πωλήσεις
      cronston_variant = CROSTON(quantity, type = "sbj"), # Εφάρμοσε παραλλαγή Croston
      nn = NNETAR(quantity, simulate = FALSE, times = 0)   # Εφάρμοσε νευρωνικό μοντέλο χρονοσειράς
    )
  return(tmp)                                         # Απόδωσε τα μοντέλα
}

# Ορισε συνάρτηση αξιολόγησης μοντέλων με test set
forecast_test_set <- function(sales, mdl) {
  tmp <- sales %>%
    as_tsibble(index = date) %>%                      # Μετέτρεψε σε tsibble για forecast
    fill_gaps(quantity = 0, .full = TRUE)             # Συμπλήρωσε κενές μέρες με μηδενικές πωλήσεις
  
  a <- forecast(mdl) %>% accuracy(tmp)                # Υπολόγισε ακρίβεια μοντέλου
  return(a)                                           # Απόδωσε αποτελέσματα
}

# Φόρτωσε αρχεία δεδομένων
res <- readr::read_csv2("data/export_sales.txt")          # Φόρτωσε πωλήσεις
OperationDays <- readr::read_delim("data/export_OperationDays.txt", delim = ";") # Φόρτωσε ημέρες λειτουργίας

rm(OperationDays)

# Δημιούργησε πλήρη πίνακα ημερομηνιών-καταστημάτων-SKU
sales <- tidyr::expand(res, date, store_code, retailer_sku) %>%
  left_join(res, by = c("store_code" = "store_code", "date" = "date", "retailer_sku" = "retailer_sku")) %>%
  mutate(quantity = replace(quantity, is.na(quantity), 0)) %>%
  left_join(res, by = c("store_code" = "store_code", "date" = "date", "retailer_sku" = "retailer_sku")) %>%
  select(1:3, 5) %>%
  rename(quantity = quantity.x) %>%
  select(-2) %>%
  mutate(date = lubridate::as_date(date))

# Οργάνωσε δεδομένα ανά SKU και υπολόγισε χαρακτηριστικά
sales <- sales %>%
  group_by(retailer_sku) %>%
  arrange(date) %>%
  mutate(series_id = row_number(),
         dayofweek = as.factor(lubridate::wday(date)),
         monthname = as.factor(lubridate::month(date)))

five_skus <- unique(sales$retailer_sku)[1:5]   # Βρες 5 πρώτα SKU
sales <- sales %>% filter(retailer_sku %in% five_skus)   # Φίλτραρε μόνο αυτά

# Εντόπισε προϊόντα με επαρκές ιστορικό
inout <- sales %>%
  filter(quantity > 0) %>%
  group_by(retailer_sku) %>%
  summarise(first_date = min(date), last_date = max(date)) %>%
  mutate(datenum = difftime(last_date, first_date, units = "days")) %>%
  filter(datenum > 21) %>%
  mutate(training = 0.95 * datenum,
         validating = 0.05 * datenum) %>%
  mutate(last_date_training = first_date + training,
         first_date_validating = first_date + training + 1) %>%
  select(-c(4,5,6)) %>%
  relocate(last_date_training, .after = first_date) %>%
  relocate(last_date, .after = first_date_validating)

rm(res)

# Δημιούργησε training set
sales_TRAINING <- sales %>%
  inner_join(inout, by = c("retailer_sku" = "retailer_sku")) %>%
  filter((date >= first_date) & (date <= last_date_training)) %>%
  select(-c(1,9,10)) %>%
  as_tsibble(key = retailer_sku, index = series_id) %>%
  tidyr::nest(., ts_tbl = c(series_id, quantity, dayofweek, monthname)) %>%
  select(1,4)

# Δημιούργησε validation set
sales_VALIDATING <- sales %>%
  inner_join(inout, by = c("retailer_sku" = "retailer_sku")) %>%
  filter((date >= first_date_validating) & (date <= last_date)) %>%
  select(-c(1,7,8)) %>%
  as_tsibble(key = retailer_sku, index = series_id, regular = FALSE) %>%
  tidyr::nest(., ts_tbl = c(series_id, quantity, dayofweek, monthname))

# Ρύθμισε παράλληλη εκτέλεση
library(furrr)
furrr_options(seed = TRUE)
plan(multisession, workers = 4)

# Εφάρμοσε μοντέλα σε όλα τα SKU
fit_models_ts <- sales_TRAINING %>%
  mutate(modl_fit = future_map(ts_tbl, applyforecastmodels, .options = furrr_options(seed = TRUE))) %>%
  unnest(modl_fit) %>%
  select(-c(ts_tbl)) %>%
  as_mable(key = retailer_sku, model = c("ets", "arima", "tslm", "cronston", "cronston_variant", "nn")) %>%
  pivot_longer(!retailer_sku, names_to = "model", values_to = "model_results")

gc()

# Υπολόγισε σφάλματα μοντέλων
fit_models_ts <- fit_models_ts %>%
  mutate(residuals = future_map(model_results, accuracy, .options = furrr_options(seed = TRUE))) %>%
  unnest(residuals) %>%
  select(-c(8,9))







