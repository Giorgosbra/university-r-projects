# ==============================================================================
# 2_Linear regression.R
# Μοντέλο Παλινδρόμησης & Ανάλυση
# ==============================================================================

library(tidyverse)
library(caret) 

options(scipen = 999)

# 1. Φόρτωση
load("data/movies_clean.RData")

# Αφαιρούμε στήλες που δεν είναι αριθμητικές (ID, Title)
reg_data <- movies_filled %>%
  select(-imdb_id, -title) %>%
  # Αφαιρούμε στήλες ειδών που είναι παντού 0 (αν υπάρχουν)
  select(where(~ var(.) != 0))

message("Δεδομένα Ανάλυσης: ", nrow(reg_data))

# --- Α. Full Linear Model ---
# Στόχος: Πρόβλεψη του 'rating' βάσει όλων των άλλων
model_full <- lm(rating ~ ., data = reg_data)

message("\n--- Summary Full Model ---")
summary(model_full)

# --- Β. Stepwise Selection (Βελτιστοποίηση) ---
# Αφαιρεί αυτόματα τις μη σημαντικές μεταβλητές
message("\n--- Stepwise Optimization ---")
model_opt <- step(model_full, direction = "both", trace = 0)
summary(model_opt)

# --- Γ. Caret Cross-Validation ---
message("\n--- Caret 10-fold CV ---")
train_control <- trainControl(method = "cv", number = 10)

caret_model <- train(rating ~ ., data = reg_data, method = "lm", trControl = train_control)
print(caret_model)

message("RMSE: ", round(caret_model$results$RMSE, 3))
message("R-squared: ", round(caret_model$results$Rsquared, 3))

# Αποθήκευση αποτελεσμάτων σε αρχείο text
capture.output(summary(model_opt), file = "regression_results.txt")