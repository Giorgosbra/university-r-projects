# ==============================================================================
# 1_Data clean and transform.R
# Προετοιμασία δεδομένων για Παλινδρόμηση
# ==============================================================================

library(tidyverse)
library(stringr)

# 1. Φόρτωση
if(file.exists("data/movies_raw.RData")) {
  load("data/movies_raw.RData")
} else {
  stop("Δεν βρέθηκε το αρχείο movies_raw.RData. Τρέξε πρώτα το Step 0!")
}

message("Αρχικές εγγραφές: ", nrow(movies_df))

# 2. Βασικός Καθαρισμός
movies_clean <- movies_df %>%
  distinct(imdb_id, .keep_all = TRUE) %>% # Αφαίρεση διπλότυπων
  filter(!is.na(rating)) %>%               # Η ταινία πρέπει να έχει βαθμολογία
  mutate(
    runtime = as.numeric(runtime),
    year = as.numeric(year),
    votes = as.numeric(votes),
    budget = as.numeric(budget),
    gross = as.numeric(gross)
  )

# 3. One-Hot Encoding για τα Genres
# Μετατροπή του "Action, Drama" σε ξεχωριστές στήλες G_Action, G_Drama
genre_dummies <- movies_clean %>%
  select(imdb_id, genres) %>%
  separate_rows(genres, sep = ",\\s*") %>%
  mutate(genres = ifelse(is.na(genres) | genres == "", "Unknown", genres)) %>%
  # Καθαρισμός ονόματος (π.χ. Sci-Fi -> SciFi)
  mutate(genres = paste0("G_", str_replace_all(genres, "[^a-zA-Z0-9]", ""))) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = genres, values_from = value, values_fill = 0) %>%
  group_by(imdb_id) %>%
  summarise(across(everything(), max))

# Ένωση με το αρχικό dataset
movies_final <- left_join(movies_clean, genre_dummies, by = "imdb_id")

# 4. Επιλογή Τελικών Μεταβλητών & Διαχείριση NA
# Κρατάμε μόνο ταινίες που έχουν τα βασικά οικονομικά/reviews στοιχεία
movies_filled <- movies_final %>%
  select(
    imdb_id, title, rating, votes, year, runtime, 
    popularity, metascore, user_reviews, critic_reviews, 
    budget, gross, 
    starts_with("G_") # Όλα τα είδη
  ) %>%
  # Αντικατάσταση NA σε popularity/reviews με 0 (αν δεν υπάρχουν)
  mutate(
    user_reviews = replace_na(user_reviews, 0),
    critic_reviews = replace_na(critic_reviews, 0),
    popularity = replace_na(popularity, 0)
  ) %>%
  # Αφαιρούμε ταινίες που δεν έχουν Budget, Gross ή Metascore 
  na.omit()

message("Ταινίες έτοιμες για μοντέλο (Clean): ", nrow(movies_filled))

# 5. Αποθήκευση
save(movies_filled, file = "data/movies_clean.RData")