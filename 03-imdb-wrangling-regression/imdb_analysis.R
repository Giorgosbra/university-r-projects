############################################
# IMDB DATA ANALYSIS – ASSIGNMENT 3
# Data wrangling, EDA & Regression Analysis
############################################

library(tidyverse)
library(ggplot2)
library(jtools)

# Load IMDb datasets
load("data/imdb1980.RData")

############################################
## 1. DATA CLEANING & TRANSFORMATION
############################################

# Τα δεδομένα του IMDB περιέχουν μη έγκυρες τιμές (\N),
# ακατάλληλους τύπους μεταβλητών και λεκτικές αναπαραστάσεις λιστών.
# Στο παρόν βήμα μετασχηματίζονται σε tidy μορφή.

title_basics_clean <- title.basics %>%
  mutate(
    startYear = as.numeric(startYear),
    endYear = as.numeric(endYear),
    runtimeMinutes = as.numeric(runtimeMinutes),
    isAdult = as.logical(isAdult),
    genres = str_split(genres, ",")
  )

title_crew_clean <- title.crew %>%
  mutate(
    directors = na_if(directors, "\\N"),
    writers = na_if(writers, "\\N"),
    directors = str_split(directors, ","),
    writers = str_split(writers, ",")
  )

title_principals_clean <- title.principals %>%
  mutate(
    job = na_if(job, "\\N"),
    characters = na_if(characters, "\\N")
  )

title_ratings_clean <- title.ratings %>%
  mutate(
    averageRating = as.numeric(averageRating),
    numVotes = as.numeric(numVotes)
  )

name_basics_clean <- name.basics %>%
  mutate(
    birthYear = as.numeric(birthYear),
    deathYear = as.numeric(deathYear),
    primaryProfession = str_split(primaryProfession, ","),
    knownForTitles = str_split(knownForTitles, ",")
  )

# Τα δεδομένα του IMDB δεν είναι άμεσα κατάλληλα για ανάλυση, καθώς περιέχουν λεκτικές αναπαραστάσεις λιστών, μη έγκυρες τιμές (\N) και μη κατάλληλους τύπους μεταβλητών. Στο παρόν βήμα γίνεται μετασχηματισμός των δεδομένων σε tidy μορφή, σύμφωνα με τις αρχές του tidyverse, ώστε να είναι δυνατή η περαιτέρω ανάλυση και μοντελοποίηση.

# Διαγραφή παλιών tibble
rm(name.basics)
rm(title.basics)
rm(title.crew)
rm(title.principals)
rm(title.ratings)

############################################
## 2. EXPLORATORY DATA ANALYSIS (EDA)
############################################

# Ανάλυση μόνο για ταινίες
movies <- title_basics_clean %>%
  filter(titleType == "movie", !is.na(startYear))

# Πλήθος ταινιών ανά έτος
movies %>%
  count(startYear) %>%
  ggplot(aes(startYear, n)) +
  geom_line() +
  labs(
    title = "Πλήθος ταινιών ανά έτος",
    x = "Έτος",
    y = "Αριθμός ταινιών"
  )
# Παρατηρείται αυξητική τάση στον αριθμό ταινιών, γεγονός που υποδηλώνει εντατικοποίηση της παραγωγής περιεχομένου, ιδιαίτερα τις τελευταίες δεκαετίες.Ακόμα φαίνεται μία πτώση στην παραγωγή ταινιών κατά την περίοδο της πανδημίας (2019-2021).


# Κατανομή διάρκειας ταινιών
movies %>%
  filter(runtimeMinutes > 40, runtimeMinutes < 250) %>%
  ggplot(aes(runtimeMinutes)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Κατανομή διάρκειας ταινιών",
    x = "Λεπτά",
    y = "Πλήθος ταινιών"
  )
# Οι περισσότερες ταινίες συγκεντρώνονται στο εύρος 80–120 λεπτών, γεγονός που δείχνει τυποποίηση της διάρκειας, πιθανόν λόγω εμπορικών και προγραμματιστικών περιορισμών.


# Top 10 genres
movies %>%
  unnest(genres) %>%
  filter(!is.na(genres)) %>%
  count(genres, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(reorder(genres, n), n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 genres ταινιών",
    x = "Genre",
    y = "Πλήθος ταινιών"
  )
# Ορισμένα είδη, όπως το Drama και το Comedy, εμφανίζονται σημαντικά συχνότερα, υποδηλώνοντας χαμηλότερο ρίσκο και μεγαλύτερη αποδοχή από το κοινό.


# Ratings & votes
ratings_movies <- movies %>%
  inner_join(title_ratings_clean, by = "tconst")

ratings_movies %>%
  ggplot(aes(averageRating)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Κατανομή βαθμολογιών IMDB",
    x = "Μέση βαθμολογία",
    y = "Πλήθος ταινιών"
  )
# Οι περισσότερες ταινίες συγκεντρώνονται γύρω από τη μέση βαθμολογία, γεγονός που δείχνει περιορισμένο αριθμό εξαιρετικά επιτυχημένων ή αποτυχημένων τίτλων.


ratings_movies %>%
  filter(numVotes > 1000) %>%
  ggplot(aes(numVotes, averageRating)) +
  geom_point(alpha = 0.3) +
  scale_x_log10() +
  labs(
    title = "Σχέση αριθμού ψήφων και βαθμολογίας",
    x = "Αριθμός ψήφων (log scale)",
    y = "Μέση βαθμολογία"
  )

# Οι ταινίες με μεγάλο αριθμό ψήφων τείνουν να έχουν πιο σταθερές βαθμολογίες, γεγονός που υποδηλώνει μεγαλύτερη αξιοπιστία της μέσης βαθμολογίας.

# Η διερευνητική ανάλυση δεδομένων ανέδειξε βασικά χαρακτηριστικά της κινηματογραφικής παραγωγής, όπως η συγκέντρωση των ταινιών σε συγκεκριμένα είδη και διάρκειες, καθώς και τη σχέση δημοφιλίας και αξιολόγησης. Τα ευρήματα αυτά καθοδηγούν την επιλογή μεταβλητών για την ανάπτυξη του μοντέλου παλινδρόμησης.


############################################
## 3. MOVIE-LEVEL DATA INTEGRATION
############################################

# Βασικός movie-level πίνακας
movies_base <- title_basics_clean %>%
  filter(titleType == "movie") %>%
  left_join(title_ratings_clean, by = "tconst")

# Συγκεντρωτικά μεγέθη συντελεστών
principals_summary <- title_principals_clean %>%
  filter(category %in% c("actor", "actress", "director", "writer")) %>%
  group_by(tconst, category) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = category,
    values_from = count,
    values_fill = 0
  ) %>%
  mutate(
    total_contributors = actor + actress + director + writer
  )

movies_full <- movies_base %>%
  left_join(principals_summary, by = "tconst")

movies_full <- movies_full %>%
  mutate(across(c(actor, actress, director, writer, total_contributors),
                ~replace_na(.x, 0)))

top_genres <- movies_full %>%
  unnest(genres) %>%
  count(genres, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(genres)

# Dummy μεταβλητές για βασικά genres
movies_full <- movies_full %>%
  mutate(
    is_drama = map_lgl(genres, ~ "Drama" %in% .x),
    is_comedy = map_lgl(genres, ~ "Comedy" %in% .x),
    is_action = map_lgl(genres, ~ "Action" %in% .x),
    is_thriller = map_lgl(genres, ~ "Thriller" %in% .x),
    is_romance = map_lgl(genres, ~ "Romance" %in% .x)
  )

# Κάθε γραμμή περιγράφει:

#βασικά χαρακτηριστικά ταινίας (έτος, διάρκεια, είδος)
#δείκτες δημοφιλίας και ποιότητας (rating, votes)
#συγκεντρωτικά μεγέθη συντελεστών


# Σχέση αριθμού συντελεστών και βαθμολογίας
movies_full %>%
  filter(total_contributors > 0) %>%
  ggplot(aes(total_contributors, averageRating)) +
  geom_point(alpha = 0.3) +
  labs(
    title = "Σχέση αριθμού συντελεστών και βαθμολογίας",
    x = "Σύνολο συντελεστών",
    y = "Μέση βαθμολογία"
  )
# Δεν προκύπτει ισχυρή γραμμική σχέση, γεγονός που υποδηλώνει ότι η ποιότητα δεν εξαρτάται αποκλειστικά από τον αριθμό των συντελεστών.

movies_full %>%
  filter(runtimeMinutes < 250) %>%
  ggplot(aes(runtimeMinutes, averageRating)) +
  geom_point(alpha = 0.3) +
  labs(
    title = "Διάρκεια και βαθμολογία ταινιών",
    x = "Διάρκεια (λεπτά)",
    y = "Μέση βαθμολογία"
  )

# Ο ενοποιημένος πίνακας σε επίπεδο ταινίας επιτρέπει τη συνδυαστική μελέτη χαρακτηριστικών περιεχομένου, συντελεστών και αξιολόγησης. Τα συγκεντρωτικά μεγέθη που δημιουργήθηκαν αποτελούν κατάλληλες υποψήφιες μεταβλητές για την ανάπτυξη μοντέλων παλινδρόμησης.


############################################
## 4. CONTRIBUTOR-LEVEL ANALYSIS
############################################

movie_principals <- title_principals_clean %>%
  inner_join(
    title_basics_clean %>% 
      filter(titleType == "movie") %>% 
      select(tconst, startYear),
    by = "tconst"
  )

career_summary <- movie_principals %>%
  group_by(nconst) %>%
  summarise(
    movies_count = n_distinct(tconst),
    first_year = min(startYear, na.rm = TRUE),
    last_year = max(startYear, na.rm = TRUE),
    career_length = last_year - first_year + 1,
    appearances_per_year = movies_count / career_length,
    .groups = "drop"
  )

roles_summary <- movie_principals %>%
  count(nconst, category) %>%
  pivot_wider(
    names_from = category,
    values_from = n,
    values_fill = 0
  )

contributors_full <- name_basics_clean %>%
  inner_join(career_summary, by = "nconst") %>%
  left_join(roles_summary, by = "nconst")

contributors_full <- contributors_full %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

# Movies count vs career length
contributors_full %>%
  filter(movies_count < 200) %>%
  ggplot(aes(career_length, movies_count)) +
  geom_point(alpha = 0.3) +
  labs(
    title = "Διάρκεια καριέρας και πλήθος ταινιών",
    x = "Χρόνια καριέρας",
    y = "Αριθμός ταινιών"
  )

# Παρατηρείται θετική συσχέτιση, ωστόσο η ένταση συμμετοχής διαφέρει σημαντικά μεταξύ συντελεστών.

# Appearances per year distribution
contributors_full %>%
  filter(appearances_per_year < 5) %>%
  ggplot(aes(appearances_per_year)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Κατανομή συχνότητας συμμετοχής",
    x = "Ταινίες ανά έτος",
    y = "Πλήθος συντελεστών"
  )

# Actors vs Directors activity
contributors_full %>%
  filter(actor > 0 | director > 0) %>%
  ggplot(aes(actor, director)) +
  geom_point(alpha = 0.3) +
  labs(
    title = "Σχέση υποκριτικών και σκηνοθετικών ρόλων",
    x = "Ρόλοι ηθοποιού",
    y = "Ρόλοι σκηνοθέτη"
  )

# Ο πίνακας σε επίπεδο συντελεστή αποτυπώνει ποσοτικά την επαγγελματική δραστηριότητα κάθε ατόμου, λαμβάνοντας υπόψη τόσο τη διάρκεια όσο και την ένταση της καριέρας του. Τα αποτελέσματα δείχνουν μεγάλη ετερογένεια μεταξύ των συντελεστών, γεγονός που υπογραμμίζει τη σημασία της εμπειρίας ως μεταβλητής ανάλυσης.


############################################
## 5. REGRESSION MODEL                    
############################################

reg_data <- movies_full %>%
  select(
    averageRating,
    runtimeMinutes,
    numVotes,
    total_contributors,
    startYear,
    is_drama
  ) %>%
  filter(
    !is.na(averageRating),
    !is.na(runtimeMinutes),
    !is.na(startYear),
    numVotes > 100
  )

reg_data <- reg_data %>%
  mutate(
    log_votes = log(numVotes)
  )

# Regression model (glm)
model_1 <- glm(
  averageRating ~ runtimeMinutes +
    log_votes +
    total_contributors +
    startYear +
    is_drama,
  data = reg_data
)

summ(model_1, digits = 3)

# log_votes (+0.190)
# Όσο αυξάνονται οι ψήφοι → ανεβαίνει το rating
# Οι δημοφιλέστερες ταινίες τείνουν να λαμβάνουν υψηλότερες αξιολογήσεις, πιθανώς λόγω μεγαλύτερης αποδοχής ή αυξημένης αξιοπιστίας της βαθμολογίας.

# is_dramaTRUE (+0.605)
# Οι δραματικές ταινίες έχουν κατά μέσο όρο ~0.6 μονάδες υψηλότερο rating
# Το είδος Drama σχετίζεται θετικά με τη μέση βαθμολογία, γεγονός που υποδηλώνει ότι το κοινό αξιολογεί ευνοϊκότερα περιεχόμενο με έντονο αφηγηματικό και συναισθηματικό χαρακτήρα.

# Η διάρκεια της ταινίας παρουσιάζει θετική αλλά περιορισμένη επίδραση στη βαθμολογία.

# total_contributors (-0.113)
# Περισσότεροι συντελεστές → χαμηλότερο rating
# Το αυξημένο μέγεθος παραγωγής δεν εγγυάται υψηλότερη ποιότητα και ενδέχεται να συνδέεται με λιγότερο συνεκτικό καλλιτεχνικό αποτέλεσμα.

# startYear (-0.007)
# Νεότερες ταινίες → ελαφρώς χαμηλότερα ratings
# Παρατηρείται χρονική διαφοροποίηση στις αξιολογήσεις, πιθανώς λόγω αλλαγών στα πρότυπα του κοινού ή αυξημένης αυστηρότητας στις σύγχρονες κριτικές.

# Το πολυμεταβλητό μοντέλο παλινδρόμησης δείχνει ότι η μέση βαθμολογία IMDB σχετίζεται σημαντικά με παράγοντες δημοφιλίας, είδους και χαρακτηριστικά παραγωγής. Ιδιαίτερα ισχυρή εμφανίζεται η επίδραση του αριθμού ψήφων και του είδους Drama, ενώ το μέγεθος της παραγωγής παρουσιάζει αρνητική συσχέτιση με την αξιολόγηση. Τα αποτελέσματα υπογραμμίζουν ότι η ποιότητα του περιεχομένου διαμορφώνεται από συνδυασμό παραγόντων και όχι από έναν μεμονωμένο δείκτη.


############################################
## 6. IMDB vs TMDB                    
############################################

# Φόρτωση δεδομένων TMDB
tmdb <- read_csv("data/TMDB_all_movies.csv")

# Το TMDB dataset περιλαμβάνει μεγάλο αριθμό ταινιών,
# πολλές από τις οποίες δεν έχουν ακόμη επαρκή αριθμό ψήφων.

glimpse(tmdb)

# Κατανομή βαθμολογιών TMDB (όλες οι ταινίες)
tmdb %>%
  ggplot(aes(vote_average)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Κατανομή βαθμολογιών TMDB",
    x = "Μέση βαθμολογία",
    y = "Πλήθος ταινιών"
  )

# Οι βαθμολογίες του TMDB εμφανίζουν διαφορετική κατανομή σε σχέση με το IMDB,
# γεγονός που υποδηλώνει διαφοροποιήσεις στη βάση χρηστών και στον τρόπο αξιολόγησης.

# Κατανομή βαθμολογιών TMDB χωρίς μη αξιολογημένες ταινίες
tmdb %>%
  filter(vote_average > 0) %>%
  ggplot(aes(vote_average)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Κατανομή βαθμολογιών TMDB (χωρίς μη αξιολογημένες)",
    x = "Μέση βαθμολογία",
    y = "Πλήθος ταινιών"
  )

# Παρατηρείται μεγάλη συγκέντρωση τιμών στο μηδέν στη βάση TMDB,
# η οποία οφείλεται σε ταινίες χωρίς επαρκή αριθμό ψήφων.
# Για τον λόγο αυτό, οι μη αξιολογημένες ταινίες αφαιρούνται,
# ώστε η κατανομή των βαθμολογιών να είναι συγκρίσιμη με εκείνη του IMDB.

# Σύγκριση IMDB vs TMDB

# Προετοιμασία δεδομένων IMDB
imdb_ratings <- reg_data %>%
  select(averageRating) %>%
  mutate(source = "IMDB")

# Χρησιμοποιείται το reg_data, καθώς περιλαμβάνει φίλτρο αξιοπιστίας
# (ελάχιστος αριθμός ψήφων) και καθαρές βαθμολογίες.

# Προετοιμασία δεδομένων TMDB
tmdb_ratings <- tmdb %>%
  filter(vote_average > 0) %>%
  select(vote_average) %>%
  rename(averageRating = vote_average) %>%
  mutate(source = "TMDB")

# Ενοποίηση των δύο πηγών
ratings_compare <- bind_rows(imdb_ratings, tmdb_ratings)

# Σύγκριση κατανομών βαθμολογιών
ratings_compare %>%
  ggplot(aes(averageRating, fill = source)) +
  geom_histogram(
    bins = 30,
    alpha = 0.6,
    position = "identity"
  ) +
  labs(
    title = "Σύγκριση κατανομής βαθμολογιών IMDB και TMDB",
    x = "Μέση βαθμολογία",
    y = "Πλήθος ταινιών",
    fill = "Πηγή"
  )

# Το IMDB εμφανίζει πιο συγκεντρωμένες βαθμολογίες γύρω από τη μέση τιμή,
# ενώ το TMDB παρουσιάζει μεγαλύτερη διασπορά.
# Οι διαφορές αυτές πιθανόν σχετίζονται με τη διαφορετική μεθοδολογία
# αξιολόγησης και το προφίλ των χρηστών κάθε πλατφόρμας.

#Η σύγκριση των κατανομών δείχνει ότι το IMDB παρουσιάζει πιο συγκεντρωμένες βαθμολογίες γύρω από τη μέση τιμή, γεγονός που υποδηλώνει μεγαλύτερη σταθερότητα και αυστηρότερα κριτήρια αξιολόγησης. Αντίθετα, το TMDB εμφανίζει μεγαλύτερη διασπορά, πιθανώς λόγω της πιο ανοιχτής και λιγότερο φιλτραρισμένης διαδικασίας αξιολόγησης από τους χρήστες. Οι διαφορές αυτές αναδεικνύουν τον ρόλο της μεθοδολογίας αξιολόγησης στη διαμόρφωση της αντιλαμβανόμενης ποιότητας μιας ταινίας.