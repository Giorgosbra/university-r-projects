# Ερώτημα 1: Καθαρισμός δεδομένων


# Libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

# Διαβάζουμε τα αρχεία
sales <- read_delim("data/Sales_four_stores_oneyearperiod.csv", delim = ";")
categories <- read_delim("data/Categories.csv", delim = ";")

# Μετονομασία στηλών
colnames(sales) <- c("basket_id","store","date_trns","ean","quantity","price")

# Μετατροπή τύπου ημερομηνίας
sales <- sales %>%
  mutate(date_trns = as.Date(date_trns, format = "%Y-%m-%d"))

# Ensure matching types
sales <- sales %>% mutate(ean = as.character(ean))
categories <- categories %>% mutate(EAN = as.character(EAN))

# Έλεγχος NA
colSums(is.na(sales))
colSums(is.na(categories))

# Αφαίρεση duplicates
sales <- sales %>% distinct()
categories <- categories %>% distinct()

# Φιλτράρισμα αρνητικών ή μηδενικών values
n_before <- nrow(sales)
sales <- sales %>% filter(quantity > 0, price >= 0)
n_after <- nrow(sales)
n_removed <- n_before - n_after
n_removed  # Πόσες γραμμές αφαιρέθηκαν

# Φιλτράρισμα ακραίων τιμών (outliers)
sales <- sales %>% filter(quantity <= 1000, price <= 1000)

# Check μετά το cleaning
summary(sales$quantity)
summary(sales$price)
colSums(is.na(sales))

# Έλεγχος συμβατότητας EAN με categories
missing_ean <- setdiff(sales$ean, categories$EAN)
length(missing_ean)



# Ερώτημα 2: Χρησιμοποιώντας τα δεδομένα του Categories.csv να προταθεί μια νέα μεταβλητή και να εξεταστεί αν μπορεί άμεσα να χρησιμοποιηθεί.


# Συνολική “category path” μεταβλητή
categories <- categories %>%
  mutate(full_category = paste(Cat1_name, Cat2_name, Cat3_name, Cat4_name, sep = " > "))  # Usable για group_by, ggplot, count, summary

head(categories$full_category)

# Μπορεί να χρησιμοποιηθεί άμεσα για ομαδοποιήσεις, φίλτρα, ή visualizations.


# Ερώτημα 3: Ποια ημέρα του έτους έγιναν οι περισσότερες εισπράξεις και σε ποια οι λιγότερες εισπράξεις; Να δοθεί μια ερμηνεία.


# Συνολική αξία ανά ημέρα
daily_revenue <- sales %>%
  group_by(date_trns) %>%
  summarise(total_revenue = sum(price)) %>%
  arrange(desc(total_revenue))

# Ημέρα με μέγιστη αξία
max_day <- daily_revenue[which.max(daily_revenue$total_revenue), ]

# Ημέρα με ελάχιστη αξία
min_day <- daily_revenue[which.min(daily_revenue$total_revenue), ]

max_day
min_day

# Οπτικοποίηση με ggplot2
ggplot(daily_revenue, aes(x = date_trns, y = total_revenue)) +
  geom_line(color = "steelblue") +
  labs(title = "Ημερήσια Έσοδα", x = "Ημερομηνία", y = "Συνολικά Έσοδα") +
  theme_minimal()

# Ερμηνία: Η ανάλυση των ημερήσιων εσόδων δείχνει μεγάλη διακύμανση. Η υψηλότερη ημέρα τζίρου ήταν κοντά στις γιορτές, υποδηλώνοντας seasonal peaks, ενώ η χαμηλότερη ημέρα αντιστοιχεί σε περίοδο με πολύ λίγες συναλλαγές, πιθανώς λόγω αργιών ή τεχνικών προβλημάτων καταγραφής.


#Ερώτημα 4: Να απεικονισθεί με ggplot τα εβδομαδιαία έσοδα κάθε καταστήματος για τους τρεις τελευταίους μήνες του χρονικού ορίζοντα.


# Βρίσκουμε το max date στο dataset
max_date <- max(sales$date_trns)

# Υπολογίζουμε την ημερομηνία 3 μήνες πριν
start_date <- max_date %m-% months(3)

# Φιλτράρουμε τα δεδομένα
sales_last3m <- sales %>%
  filter(date_trns >= start_date & date_trns <= max_date)

# Προσθέτουμε εβδομάδα και ομαδοποιούμε ανά κατάστημα
weekly_revenue <- sales_last3m %>%
  mutate(week = floor_date(date_trns, unit = "week")) %>%  # week starting Monday
  group_by(store, week) %>%
  summarise(total_revenue = sum(price), .groups = "drop")

# Δημιουργία ggplot
ggplot(weekly_revenue, aes(x = week, y = total_revenue, color = as.factor(store))) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Εβδομαδιαία Έσοδα ανά Κατάστημα (Τελευταίοι 3 Μήνες)",
    x = "Εβδομάδα",
    y = "Συνολικά Έσοδα",
    color = "Κατάστημα"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Ερώτημα 5: Ποια είναι τα 10 top selling προϊόντα ανά μήνα σε κάθε κατάστημα. 


# Προσθέτουμε μήνα και έτος
sales <- sales %>%
  mutate(year_month = floor_date(date_trns, unit = "month"))

# Ομαδοποιούμε ανά κατάστημα, μήνα και προϊόν, υπολογίζουμε συνολική ποσότητα
monthly_top <- sales %>%
  group_by(store, year_month, ean) %>%
  summarise(total_quantity = sum(quantity), .groups = "drop")

# Βρίσκουμε τα top 10 προϊόντα ανά μήνα και κατάστημα
top10_monthly <- monthly_top %>%
  group_by(store, year_month) %>%
  slice_max(order_by = total_quantity, n = 10) %>%
  arrange(store, year_month, desc(total_quantity))

# Προβολή αποτελεσμάτων
top10_monthly


# Έλεγχος αν κάθε store/μήνας έχει 10 γραμμές
check_rows <- top10_monthly %>%
  group_by(store, year_month) %>%
  summarise(n_rows = n()) %>%
  ungroup()

if(any(check_rows$n_rows != 10)){
  cat("Προσοχή: Υπάρχουν store/μήνες με !=10 προϊόντα\n")
  print(check_rows %>% filter(n_rows != 10))
} else {
  cat("Ολα τα store/μήνες έχουν σωστά 10 προϊόντα\n")
}

# Υπάρχει ισοπαλία στην 10η θέση δηλαδή δύο προϊόντα έχουν ίδια ποσότητα

# Έλεγχος ότι top10 καλύπτουν σημαντικό μέρος των πωλήσεων
top10_percent <- top10_monthly %>%
  group_by(store, year_month) %>%
  summarise(top10_sum = sum(total_quantity)) %>%
  left_join(
    monthly_top %>% group_by(store, year_month) %>% summarise(total_sum = sum(total_quantity)),
    by = c("store", "year_month")
  ) %>%
  mutate(percent_top10 = top10_sum / total_sum * 100)

# Προβολή των ποσοστών
top10_percent

# Quick preview top προϊόντων για sanity check
top10_monthly %>%
  group_by(store, year_month) %>%
  slice_head(n = 5) %>%
  select(store, year_month, ean, total_quantity)


#Ερώτημα 6: Δοθέντος ότι ένα καλάθι περιέχει γάλα (ή κάποιο γαλακτοκομικό προϊόν), να ελεγχθεί η κατανομή πιθανότητας των άλλων ειδών.


# Καθαρίζουμε τα EANs από spaces, αφαιρούμε κενά πριν/μετά
sales <- sales %>%
  mutate(ean = str_trim(ean))

categories <- categories %>%
  mutate(EAN = str_trim(EAN))

# Merge μόνο το Cat2_name
sales_cat2 <- sales %>%
  left_join(categories %>% select(EAN, Cat2_name), by = c("ean" = "EAN"))

# Έλεγχος
head(sales_cat2)

# Βρίσκουμε τα basket_id που περιέχουν γαλακτοκομικά
milk_baskets <- sales_cat2 %>%
  filter(Cat2_name == "ΓΑΛΑΚΤΟΚΟΜΙΚΑ") %>%
  pull(basket_id) %>%
  unique()

# Φιλτράρουμε όλα τα προϊόντα που εμφανίζονται σε αυτά τα καλάθια
milk_basket_products <- sales_cat2 %>%
  filter(basket_id %in% milk_baskets)

# Αφαιρούμε τα γαλακτοκομικά
other_products <- milk_basket_products %>%
  filter(Cat2_name != "ΓΑΛΑΚΤΟΚΟΜΙΚΑ")

# Υπολογίζουμε κατανομή πιθανότητας
product_distribution <- other_products %>%
  group_by(Cat2_name) %>%
  summarise(count = n()) %>%
  mutate(probability = count / sum(count)) %>%
  arrange(desc(probability))

# Προβολή αποτελεσμάτων
product_distribution

# Οπτικοποίση με ggplot2
ggplot(product_distribution, aes(x = reorder(Cat2_name, -probability), y = probability)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Κατανομή πιθανοτήτων άλλων ειδών σε καλάθια με γαλακτοκομικά",
    x = "Κατηγορία προϊόντος",
    y = "Πιθανότητα"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Ερώτημα 7: Ποια είναι η συλλογή προϊόντων (επίπεδο φορολογικού κωδικού) που είναι κοινή μεταξύ των καταστημάτων. 


# Merge μόνο την full_category
sales_cat <- sales %>%
  left_join(categories %>% select(EAN, full_category),
            by = c("ean" = "EAN"))

# Ομαδοποιούμε ανά προϊόν (EAN) και μετράμε σε πόσα καταστήματα εμφανίζεται
product_stores <- sales_cat %>%
  group_by(ean, full_category) %>%
  summarise(n_stores = n_distinct(store), .groups = "drop")

# Φιλτράρουμε τα προϊόντα που εμφανίζονται σε όλα τα καταστήματα
common_products <- product_stores %>%
  filter(n_stores == 4)

# Προβολή πρώτων 10 προϊόντων
head(common_products, 10)

# Αφαίρεση γραμμών με NA, καθώς δεν υπάρχει αντιστοίχιση μεταξύ των ΕΑΝ των 2 data sets 
sales_cat <- sales_cat %>%
  filter(!is.na(full_category))

# Έλεγχος ότι δεν υπάρχουν NA πλέον
sum(is.na(sales_cat$full_category))


#Ερώτημα 8: Υπάρχει η πεποίθηση ότι όσο μεγαλύτερη ποικιλία έχει ένα καλάθι, τόσο υψηλότερη είναι και η αξία του. Υπάρχουν δεδομένα που να συνηγορούν σε αυτή την άποψη? 


# Υπολογίζουμε για κάθε καλάθι:
# - Την ποικιλία (αριθμός διαφορετικών EAN)
# - Την συνολική αξία (sum τιμών)
basket_summary <- sales_cat %>%
  group_by(basket_id) %>%
  summarise(
    variety = n_distinct(ean),
    total_value = sum(price),
    .groups = "drop"
  )

# Ελέγχουμε συσχέτιση με correlation
correlation <- cor(basket_summary$variety, basket_summary$total_value)
cat("Συσχέτιση ποικιλίας vs αξίας καλαθιού:", correlation, "\n")

# Ομαδοποιούμε την ποικιλία σε categories για πιο καθαρό plot (προαιρετικό)
basket_summary <- basket_summary %>%
  mutate(variety_group = factor(variety))

# Box plot: αξία καλαθιού ανά επίπεδο ποικιλίας
ggplot(basket_summary, aes(x = variety_group, y = total_value)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Αξία καλαθιού ανά επίπεδο ποικιλίας προϊόντων",
    x = "Ποικιλία προϊόντων (διαφορετικά EAN)",
    y = "Συνολική αξία καλαθιού"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Η συσχέτιση ~0.65 δείχνει μέτρια-ισχυρή θετική σχέση:
# Όσο μεγαλύτερη η ποικιλία προϊόντων σε ένα καλάθι, τόσο μεγαλύτερη η αξία του καλαθιού.
# Αυτό στηρίζει την πεποίθηση που αναφέρεται στο ερώτημα.
# H συσχέτιση δεν συνεπάγεται αιτιότητα


# Ερώτημα 9: Όσοι δεν πιστεύουν το προηγούμενο, εκτιμούν πως η αξία του καλαθιού στο μεγαλύτερο ποσοστό καθορίζεται από τα τρόφιμα. 


basket_unique_products <- sales_cat %>%
  select(basket_id, ean, full_category, price) %>%
  distinct() %>%
  mutate(is_food = str_detect(full_category, "ΤΡΟΦΙΜ"))

# Μαρκάρουμε τα προϊόντα που είναι τρόφιμα
sales_cat_food <- sales_cat %>%
  mutate(is_food = str_detect(full_category, "ΤΡΟΦΙΜ"))

# Υπολογίζουμε για κάθε καλάθι
food_ratio <- sales_cat_food %>%
  group_by(basket_id) %>%
  summarise(
    total_products = n_distinct(ean),
    food_products = n_distinct(ean[is_food]),
    food_ratio = food_products / total_products
  )

# Αποτελέσματα (πρώτες γραμμές)
head(food_ratio)

basket_value <- basket_unique_products %>%
  group_by(basket_id) %>%
  summarise(
    basket_value = sum(price, na.rm = TRUE)
  )

basket_data <- food_ratio %>%
  left_join(basket_value, by = "basket_id")

model <- lm(basket_value ~ food_ratio, data = basket_data)
summary(model)

# t-value = 81.51 (γιγαντιαίο)
# p-value < 2e-16 (πρακτικά 0)
# Άρα υπάρχει πραγματική, στατιστικά ισχυρή σχέση.
# Τα τρόφιμα έχουν θετική, ξεκάθαρη και στατιστικά σημαντική επίδραση, αλλά εξηγούν σημαντικό μέρος της αξίας όχι όμως το σύνολο.