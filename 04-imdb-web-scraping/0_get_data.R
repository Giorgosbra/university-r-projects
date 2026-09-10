# ======================================
# 0_Get_data.R
# IMDb Web Harvesting
# ======================================
options(scipen = 999)
library(rvest)
library(httr)
library(tidyverse)
library(jsonlite)
library(stringr)

# ---------------------------------------------------------
# 1. Ρυθμίσεις & Αρχικοποίηση
# ---------------------------------------------------------
start_id <- "tt0071562"
queue <- c(start_id)
processed_ids <- character()
all_data <- list()
max_movies <- 5000

my_headers <- add_headers(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
  `Accept-Language` = "en-US,en;q=0.9"
)

# Helper function
parse_number_clean <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(NA)
  x <- x[1]
  # Αν περιέχει K (π.χ. 1.2K), το μετατρέπουμε σε αριθμό
  multiplier <- 1
  if (str_detect(x, "K")) multiplier <- 1000
  
  clean_val <- str_replace_all(x, "[^0-9]", "")
  if (clean_val == "") return(NA)
  return(as.numeric(clean_val) * multiplier)
}

# ---------------------------------------------------------
# 2. Κύριο Loop Συλλογής (Crawl)
# ---------------------------------------------------------
while (length(queue) > 0 && length(processed_ids) < max_movies) {
  
  current_id <- queue[1]
  queue <- queue[-1]
  
  if (current_id %in% processed_ids) next
  
  message(sprintf("[%d/%d] Scraping: %s (Queue: %d)", length(processed_ids) + 1, max_movies, current_id, length(queue)))
  Sys.sleep(runif(1, 1.5, 3.0))
  
  data_step <- tryCatch({
    url <- paste0("https://www.imdb.com/title/", current_id, "/")
    resp <- GET(url, my_headers)
    
    if (status_code(resp) != 200) {
      NULL
    } else {
      page_html <- read_html(resp)
      page_text <- as.character(page_html)
      
      # --- Α. JSON-LD (Βασικά Στοιχεία) ---
      json_node <- page_html %>% html_element("script[type='application/ld+json']")
      json_data <- if(!is.na(json_node)) fromJSON(html_text(json_node)) else NULL
      
      if (is.null(json_data)) {
        title <- NA; rating <- NA; votes <- NA; genres <- NA; year <- NA; runtime <- NA
      } else {
        title  <- if(!is.null(json_data$name)) json_data$name[1] else NA
        rating <- as.numeric(json_data$aggregateRating$ratingValue)
        votes  <- as.numeric(json_data$aggregateRating$ratingCount)
        genres <- paste(json_data$genre, collapse = ", ")
        year   <- as.numeric(substr(json_data$datePublished, 1, 4))
        
        d <- json_data$duration
        runtime <- NA
        if (!is.null(d)) {
          h <- as.numeric(str_extract(d, "(?<=PT)\\d+(?=H)")); h <- ifelse(is.na(h), 0, h)
          m <- as.numeric(str_extract(d, "(?<=H)\\d+(?=M)|(?<=PT)\\d+(?=M)")); m <- ifelse(is.na(m), 0, m)
          runtime <- h * 60 + m
        }
      }
      
      # --- Β. Popularity & Metascore ---
      popularity <- parse_number_clean(page_html %>% html_element("[data-testid='hero-rating-bar__popularity__score']") %>% html_text())
      metascore  <- parse_number_clean(page_html %>% html_element("span.metacritic-score-box") %>% html_text())
      
      # --- Γ. User & Critic Reviews ---
      # Προσπαθούμε πρώτα από το JSON __NEXT_DATA__ που είναι το πιο σίγουρο
      next_data_text <- page_html %>% html_element("script[id='__NEXT_DATA__']") %>% html_text()
      
      user_reviews <- NA
      critic_reviews <- NA
      
      if (!is.na(next_data_text)) {
        # Ψάχνουμε τα πεδία total reviews μέσα στο JSON κείμενο
        user_reviews <- as.numeric(str_extract(next_data_text, "(?<=\"reviews\":\\{\"total\":)\\d+"))
        critic_reviews <- as.numeric(str_extract(next_data_text, "(?<=\"criticReviewsTotal\":\\{\"total\":)\\d+"))
      }
      
      # Fallback: Αν το JSON αποτύχει, χρησιμοποιούμε το βελτιωμένο Regex στο κείμενο
      if (is.na(user_reviews)) {
        user_reviews <- parse_number_clean(str_extract(page_text, "[0-9.,K]+(?=\\s+User reviews)"))
      }
      if (is.na(critic_reviews)) {
        critic_reviews <- parse_number_clean(str_extract(page_text, "[0-9.,K]+(?=\\s+Critic reviews)"))
      }
      
      # --- Δ. Review Topics (Chips) ---
      topics <- page_html %>% html_elements("[data-testid='interests'] .ipc-chip__text") %>% html_text()
      review_topics <- if(length(topics) > 0) paste(unique(topics), collapse = ", ") else NA
      
      # --- Ε. Box Office ---
      box_list <- page_html %>% html_elements("[data-testid='title-boxoffice-section'] li") %>% html_text()
      budget <- parse_number_clean(box_list[str_detect(box_list, "Budget")][1])
      gross  <- parse_number_clean(box_list[str_detect(box_list, "Gross worldwide")][1])
      
      # --- ΣΤ. Rating Distribution ---
      rating_dist <- rep(0, 10)
      names(rating_dist) <- paste0("rating_", 10:1)
      if (!is.na(next_data_text)) {
        counts <- str_extract_all(next_data_text, '"rating":\\d+,"voteCount":\\d+')[[1]]
        if(length(counts) >= 10) {
          for(i in 1:length(counts)) {
            r_val <- as.numeric(str_extract(counts[i], "(?<=\"rating\":)\\d+"))
            v_cnt <- as.numeric(str_extract(counts[i], "(?<=\"voteCount\":)\\d+"))
            if(!is.na(r_val) && r_val >= 1 && r_val <= 10) {
              rating_dist[paste0("rating_", r_val)] <- v_cnt
            }
          }
        }
      }
      
      # --- Ζ. More Like This ---
      similar <- page_html %>%
        html_elements("a[href*='/title/tt']") %>%
        html_attr("href") %>%
        str_extract("tt\\d+") %>%
        na.omit() %>% unique()
      similar <- setdiff(similar, c(current_id, processed_ids))
      
      row_df <- data.frame(
        imdb_id = current_id, title = title, year = year, runtime = runtime,
        rating = rating, votes = votes, popularity = popularity, metascore = metascore,
        user_reviews = user_reviews, critic_reviews = critic_reviews,
        budget = budget, gross = gross, genres = genres, review_topics = review_topics,
        stringsAsFactors = FALSE
      )
      full_row <- cbind(row_df, as.data.frame(t(rating_dist)))
      list(data = full_row, similar = head(similar, 15))
    }
  }, error = function(e) {
    message("❌ Σφάλμα στο ", current_id, ": ", e$message)
    return(NULL)
  })
  
  if (!is.null(data_step)) {
    all_data[[current_id]] <- data_step$data
    queue <- unique(c(queue, data_step$similar))
    processed_ids <- c(processed_ids, current_id)
    if (length(processed_ids) %% 50 == 0) {
      temp_df <- bind_rows(all_data)
      save(temp_df, file = "data/movies_backup.RData")
      message(">>> Backup αποθηκεύτηκε: ", length(processed_ids), " ταινίες.")
    }
  }
}

movies_df <- bind_rows(all_data) %>% distinct(imdb_id, .keep_all = TRUE)
save(movies_df, file = "data/movies_raw.RData")
message("Η συλλογή ολοκληρώθηκε! Σύνολο ταινιών: ", nrow(movies_df))