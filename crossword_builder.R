rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Future can download (and pay) for other curated word lists, e.g. 
# https://www.georgeho.org/crosswords-datasets-dictionaries/

# Libraries -------------------------------
library(data.table)
library(tidyverse)
library(forcats)
library(R6)



# Load and process data -------------------------------
raw_nyt = fread("corpus/nytcrosswords.csv")
raw_xd = read_tsv("corpus/xd_clues.tsv")

# Combine all words into one DF 
words_nyt <- raw_nyt %>%
  pull(Word) %>%
  tolower()

words_xd <- raw_xd %>%
  pull(answer) %>%
  tolower()


df_words <- c(words_xd, words_nyt) %>%
  unique() %>%
  sort() %>%
  tibble(word = .) %>%
  filter(grepl("^[a-z]+$", word))

get_clue_lookup <- function(raw_xd, raw_nyt) {
  clues_xd <- raw_xd %>%
    transmute(word = toupper(answer), clue = clue)
  
  clues_nyt <- raw_nyt %>%
    transmute(word = toupper(Word), clue = Clue)
  
  bind_rows(clues_xd, clues_nyt) %>%
    distinct() %>%
    group_by(word) %>%
    summarise(clue = sample(clue, 1), .groups = "drop")  # pick one clue per word
}


# Find anagrams
df_words <- df_words %>%
  mutate(sorted = sapply(strsplit(word, ""), function(x) paste0(sort(x), collapse = "")))

sorted_counts <- df_words %>%
  count(sorted, name = "match_count")

df_anagrams <- df_words %>%
  inner_join(df_words, by = "sorted", suffix = c("A", "B"), relationship = "many-to-many") %>%
  filter(wordA != wordB) %>%
  mutate(pair_id = paste0(wordA, "<>", wordB)) %>%
  left_join(sorted_counts, by = "sorted") %>%
  select(pair_id, wordA, wordB, sorted, match_count) %>%
  arrange(desc(match_count))

# Build the link/bigram dataset
df_links <- df_anagrams %>%
  mutate(length = nchar(wordA)) %>%
  filter(nchar(wordA) == nchar(wordB)) %>%  # sanity check: only compare equal-length words
  pmap_dfr(function(pair_id, wordA, wordB, sorted, match_count, ...) {
    word_len <- nchar(wordA)
    lettersA <- strsplit(wordA, "")[[1]]
    lettersB <- strsplit(wordB, "")[[1]]
    
    tibble(
      anagram = pair_id,
      position = 1:word_len,
      link = paste0(lettersA, lettersB),
      next_link = c(paste0(lettersA[-1], lettersB[-1]), NA),
      prev_link = c(NA, paste0(lettersA[-word_len], lettersB[-word_len])),
      length = word_len,
      wordA = wordA,
      wordB = wordB,
      sorted = sorted
    )
  })


# Some general EDA -----------------------------------

# word length freq
df_anagrams %>%
  select(wordA, wordB) %>%
  pivot_longer(everything(), values_to = "word") %>%
  distinct(word) %>%
  mutate(length = nchar(word)) %>%
  count(length) %>%
  ggplot(aes(x = length, y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Word Length Distribution (Words in Anagram Pairs Only)",
    x = "Word Length",
    y = "Number of Words"
  ) +
  theme_minimal()



# bigram by position freq
# Identify top 20 most common links
top_links <- df_links %>%
  count(link, sort = TRUE) %>%
  slice_head(n = 20) %>%
  pull(link)

# Prepare filtered heatmap data
heatmap_data <- df_links %>%
  filter(link %in% top_links) %>%
  count(link, position)

# Plot
ggplot(heatmap_data, aes(x = position, y = fct_reorder(link, -n))) +
  geom_tile(aes(fill = n), color = "grey90") +
  geom_text(aes(label = n), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Top 20 Letter Links by Position",
    x = "Position in Word",
    y = "Letter Link (A[i] + B[i])",
    fill = "Frequency"
  ) +
  theme_minimal()

# helper functions for crossword crafting ------------------------

top_links_at_position <- function(df,
                                  result_position,
                                  target_length = NULL,
                                  filter_positions = NULL,
                                  filter_links = NULL,
                                  top_n = 10) {
  
  filtered_df <- df
  
  # Optional length filter
  if (!is.null(target_length)) {
    filtered_df <- filtered_df %>%
      filter(length %in% target_length)
  }
  
  # Optional multiple position/link filters
  if (!is.null(filter_positions) && !is.null(filter_links)) {
    if (length(filter_positions) != length(filter_links)) {
      stop("`filter_positions` and `filter_links` must be the same length")
    }
    
    # For each condition, filter for matching anagrams and take intersection
    valid_anagrams_list <- map2(filter_positions, filter_links, ~ {
      filtered_df %>%
        filter(position == .x, link == .y) %>%
        pull(anagram) %>%
        unique()
    })
    
    # Get intersection of all sets of valid anagrams
    valid_anagrams <- reduce(valid_anagrams_list, intersect)
    
    filtered_df <- filtered_df %>%
      filter(anagram %in% valid_anagrams)
  }
  
  # Return top N links at result_position
  filtered_df %>%
    filter(position == result_position) %>%
    count(link, sort = TRUE) %>%
    slice_head(n = top_n)
}

compare_link_sets <- function(
    df,
    checker1_args,
    checker2_args,
    score_mode = c("min", "mean", "geom", "product")
) {
  score_mode <- match.arg(score_mode)
  
  # Run each checker (remove top_n cutoff)
  df1 <- do.call(top_links_at_position, c(list(df = df), checker1_args, top_n = Inf)) %>%
    rename(freq1 = n)
  
  df2 <- do.call(top_links_at_position, c(list(df = df), checker2_args, top_n = Inf)) %>%
    rename(freq2 = n)
  
  # Inner join to links present in both
  df_combined <- inner_join(df1, df2, by = "link")
  
  # Score based on selected method
  df_scored <- df_combined %>%
    mutate(score = case_when(
      score_mode == "min"   ~ pmin(freq1, freq2),
      score_mode == "mean"  ~ rowMeans(cbind(freq1, freq2)),
      score_mode == "geom"  ~ sqrt(freq1 * freq2),
      score_mode == "product" ~ freq1 * freq2
    )) %>%
    arrange(desc(score))
  
  return(df_scored)
}

find_matching_anagrams_from_links <- function(df_links, link_pattern) {
  word_length <- length(link_pattern)
  known_positions <- which(!is.na(link_pattern))
  
  if (length(known_positions) == 0) {
    stop("At least one link must be specified.")
  }
  
  # Filter to correct word length and only relevant positions
  filtered <- df_links %>%
    filter(length == word_length, position %in% known_positions)
  
  # For each known position, keep rows matching the expected link
  matches <- map_dfr(known_positions, function(pos) {
    filtered %>%
      filter(position == pos, link == link_pattern[[pos]]) %>%
      mutate(pos_check = pos)
  })
  
  # Count how many required link positions each anagram satisfies
  matches %>%
    count(anagram, sort = TRUE) %>%
    filter(n == length(known_positions)) %>%
    pull(anagram)
}







# Example
top_links_at_position(
  df_links,
  result_position = 4,
  target_length = c(7, 8),
  filter_positions = c(1, 3),
  filter_links = c("ee", "rs")
)

top_links_at_position(df_links, result_position = 2)





# crossword builder  -------------------------
CrosswordGrid <- R6Class("CrosswordGrid",
                         public = list(
                           grid = NULL,
                           n_rows = NULL,
                           n_cols = NULL,
                           
                           initialize = function(dim = c(13, 13), row_patterns) {
                             self$n_rows <- dim[1]
                             self$n_cols <- dim[2]
                             self$grid <- matrix("#", nrow = self$n_rows, ncol = self$n_cols)
                             
                             if (length(row_patterns) != self$n_rows) {
                               stop("row_patterns must have one entry per row.")
                             }
                             
                             for (i in seq_len(self$n_rows)) {
                               pattern <- row_patterns[[i]]
                               is_white <- TRUE
                               col_pos <- 1
                               for (run_length in pattern) {
                                 if (run_length > 0) {
                                   fill_value <- if (is_white) "." else "#"
                                   end_pos <- min(col_pos + run_length - 1, self$n_cols)
                                   self$grid[i, col_pos:end_pos] <- fill_value
                                   col_pos <- end_pos + 1
                                 }
                                 is_white <- !is_white
                               }
                             }
                           },
                           
                           show = function() {
                             padded_grid <- apply(self$grid, c(1, 2), function(cell) {
                               if (nchar(cell) == 1) paste0(cell, " ") else cell
                             })
                             cat(apply(padded_grid, 1, paste, collapse = " "), sep = "\n")
                           },
                           
                           prettyshow = function() {

                             df <- expand.grid(
                               row = 1:self$n_rows,
                               col = 1:self$n_cols
                             )
                             df$val <- as.vector(self$grid)
                             
                             df <- df %>%
                               mutate(
                                 type = case_when(
                                   val == "#" ~ "black",
                                   val == "." ~ "white",
                                   TRUE       ~ "link"
                                 ),
                                 l1 = ifelse(nchar(val) == 2, substr(val, 1, 1), ""),
                                 l2 = ifelse(nchar(val) == 2, substr(val, 2, 2), "")
                               )
                             
                             ggplot(df, aes(x = col, y = -row)) +
                               geom_tile(aes(fill = type), color = "grey70") +
                               geom_text(aes(label = l1), color = "blue", size = 5, nudge_x = -0.2) +
                               geom_text(aes(label = l2), color = "red", size = 5, nudge_x =  0.2) +
                               scale_fill_manual(values = c(black = "black", white = "white", link = "white")) +
                               coord_fixed() +
                               theme_void() +
                               theme(legend.position = "none")
                           },
                           
                           
                           
                           
                           update = function(row, col, value) {
                             if (self$grid[row, col] == "#") {
                               stop("Cannot write to a black square.")
                             }
                             self$grid[row, col] <- value
                           },
                           
                           clear = function(row, col) {
                             if (self$grid[row, col] == "#") {
                               stop("Cannot clear a black square.")
                             }
                             self$grid[row, col] <- "."
                           },
                           
                           get = function(row, col) {
                             self$grid[row, col]
                           },
                           
                           printable = function(raw_xd, raw_nyt, df_links) {
       
                             
                             clue_lookup <- get_clue_lookup(raw_xd, raw_nyt)
                             
                             grid_vals <- self$grid
                             clue_number <- matrix(NA_integer_, nrow = self$n_rows, ncol = self$n_cols)
                             clues <- list()
                             number <- 1
                             
                             for (i in 1:self$n_rows) {
                               for (j in 1:self$n_cols) {
                                 if (grid_vals[i, j] == "#") next
                                 
                                 starts_across <- (j == 1 || grid_vals[i, j-1] == "#") &&
                                   (j < self$n_cols && grid_vals[i, j+1] != "#")
                                 starts_down   <- (i == 1 || grid_vals[i-1, j] == "#") &&
                                   (i < self$n_rows && grid_vals[i+1, j] != "#")
                                 
                                 if (starts_across || starts_down) {
                                   clue_number[i, j] <- number
                                   
                                   # Across words
                                   if (starts_across) {
                                     end_j <- j
                                     while (end_j <= self$n_cols && grid_vals[i, end_j] != "#") end_j <- end_j + 1
                                     span <- grid_vals[i, j:(end_j - 1)]
                                     
                                     # Safely extract wordA and wordB
                                     wordA_chars <- substr(span, 1, 1)
                                     wordB_chars <- substr(span, 2, 2)
                                     wordA <- toupper(paste0(wordA_chars, collapse = ""))
                                     wordB <- toupper(paste0(wordB_chars, collapse = ""))
                                     
                                     # Log what we found
                                     message(glue::glue("Across @ ({i},{j}): A = {wordA}, B = {wordB}"))
                                     
                                     clueA <- clue_lookup$clue[match(wordA, clue_lookup$word)] %||% NA
                                     clueB <- clue_lookup$clue[match(wordB, clue_lookup$word)] %||% NA
                                     
                                     message(glue::glue("  Clues: A = {clueA}, B = {clueB}"))
                                     
                                     clues[[length(clues) + 1]] <- tibble(
                                       number = number, label = paste0(number, "A"), direction = "Across-A",
                                       row = i, col = j, word = wordA, clue = clueA
                                     )
                                     clues[[length(clues) + 1]] <- tibble(
                                       number = number, label = paste0(number, "A"), direction = "Across-B",
                                       row = i, col = j, word = wordB, clue = clueB
                                     )
                                   }
                                   
                                   
                                   # Down words
                                   if (starts_down) {
                                     end_i <- i
                                     while (end_i <= self$n_rows && grid_vals[end_i, j] != "#") end_i <- end_i + 1
                                     span <- grid_vals[i:(end_i - 1), j]
                                     
                                     wordA_chars <- substr(span, 1, 1)
                                     wordB_chars <- substr(span, 2, 2)
                                     wordA <- toupper(paste0(wordA_chars, collapse = ""))
                                     wordB <- toupper(paste0(wordB_chars, collapse = ""))
                                     
                                     message(glue::glue("Down @ ({i},{j}): A = {wordA}, B = {wordB}"))
                                     
                                     clueA <- clue_lookup$clue[match(wordA, clue_lookup$word)] %||% NA
                                     clueB <- clue_lookup$clue[match(wordB, clue_lookup$word)] %||% NA
                                     
                                     message(glue::glue("  Clues: A = {clueA}, B = {clueB}"))
                                     
                                     clues[[length(clues) + 1]] <- tibble(
                                       number = number, label = paste0(number, "D"), direction = "Down-A",
                                       row = i, col = j, word = wordA, clue = clueA
                                     )
                                     clues[[length(clues) + 1]] <- tibble(
                                       number = number, label = paste0(number, "D"), direction = "Down-B",
                                       row = i, col = j, word = wordB, clue = clueB
                                     )
                                   }
                                   
                                   
                                   number <- number + 1
                                 }
                               }
                             }
                             
                             clue_df <- bind_rows(clues)
                             
                             # build tile grid for plotting
                             df <- expand.grid(row = 1:self$n_rows, col = 1:self$n_cols)
                             df$val <- as.vector(grid_vals)
                             df <- df %>%
                               mutate(
                                 type = ifelse(val == "#", "black", "white"),
                                 number = as.vector(clue_number)
                               )
                             
                             p <- ggplot(df, aes(x = col, y = -row)) +
                               geom_tile(aes(fill = type), color = "grey60") +
                               geom_text(aes(label = ifelse(!is.na(number), number, "")),
                                         hjust = 0, vjust = 1, nudge_x = -0.35, nudge_y = 0.35,
                                         size = 2.5, fontface = "bold") +
                               scale_fill_manual(values = c(white = "white", black = "black")) +
                               coord_fixed() +
                               theme_void() +
                               theme(legend.position = "none")
                             
                             return(list(plot = p, clues = clue_df))
                           }
                           
                           
                         )
)


## Example
# Define a pattern
# row_patterns <- list(
#   c(7,1,5),
#   c(0,2,7,1,3),
#   c(13),
#   c(3,1,9),
#   c(0,4,5,4)
# )
# grid <- CrosswordGrid$new(dim = c(5,13), row_patterns = row_patterns)
# grid$show()
# grid$update(3, 5, "ee")
# grid$show()
# grid$clear(3, 5)
# grid$get(3, 5)

# My 13x13 grid
row_patterns <- list(
  c(7,1,5),
  c(1,1,1,1,1,1,3,1,1,1,1),
  c(7,1,5),
  c(1,1,1,1,1,1,3,1,1,1,1),
  c(4,1,1,2,3,1,1),
  c(0,2,7,1,3),
  c(1,2,1,1,1,1,1,1,1,2,1),
  c(3,1,7,2),
  c(1,1,3,2,1,1,4),
  c(1,1,1,1,3,1,1,1,1,1,1),
  c(5,1,7),
  c(1,1,1,1,3,1,1,1,1,1,1),
  c(5,1,7)
)

grid <- CrosswordGrid$new(dim = c(13,13), row_patterns = row_patterns)
grid$show()

# Building out the crossword ---------------------

grid$update(1,1,"ss")
grid$update(3,1,"ee")
grid$update(5,1,"tt")
grid$update(2,1,"pl")
grid$update(4,1,"lp")

grid$show()


overlap<-compare_link_sets(
  df_links,
  checker1_args = list(
    result_position = 1,
    target_length = 4
  ),
  checker2_args = list(
    result_position = 5,
    target_length = 5,
    filter_positions = c(1,3),
    filter_links = c("ss","ee")
  ),
  score_mode = "min"  # or "mean", "geom", "product"
)

overlap %>% arrange(desc(score))

overlap %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(link, score), y = score)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top Overlapping Links by Min Score",
    x = "Link",
    y = "Score (min of two positions)"
  ) +
  theme_minimal()


find_matching_anagrams_from_links(
  df_links,
  link_pattern = c("ss", NA, "ee", NA, "tt")
)


# Automatic searcher -------------------------
find_next_intersection <- function(grid, start_row = 1, start_col = 1) {
  for (i in start_row:nrow(grid$grid)) {
    j_start <- if (i == start_row) start_col else 1
    for (j in j_start:ncol(grid$grid)) {
      cell <- grid$grid[i, j]
      if (cell != ".") next
      
      # Check for at least one white neighbor in both directions
      has_across <- j > 1 && grid$grid[i, j-1] != "#" ||
        j < ncol(grid$grid) && grid$grid[i, j+1] != "#"
      has_down   <- i > 1 && grid$grid[i-1, j] != "#" ||
        i < nrow(grid$grid) && grid$grid[i+1, j] != "#"
      
      if (has_across && has_down) {
        return(list(row = i, col = j))
      }
    }
  }
  return(NULL)
}

find_best_intersection <- function(grid, df_links) {
  candidates <- list()
  
  for (i in 1:nrow(grid$grid)) {
    for (j in 1:ncol(grid$grid)) {
      if (grid$grid[i, j] != ".") next
      
      has_across <- j > 1 && grid$grid[i, j-1] != "#" || j < ncol(grid$grid) && grid$grid[i, j+1] != "#"
      has_down   <- i > 1 && grid$grid[i-1, j] != "#" || i < nrow(grid$grid) && grid$grid[i+1, j] != "#"
      
      if (has_across && has_down) {
        row_vec <- grid$grid[i, ]
        col_vec <- grid$grid[, j]
        
        horiz_bounds <- get_word_bounds(row_vec, j)
        vert_bounds  <- get_word_bounds(col_vec, i)
        
        if (is.null(horiz_bounds) || is.null(vert_bounds)) next
        
        horiz_vals <- row_vec[horiz_bounds$start:horiz_bounds$end]
        vert_vals  <- col_vec[vert_bounds$start:vert_bounds$end]
        
        horiz_known <- sum(horiz_vals %in% c(".", "#") == FALSE)
        vert_known  <- sum(vert_vals %in% c(".", "#") == FALSE)
        
        total_known <- horiz_known + vert_known
        total_len   <- horiz_bounds$length + vert_bounds$length
        
        candidates[[length(candidates) + 1]] <- list(
          row = i,
          col = j,
          known = total_known,
          length = total_len,
          horiz_len = horiz_bounds$length,
          vert_len = vert_bounds$length
        )
      }
    }
  }
  
  if (length(candidates) == 0) return(NULL)
  
  candidate_df <- bind_rows(candidates)
  candidate_df %>%
    arrange(desc(known), length) %>%
    slice(1) %>%
    select(row, col) %>%
    as.list()
}



get_word_bounds <- function(vec, index) {
  if (vec[index] == "#") return(NULL)
  
  start <- index
  while (start > 1 && vec[start - 1] != "#") {
    start <- start - 1
  }
  
  end <- index
  while (end < length(vec) && vec[end + 1] != "#") {
    end <- end + 1
  }
  
  return(list(
    start = start,
    end = end,
    length = end - start + 1,
    relative_pos = index - start + 1
  ))
}

autofill_with_backtracking <- function(grid, df_links, steps = 5, retry_limit = 5) {
  grid <- grid$clone()
  stack <- list()
  
  for (step in 1:steps) {
    message("=== Step ", step, " ===")
    
    pos <- find_best_intersection(grid, df_links) # find_next_intersection(grid)
    if (is.null(pos)) {
      message("No more intersection cells.")
      break
    }
    
    row <- pos$row
    col <- pos$col
    
    # Get row and column vectors
    col_vec <- grid$grid[, col]
    row_vec <- grid$grid[row, ]
    
    vert_bounds <- get_word_bounds(col_vec, row)
    horiz_bounds <- get_word_bounds(row_vec, col)
    
    if (is.null(vert_bounds) || is.null(horiz_bounds)) {
      message("Invalid intersection at [", row, ",", col, "] — skipping")
      next
    }
    
    vertical_pattern <- lapply(col_vec[vert_bounds$start:vert_bounds$end], function(cell) if (cell %in% c(".", "#")) NA else cell)
    horizontal_pattern <- lapply(row_vec[horiz_bounds$start:horiz_bounds$end], function(cell) if (cell %in% c(".", "#")) NA else cell)
    
    # Run compare_link_sets on the two overlapping slots
    suggestions <- compare_link_sets(
      df_links,
      checker1_args = list(
        result_position = horiz_bounds$relative_pos,
        target_length = horiz_bounds$length,
        filter_positions = which(!is.na(horizontal_pattern)),
        filter_links = unlist(horizontal_pattern[!is.na(horizontal_pattern)])
      ),
      checker2_args = list(
        result_position = vert_bounds$relative_pos,
        target_length = vert_bounds$length,
        filter_positions = which(!is.na(vertical_pattern)),
        filter_links = unlist(vertical_pattern[!is.na(vertical_pattern)])
      ),
      score_mode = "min"
    )
    
    if (nrow(suggestions) == 0) {
      message("No valid suggestions for [", row, ",", col, "], backtracking...")
      retry <- 0
      while (length(stack) > 0 && retry < retry_limit) {
        top <- stack[[length(stack)]]
        stack <- stack[-length(stack)]
        grid$clear(top$row, top$col)
        
        remaining <- top$suggestions %>%
          filter(!(link %in% top$tried))
        
        if (nrow(remaining) > 0) {
          new_link <- remaining$link[1]
          message("Retrying [", top$row, ",", top$col, "] with '", new_link, "'")
          grid$update(top$row, top$col, new_link)
          
          stack[[length(stack) + 1]] <- list(
            row = top$row,
            col = top$col,
            tried = c(top$tried, new_link),
            suggestions = remaining
          )
          break
        } else {
          retry <- retry + 1
        }
      }
      
      if (retry == retry_limit) {
        message("Exceeded retry limit. Aborting.")
        break
      }
      
      next
    }
    
    best_link <- suggestions$link[1]
    grid$update(row, col, best_link)
    
    stack[[length(stack) + 1]] <- list(
      row = row,
      col = col,
      tried = best_link,
      suggestions = suggestions
    )
  }
  
  return(grid)
}

get_link_pattern_from_grid <- function(grid, row_or_col = "row", index, start, end) {
  vec <- if (row_or_col == "row") grid$grid[index, start:end] else grid$grid[start:end, index]
  lapply(vec, function(cell) if (cell %in% c(".", "#")) NA else cell)
}

autofill_words <- function(grid, df_links) {
  grid <- grid$clone()
  alternates_log <- list()
  
  # Process ACROSS words
  for (i in 1:nrow(grid$grid)) {
    row_vec <- grid$grid[i, ]
    j <- 1
    while (j <= ncol(grid$grid)) {
      if (row_vec[j] == "#") {
        j <- j + 1
        next
      }
      
      bounds <- get_word_bounds(row_vec, j)
      if (!is.null(bounds$length) && bounds$length > 1) {
        pattern <- get_link_pattern_from_grid(grid, "row", i, bounds$start, bounds$end)
        anas <- find_matching_anagrams_from_links(df_links, pattern)
        
        if (length(anas) >= 1) {
          words <- strsplit(anas[1], "<>")[[1]]
          wordA <- strsplit(words[1], "")[[1]]
          wordB <- strsplit(words[2], "")[[1]]
          for (k in seq_along(wordA)) {
            grid$update(i, bounds$start + k - 1, paste0(wordA[k], wordB[k]))
          }
          if (length(anas) > 1) {
            key <- paste0("row-", i, "-", bounds$start, "-", bounds$end)
            alternates_log[[key]] <- anas
          }
        }
        
        j <- bounds$end + 1
      } else {
        j <- j + 1
      }
    }
  }
  
  # Process DOWN words
  for (j in 1:ncol(grid$grid)) {
    col_vec <- grid$grid[, j]
    i <- 1
    while (i <= nrow(grid$grid)) {
      if (col_vec[i] == "#") {
        i <- i + 1
        next
      }
      
      bounds <- get_word_bounds(col_vec, i)
      if (!is.null(bounds$length) && bounds$length > 1) {
        pattern <- get_link_pattern_from_grid(grid, "col", j, bounds$start, bounds$end)
        anas <- find_matching_anagrams_from_links(df_links, pattern)
        
        if (length(anas) >= 1) {
          words <- strsplit(anas[1], "<>")[[1]]
          wordA <- strsplit(words[1], "")[[1]]
          wordB <- strsplit(words[2], "")[[1]]
          for (k in seq_along(wordA)) {
            grid$update(bounds$start + k - 1, j, paste0(wordA[k], wordB[k]))
          }
          if (length(anas) > 1) {
            key <- paste0("col-", j, "-", bounds$start, "-", bounds$end)
            alternates_log[[key]] <- anas
          }
        }
        
        i <- bounds$end + 1
      } else {
        i <- i + 1
      }
    }
  }
  
  return(list(grid = grid, alternates = alternates_log))
}




grid2 <- grid$clone()


grid2 <- autofill_with_backtracking(grid2, df_links, steps = 60, retry_limit = 5)
grid2$show()
grid2$prettyshow()


result <- autofill_words(grid2, df_links)
result$grid$prettyshow()
result$alternates  # A list of word slots with multiple valid anagram options


# Get the printable vrs with clues
out <- result$grid$printable(raw_xd, raw_nyt, df_links)
out$plot

# Clues
out$clues %>% print(n = Inf)



# asdasd -------------------------
# asdasd -------------------------
# asdasd -------------------------
# asdasd -------------------------
# asdasd -------------------------
# asdasd -------------------------
# asdasd -------------------------