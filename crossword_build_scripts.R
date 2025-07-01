rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load required libraries
library(data.table)
library(tidyverse)
library(R6)

# Load your functions
source("crossword_functions.R")

# Load and preprocess data
raw_nyt <- fread("corpus/nytcrosswords.csv")
raw_xd <- read_tsv("corpus/xd_clues.tsv")

words_nyt <- raw_nyt %>% pull(Word) %>% tolower()
#words_xd <- raw_xd %>% pull(answer) %>% tolower()

df_words <- words_nyt %>% #c(words_xd, words_nyt) %>%
  unique() %>%
  sort() %>%
  tibble(word = .) %>%
  filter(grepl("^[a-z]+$", word)) %>%
  mutate(sorted = sapply(strsplit(word, ""), function(x) paste0(sort(x), collapse = "")))

sorted_counts <- df_words %>% count(sorted, name = "match_count")

df_anagrams <- df_words %>%
  inner_join(df_words, by = "sorted", suffix = c("A", "B")) %>%
  filter(wordA != wordB) %>%
  mutate(pair_id = paste0(wordA, "<>", wordB)) %>%
  left_join(sorted_counts, by = "sorted") %>%
  select(pair_id, wordA, wordB, sorted, match_count) %>%
  arrange(desc(match_count))

df_links <- df_anagrams %>%
  mutate(length = nchar(wordA)) %>%
  filter(nchar(wordA) == nchar(wordB)) %>%
  pmap_dfr(function(pair_id, wordA, wordB, sorted, match_count, ...) {
    word_len <- nchar(wordA)
    lettersA <- strsplit(wordA, "")[[1]]
    lettersB <- strsplit(wordB, "")[[1]]
    tibble(
      anagram = pair_id,
      position = 1:word_len,
      link = paste0(lettersA, lettersB),
      length = word_len
    )
  })

# Define crossword pattern
row_patterns <- list(
  c(5),
  c(3,1,1),
  c(5),
  c(1,1,3),
  c(5)
  
)

# Initialize and build
grid <- CrosswordGrid$new(dim = c(length(row_patterns), sum(row_patterns[[1]])), 
                          row_patterns = row_patterns)
grid$show()

grid_filled <- autofill_with_backtracking(grid, df_links, steps = 1000, retry_limit = 30, selection_mode = "weighted")
grid_filled$prettyshow()

result <- autofill_words(grid_filled, df_links)
result$grid$prettyshow()

out <- result$grid$printable(raw_xd, raw_nyt, df_links)
print(out$clues, n = Inf)
print(out$plot)


# Save puzzle
nrow <- result$grid$n_rows
ncol <- result$grid$n_cols
grid_size <- paste0(nrow, "x", ncol)
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
base_filename <- paste0("puzzle_", grid_size, "_", timestamp)

# Dynamic image size: 100px per grid cell
width <- 100 * ncol
height <- 100 * nrow

# Save grid plot
png(file.path(output_dir, paste0(base_filename, "_grid.png")), width = width, height = height)
result$grid$prettyshow()
dev.off()

# Save clue plot (same size or fixed if preferred)
png(file.path(output_dir, paste0(base_filename, "_clueplot.png")), width = width, height = height)
print(out$plot)
dev.off()

# Save clues as CSV
write.csv(out$clues, file = file.path(output_dir, paste0(base_filename, "_clues.csv")), row.names = FALSE)



