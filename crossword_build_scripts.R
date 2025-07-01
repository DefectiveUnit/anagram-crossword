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
words_xd <- raw_xd %>% pull(answer) %>% tolower()

df_words <- c(words_xd, words_nyt) %>%
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
  c(0,2,3),
  c(0,1,4),
  c(5),
  c(4,1,0),
  c(3,2,0)
)

# Initialize and build
grid <- CrosswordGrid$new(dim = c(5, 5), row_patterns = row_patterns)
grid$show()

grid_filled <- autofill_with_backtracking(grid, df_links, steps = 60, retry_limit = 5)
grid_filled$prettyshow()

result <- autofill_words(grid_filled, df_links)
result$grid$prettyshow()

out <- result$grid$printable(raw_xd, raw_nyt, df_links)
print(out$clues, n = Inf)
print(out$plot)
