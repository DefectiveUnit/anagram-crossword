library(R6)

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
                             cat(apply(self$grid, 1, paste, collapse = " "), sep = "\n")
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
                           }
                         )
)



# Define a pattern
row_patterns <- list(
  c(7,1,5),
  c(0,2,7,1,3),
  c(13),
  c(3,1,9),
  c(0,4,5,4)
)

# Create a grid object
grid <- CrosswordGrid$new(dim = c(5,13), row_patterns = row_patterns)

# Show it
grid$show()

# Update cell (row 3, col 5) with link "ee"
grid$update(3, 5, "ee")

# View again
grid$show()

# Clear it
grid$clear(3, 5)

# Get the value of a cell
grid$get(3, 5)

