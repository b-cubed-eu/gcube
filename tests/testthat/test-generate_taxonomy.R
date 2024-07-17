## Prepare example data
n_spec <- 5
colnames_tax <- c("species", "genus", "family", "order", "class", "phylum",
                  "kingdom")

# Example dataframe
existing_df <- data.frame(
  count = c(1, 2, 5, 4, 8, 9, 3),
  det_prob = c(0.9, 0.9, 0.9, 0.8, 0.5, 0.2, 0.2)
  )


## Unit tests

test_that("generate_taxonomy function works correctly", {
  # Is the output format correct?
  # Testing with default values
  taxonomy1 <- generate_taxonomy(n_spec, 3, 2, seed = 123)

  # Correct dimensions?
  expect_equal(nrow(taxonomy1), n_spec)
  expect_equal(ncol(taxonomy1), length(colnames_tax))

  # Correct column names?
  expect_true(all(colnames(taxonomy1) == colnames_tax))

  # Testing with existing dataframe
  taxonomy2 <- generate_taxonomy(existing_df, 4, 2, seed = 123)

  # Correct dimensions?
  expect_equal(nrow(taxonomy2), nrow(existing_df))
  expect_equal(ncol(taxonomy2), ncol(existing_df) + length(colnames_tax))

  # Correct column names?
  expect_true(
    all(colnames(taxonomy2) == c(colnames_tax, colnames(existing_df)))
    )

  # Check if seed makes results reproducible
  # Generate taxonomy with seed 123 twice
  taxonomy_seed123 <- generate_taxonomy(5, 3, 2, seed = 123)
  taxonomy_seed123_again <- generate_taxonomy(5, 3, 2, seed = 123)

  expect_equal(taxonomy_seed123, taxonomy_seed123_again)
})

test_that("generate_taxonomy handles invalid inputs", {
  # Validating single integer counts
  expect_error(generate_taxonomy("not_an_integer", 3, 2),
               "`num_species` should be a single integer or a dataframe.")
  expect_error(generate_taxonomy(5, 3, 2, "not_an_integer"),
               "`num_orders` should be a single integer.")

  # Validating seed input
  expect_error(generate_taxonomy(5, 3, 2, seed = "not_a_number"),
               "`seed` must be a numeric vector of length 1 or NA.")

  # Validating number logic
  expect_error(
    generate_taxonomy(10, 50, 4, 2),
    "Number of genera should be smaller or equal to number of species.")
  expect_error(
    generate_taxonomy(5, 3, 2, 3),
    "Number of orders should be smaller or equal to number of families.")
})
