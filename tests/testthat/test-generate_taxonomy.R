## Prepare example datasets
n_spec1 <- 7

# Existing dataframe
existing_df <- data.frame(
  species = paste0("species", seq_len(n_spec)),
  count = c(1, 2, 5, 4, 8, 9, 3),
  det_prob = c(0.9, 0.9, 0.9, 0.8, 0.5, 0.2, 0.2)
  )

# Existing dataframe with wrong column name
existing_df2 <- data.frame(
  spec = paste0("species", seq_len(n_spec)),
  count = c(1, 2, 5, 4, 8, 9, 3),
  det_prob = c(0.9, 0.9, 0.9, 0.8, 0.5, 0.2, 0.2)
)

# Existing dataframe without unique species names
existing_df3 <- data.frame(
  species = paste0("species", c(seq_len(n_spec - 1), 6)),
  count = c(1, 2, 5, 4, 8, 9, 3),
  det_prob = c(0.9, 0.9, 0.9, 0.8, 0.5, 0.2, 0.2)
)


## Unit tests

test_that("generate_taxonomy function works correctly", {
  # Is the output format correct?
  # Testing with default values
  n_spec2 <- 5
  taxonomy1 <- generate_taxonomy(n_spec2, 3, 2, seed = 123)

  # Correct dimensions?
  expect_equal(nrow(taxonomy1), n_spec2)
  expect_equal(ncol(taxonomy1), 7)

  # Correct column names?
  colnames_tax <- c("species", "genus", "family", "order", "class", "phylum",
                    "kingdom")
  expect_true(all(colnames(taxonomy1) == colnames_tax))

  # Testing with existing dataframe
  taxonomy2 <- generate_taxonomy(existing_df, 4, 2, seed = 123)

  # Correct dimensions?
  expect_equal(nrow(taxonomy2), n_spec1)
  expect_equal(ncol(taxonomy2), ncol(existing_df) + 6)

  # Correct column names?
  expect_true(
    all(colnames(taxonomy2) ==
          c(colnames_tax,
            colnames(existing_df)[colnames(existing_df) != "species"]
            )
        )
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
               "`num_species` should be a single integer.")
  expect_error(generate_taxonomy(5, 3, 2, "not_an_integer"),
               "`num_orders` should be a single integer.")

  # Validating dataframe input
  expect_error(generate_taxonomy(existing_df2, 3, 2),
               "`species` column not present in `num_species` dataframe.")
  expect_error(generate_taxonomy(existing_df3, 3, 2),
               "`species` column must contain unique species names.")

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

