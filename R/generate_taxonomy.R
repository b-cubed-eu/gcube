#' Generate a taxonomic hierarchy
#'
#' This function generates a random taxonomic hierarchy for a specified numbers
#' of species, genera, families, orders, classes, phyla, and kingdoms. The
#' output is a data frame with the hierarchical classification for each species.
#'
#' The function works by randomly assigning species to genera, genera to
#' families, families to orders, orders to classes, classes to phyla, and phyla
#' to kingdoms. Sampling is done with replacement, meaning that multiple
#' lower-level taxa (e.g., species) can be assigned to the same higher-level
#' taxon (e.g., genus).
#'
#' @param num_species Number of species to generate, or a dataframe. With a
#' dataframe, the function will create a species with taxonomic hierarchy for
#' each row. The original columns of the dataframe will be retained in the
#' output.
#' @param num_genera Number of genera to generate.
#' @param num_families Number of families to generate.
#' @param num_orders Number of orders to generate. Defaults to 1.
#' @param num_classes Number of classes to generate. Defaults to 1.
#' @param num_phyla Number of phyla to generate. Defaults to 1.
#' @param num_kingdoms Number of kingdoms to generate. Defaults to 1.
#' @param seed The seed for random number generation to make results
#' reproducible. If `NA` (the default), no seed is used.
#'
#' @return A data frame with the taxonomic classification of each species. If
#' `num_species` is a dataframe, the taxonomic classification is added to this
#' input dataframe. The original columns of the dataframe will be retained in
#' the output.
#'
#' @export
#'
#' @import dplyr
#' @import assertthat
#' @importFrom withr local_seed
#'
#' @family multispecies
#'
#' @examples
#' # Create simple taxonomic hierarchy
#' generate_taxonomy(
#'   num_species = 5,
#'   num_genera = 3,
#'   num_families = 2,
#'   seed = 123)
#'
#' # Add taxonomic hierarchy to a dataframe
#' existing_df <- data.frame(
#'   count = c(1, 2, 5, 4, 8, 9, 3),
#'   det_prob = c(0.9, 0.9, 0.9, 0.8, 0.5, 0.2, 0.2)
#'   )
#'
#' generate_taxonomy(
#'   num_species = existing_df,
#'   num_genera = 4,
#'   num_families = 2,
#'   seed = 125)

generate_taxonomy <- function(
    num_species,
    num_genera,
    num_families,
    num_orders = 1,
    num_classes = 1,
    num_phyla = 1,
    num_kingdoms = 1,
    seed = NA) {
  ### Start checks
  # 1. Check input type and length
  # Check if numbers are single counts (or dataframe)
  stopifnot("`num_species` should be a single integer or a dataframe." =
              assertthat::is.count(num_species) ||
              inherits(num_species, "data.frame"))
  stopifnot("`num_genera` should be a single integer." =
              assertthat::is.count(num_genera))
  stopifnot("`num_families` should be a single integer." =
              assertthat::is.count(num_families))
  stopifnot("`num_orders` should be a single integer." =
              assertthat::is.count(num_orders))
  stopifnot("`num_phyla` should be a single integer." =
              assertthat::is.count(num_phyla))
  stopifnot("`num_kingdoms` should be a single integer." =
              assertthat::is.count(num_kingdoms))

  # Check if seed is NA or a number
  stopifnot("`seed` must be a numeric vector of length 1 or NA." =
              (is.numeric(seed) | is.na(seed)) &
              length(seed) == 1)

  # 2. Other checks
  # Validate dataframe input
  if (inherits(num_species, "data.frame")) {
    # Define dataframe and number of species correctly
    species_df <- num_species
    num_species <- nrow(species_df)

    # Generate species names
    species_df$species <- paste0("species", seq_len(num_species))

    # Generate species keys
    species_df$species_key <- seq_len(num_species)
  } else {
    # Generate species names
    species_df <- data.frame(species = paste0("species", seq_len(num_species)),
                             species_key = seq_len(num_species))
  }

  # Check if number of species is smaller than number of genera is smaller than
  # number of families ...
  stopifnot(
    "Number of genera should be smaller or equal to number of species." =
      num_species >= num_genera)
  stopifnot(
    "Number of families should be smaller or equal to number of genera." =
      num_genera >= num_families)
  stopifnot(
    "Number of orders should be smaller or equal to number of families." =
      num_families >= num_orders)
  stopifnot(
    "Number of classes should be smaller or equal to number of orders." =
      num_orders >= num_classes)
  stopifnot(
    "Number of phyla should be smaller or equal to number of classes." =
      num_classes >= num_phyla)
  stopifnot(
    "Number of kingdoms should be smaller or equal to number of phyla." =
      num_phyla >= num_kingdoms)
  ### End checks

  # Set seed if provided
  if (!is.na(seed)) {
    withr::local_seed(seed)
  }

  # Assign species to genera
  genera <- paste0("genus", seq_len(num_genera))
  species_to_genera <- sample(genera, num_species, replace = TRUE)

  # Assign genera to families
  families <- paste0("family", seq_len(num_families))
  genera_to_families <- data.frame(
    genus = genera,
    family = sample(families, num_genera, replace = TRUE))

  # Assign families to orders
  orders <- paste0("order", seq_len(num_orders))
  families_to_orders <- data.frame(
    family = families,
    order = sample(orders, num_families, replace = TRUE))

  # Assign orders to classes
  classes <- paste0("class", seq_len(num_classes))
  orders_to_classes <- data.frame(
    order = orders,
    class = sample(classes, num_orders, replace = TRUE))

  # Assign classes to phyla
  phyla <- paste0("phylum", seq_len(num_phyla))
  classes_to_phyla <- data.frame(
    class = classes,
    phylum = sample(phyla, num_classes, replace = TRUE))

  # Assign phyla to kingdoms
  kingdoms <- paste0("kingdom", seq_len(num_kingdoms))
  phyla_to_kingdoms <- data.frame(
    phylum = phyla,
    kingdom = sample(kingdoms, num_phyla, replace = TRUE))

  # Create a data frame to store the taxonomy
  taxonomy <- species_df %>%
    dplyr::mutate(genus = species_to_genera) %>%
    dplyr::left_join(genera_to_families, by = "genus") %>%
    dplyr::left_join(families_to_orders, by = "family") %>%
    dplyr::left_join(orders_to_classes, by = "order") %>%
    dplyr::left_join(classes_to_phyla, by = "class") %>%
    dplyr::left_join(phyla_to_kingdoms, by = "phylum") %>%
    dplyr::select("species", "species_key", "genus", "family", "order", "class",
                  "phylum", "kingdom", everything())

  return(taxonomy)
}
