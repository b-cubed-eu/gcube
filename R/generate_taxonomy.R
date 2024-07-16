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
#' @param num_species Number of species to generate.
#' @param num_genera Number of genera to generate.
#' @param num_families Number of families to generate.
#' @param num_orders Number of orders to generate. Defaults to 1.
#' @param num_classes Number of classes to generate. Defaults to 1.
#' @param num_phyla Number of phyla to generate. Defaults to 1.
#' @param num_kingdoms Number of kingdoms to generate. Defaults to 1.
#' @param seed The seed for random number generation to make results
#' reproducible. If `NA` (the default), no seed is used.
#'
#' @details The function works by randomly assigning species to genera, genera
#' to families, families to orders, orders to classes, classes to phyla, and
#' phyla to kingdoms. Sampling is done with replacement, meaning that multiple
#' lower-level taxa (e.g., species) can be assigned to the same higher-level
#' taxon (e.g., genus).
#'
#' @return A data frame with the taxonomic classification of each species.
#'
#' @export
#'
#' @import dplyr
#'
#' @family multispecies
#'
#' @examples
#' # Create simple taxonomic hierarchy
#' generate_taxonomy(
#'   num_species = 5,
#'   num_genera = 3,
#'   num_families = 2
#'   seed = 123)

generate_taxonomy <- function(
    num_species,
    num_genera,
    num_families,
    num_orders = 1,
    num_classes = 1,
    num_phyla = 1,
    num_kingdoms = 1,
    seed = NA) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  library(dplyr)

  # Generate species names
  species <- paste0("species", seq_len(num_species))

  # Assign species to genera
  genera <- paste0("genus", seq_len(num_genera))
  species_to_genera <- sample(genera, num_species, replace = TRUE)

  # Assign genera to families
  families <- paste0("family", seq_len(num_families))
  genera_to_families <- data.frame(genus = genera, family = sample(families, num_genera, replace = TRUE))

  # Assign families to orders
  orders <- paste0("order", seq_len(num_orders))
  families_to_orders <- data.frame(family = families, order = sample(orders, num_families, replace = TRUE))

  # Assign orders to classes
  classes <- paste0("class", seq_len(num_classes))
  orders_to_classes <- data.frame(order = orders, class = sample(classes, num_orders, replace = TRUE))

  # Assign classes to phyla
  phyla <- paste0("phylum", seq_len(num_phyla))
  classes_to_phyla <- data.frame(class = classes, phylum = sample(phyla, num_classes, replace = TRUE))

  # Assign phyla to kingdoms
  kingdoms <- paste0("kingdom", seq_len(num_kingdoms))
  phyla_to_kingdoms <- data.frame(phylum = phyla, kingdom = sample(kingdoms, num_phyla, replace = TRUE))

  # Create a data frame to store the taxonomy
  taxonomy <- data.frame(species = species, genus = species_to_genera) %>%
    left_join(genera_to_families, by = "genus") %>%
    left_join(families_to_orders, by = "family") %>%
    left_join(orders_to_classes, by = "order") %>%
    left_join(classes_to_phyla, by = "class") %>%
    left_join(phyla_to_kingdoms, by = "phylum")

  return(taxonomy)
}

# Example usage
generate_taxonomy(num_species = 5, num_genera = 3, num_families = 2, num_orders = 1, num_classes = 1, num_phyla = 1, num_kingdoms = 1, seed = 123)
