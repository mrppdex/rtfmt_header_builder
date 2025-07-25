# --- Core Header Construction Functions ---

#' Create a base header definition from a vector of column names.
#'
#' This is the starting point for building a header specification. It creates a flat
#' list where each element corresponds to a column in your final table.
#'
#' @param columns A character vector of column names.
#' @param default_just The default justification to apply to each new label.
#' @return A list of lists, representing the header structure.
#' @examples
#' h1 <- create_header(c("col_a", "col_b", "col_c"))
create_header <- function(columns, default_just = 'c') {
  lapply(columns, function(col_name) {
    list(label = col_name, just = default_just)
  })
}

#' Add a spanner label over a set of columns.
#'
#' This function finds a contiguous set of columns (or existing spanners) at the
#' top level of the current header structure and groups them under a new spanner.
#'
#' @param header The header structure list.
#' @param label The character label for the new spanner.
#' @param columns A character vector of the labels of the columns/spanners to group.
#'        These must be contiguous in the current header structure.
#' @param ... Additional named arguments (e.g., just = 'c', pct = 100) to be
#'        added to the spanner's list definition.
#' @return An updated header structure list.
#' @examples
#' h2 <- create_header(c("a1", "a2", "b1"))
#' h2 <- tab_spanner(h2, label = "Group A", columns = c("a1", "a2"))
tab_spanner <- function(header, label, columns, ...) {
  if (!is.list(header) || length(header) == 0) {
    stop("The 'header' argument must be a non-empty list.", call. = FALSE)
  }
  if (length(columns) == 0) {
    stop("You must specify at least one column to span.", call. = FALSE)
  }

  # Get the labels of the top-level elements in the current header structure
  top_level_labels <- vapply(header, function(x) x$label, character(1))

  # Find the indices of the columns to be spanned
  indices <- match(columns, top_level_labels)

  if (anyNA(indices)) {
    stop("One or more specified columns were not found at the top level of the header.", call. = FALSE)
  }

  # Check that the columns are a contiguous block
  if (length(indices) > 1 && !all(diff(sort(indices)) == 1)) {
    stop("Columns to be spanned must be contiguous.", call. = FALSE)
  }
  
  start_index <- min(indices)
  end_index <- max(indices)

  # The list of elements that will go under the new spanner
  sub_list <- header[start_index:end_index]

  # The new spanner element itself
  spanner_spec <- list(
    label = label,
    sub = sub_list,
    ...
  )
  # Ensure justification is set if not provided
  if (is.null(spanner_spec$just)) {
    spanner_spec$just <- 'c'
  }

  # Reconstruct the header list
  new_header <- list()
  # Add elements before the new spanner
  if (start_index > 1) {
    new_header <- c(new_header, header[1:(start_index - 1)])
  }
  # Add the new spanner
  new_header <- c(new_header, list(spanner_spec))
  # Add elements after the new spanner
  if (end_index < length(header)) {
    new_header <- c(new_header, header[(end_index + 1):length(header)])
  }

  return(new_header)
}

#' Modify the labels of final (leaf) columns.
#'
#' This function recursively traverses the header structure and renames the
#' underlying columns (those that do not have their own `sub` elements).
#'
#' @param header The header structure list.
#' @param ... Named arguments in the format `old_label = "new_label"`.
#' @return An updated header structure list.
#' @examples
#' h3 <- create_header(c("col_a", "col_b"))
#' h3 <- cols_label(h3, col_a = "Column A", col_b = "Column B")
cols_label <- function(header, ...) {
  labels_to_change <- list(...)
  if (length(labels_to_change) == 0) return(header)

  # Recursive function to traverse the structure
  modify_leaf_labels <- function(h, changes) {
    lapply(h, function(col_spec) {
      if (!is.null(col_spec$sub)) {
        # This is a spanner, so we recurse into its sub-list
        col_spec$sub <- modify_leaf_labels(col_spec$sub, changes)
      } else {
        # This is a leaf node, check if its label needs to be changed
        if (col_spec$label %in% names(changes)) {
          col_spec$label <- changes[[col_spec$label]]
        }
      }
      col_spec
    })
  }

  modify_leaf_labels(header, labels_to_change)
}


#' Modify the justification of any header element.
#'
#' Finds header elements (spanners or columns) by their labels and updates
#' their justification.
#'
#' @param header The header structure list.
#' @param just The new justification ('c', 'l', or 'r').
#' @param columns A character vector of labels to apply the justification to.
#' @return An updated header structure list.
cols_justify <- function(header, just, columns) {
  
  # Recursive modifier function
  modify_just <- function(h) {
    lapply(h, function(col_spec) {
      # Recurse first to handle nested cases
      if (!is.null(col_spec$sub)) {
        col_spec$sub <- modify_just(col_spec$sub)
      }
      
      # Check if the current spec's label is in our target list
      if (col_spec$label %in% columns) {
        col_spec$just <- just
      }
      col_spec
    })
  }
  
  modify_just(header)
}


# --- Utility Function for Visualization ---

#' Print the header structure in a readable, indented format.
#'
#' @param header The header structure list.
#' @param indent The initial indentation string.
print_header_structure <- function(header, indent = "") {
  for (spec in header) {
    # Construct the display string for the current level
    details <- paste0(
      "(just='", spec$just, "'",
      if (!is.null(spec$pct)) paste0(", pct=", spec$pct) else "",
      ")"
    )
    cat(paste0(indent, "- ", spec$label, " ", details, "\n"))
    
    # If there's a sub-list, recurse with increased indentation
    if (!is.null(spec$sub)) {
      print_header_structure(spec$sub, indent = paste0(indent, "  "))
    }
  }
}


# --- EXAMPLES: Recreating your specs ---

# Example 1: specs1
cat("--- Building specs1 ---\n")
specs1_build <- create_header(c("Column A", "Column B", "Column C"))
print_header_structure(specs1_build)
cat("\n")

# Example 2: specs2
cat("--- Building specs2 ---\n")
specs2_build <- create_header(c("Sub A1", "Sub A2", "Main B"))
specs2_build <- tab_spanner(specs2_build, label = "Main A", columns = c("Sub A1", "Sub A2"))
print_header_structure(specs2_build)
cat("\n")

# Example 3: specs3 (built step-by-step)
cat("--- Building specs3 ---\n")
# Start with the innermost column labels
specs3_build <- create_header(c("Inner A1a", "Inner A1b", "Middle B1", "Middle B2"))

# Create the middle-level spanners
specs3_build <- tab_spanner(specs3_build, label = "Middle A1", columns = c("Inner A1a", "Inner A1b"))
specs3_build <- tab_spanner(specs3_build, label = "Header B", columns = c("Middle B1", "Middle B2"))

# Create the top-level spanner over "Middle A1"
specs3_build <- tab_spanner(specs3_build, label = "Header A", columns = c("Middle A1"))

print_header_structure(specs3_build)

# You can also use pipes for a more gt-like feel (requires R >= 4.1 or magrittr)
if (exists("|>")) {
    cat("\n--- Building specs3 with pipes |> ---\n")
    specs3_pipe <-
      create_header(c("Inner A1a", "Inner A1b", "Middle B1", "Middle B2")) |>
      tab_spanner(label = "Middle A1", columns = c("Inner A1a", "Inner A1b")) |>
      tab_spanner(label = "Header B", columns = c("Middle B1", "Middle B2")) |>
      tab_spanner(label = "Header A", columns = c("Middle A1"))
      
    print_header_structure(specs3_pipe)
}

