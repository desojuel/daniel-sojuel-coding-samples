fix_sources <- function(x, labels) {
  
  # If the cell is NA, return NA and exit the function immediately.
  if (is.na(x)) return(NA_character_)
  
  
  # ------------------------------------------------------------------
  # STEP: Identify which known labels are present inside the string x.
  #
  # Understanding the expression:
  #
  #   str_detect(x, fixed(labels))
  #
  # This performs pattern matching using each element of 'labels'
  # against the text value in 'x'. Because 'labels' is a character
  # vector with length > 1, str_detect() returns a logical vector of
  # the same length as 'labels', where each element indicates whether
  # that specific label was found inside x.
  #
  # Example:
  #   x = "Tubería (...) Pozo (...)"
  #   labels = c("Tubería (...)", "Pozo (...)", "Otra fuente")
  #
  #   str_detect(x, fixed(labels)) returns:
  #     [1]  TRUE  TRUE FALSE
  #
  # The 'fixed()' wrapper tells stringr to match literal text, not regex.
  # Without fixed(), punctuation like parentheses would need escaping.
  #
  # Now, we use that logical vector inside labels[ ... ] to subset only
  # the labels that matched. In R, indexing a vector with TRUE/FALSE
  # keeps elements where the condition is TRUE and discards FALSE.
  #
  # So:
  #   labels[c(TRUE, TRUE, FALSE)] becomes:
  #     c("Tubería (...)", "Pozo (...)")
  #
  # The result is a character vector containing only the detected labels.
  # ------------------------------------------------------------------
  
  found <- labels[str_detect(x, fixed(labels))]
  
  
  # If no labels were detected (found == character(0)), return original.
  if (length(found) == 0) return(x)
  
  # Combine detected labels into a single comma-separated string.
  paste(found, collapse = ";")
}
