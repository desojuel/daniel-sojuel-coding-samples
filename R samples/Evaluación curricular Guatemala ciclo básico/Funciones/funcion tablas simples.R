make_freq_table <- function(data, var, label, levels_order = NULL) {
  
  var <- enquo(var)  # capture variable
  
  data <- data %>%
    # Recode missing values first
    mutate(
      !!var := recode(!!var, .missing = "Sin información")
    )
  
  # If a custom order is provided, factorize the variable
  if (!is.null(levels_order)) {
    data <- data %>%
      mutate(
        !!var := factor(!!var, levels = levels_order, ordered = TRUE)
      )
  }
  
  data %>%
    tabyl(!!var, show_na = FALSE) %>%  # already handled NA above
    # Arrange: if levels_order is NULL, order by frequency descending
    { 
      if (is.null(levels_order)) arrange(., desc(n)) else arrange(., !!var)
    } %>%
    adorn_totals("row") %>%
    mutate(
      f   = format(n, big.mark = ","),
      `%` = percent(percent, accuracy = 0.01) |> recode("100.00%" = "100%")
    ) %>%
    select(!!label := !!var, f, `%`)
}