crear_flextable_resumen <- function(objeto) {
  flextable(objeto) %>%
    bold(part = "header") %>%
    align(j = 2, align = "center", part = "header") %>%
    align(j = 2, align = "right", part = "body") %>%
    fontsize(size = 9, part = "all") %>%
    width(j = 1, width = 2) %>%
    width(j = 2, width = 0.5)
}