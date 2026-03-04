crear_flextable <- function(objeto) {
  flextable(objeto) %>%
    bold(part = "header") %>%
    align(j = 2:3, align = "center", part = "header") %>%
    align(j = 2:3, align = "right", part = "body") %>%
    fontsize(size = 9, part = "all") %>%
    width(j = 1, width = 2) %>%
    width(j = 2:3, width = 0.5)
}