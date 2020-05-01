require(rvest)

policies.levels <- read_html("https://github.com/covid19datahub/COVID19") %>% html_nodes("table")
policies.levels <- policies.levels[policies.levels %>% html_nodes("thead") %>% html_text() %>% grepl(pattern = "Variable.*Description")][[1]] %>% html_table()

policies.levels <- policies.levels[,2] %>% matrix(dimnames = list(policies.levels[,1], NULL)) %>% drop()

policies.levels <- lapply(policies.levels, function(v) {
  if (grepl("\\d+:", v)) {
    v <- v %>%
      sub(pattern=".\\s+More\\s+details$", replacement="") %>%
      strsplit("\\s+-\\s+") %>%
      lapply(strsplit, split="\\s*:\\s*") %>%
      unlist() %>% matrix(ncol = 2, byrow = T)
    w <- v[,2]
    names(w) <- v[,1]
    w
  } else NULL
})
policies.levels <- policies.levels[!sapply(policies.levels, is.null)]

policies <- names(policies.levels)
