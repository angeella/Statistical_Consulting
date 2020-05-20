
require(dplyr)

policies <- covid19(level = 1)
policies <- policies.levels[,grepl("(closing|cancel|restriction|campaign|policy|tracing)", colnames(policies.levels))] %>%
  sapply(range)
