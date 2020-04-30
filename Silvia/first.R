install.packages("COVID19")
library(COVID19)
require("COVID19")
covid19(level = 1)
covid19("USA", level = 1)
covid19("CHE", level = 2)
covid19(
  ISO = NULL,
  level = 1,
  start = "2019-01-01",
  end = Sys.Date(),
  vintage = FALSE,
  raw = FALSE,
  cache = TRUE
)
covid19(ISO = NULL, level = 1, start = "2019-01-01", end = Sys.Date(), vintage = FALSE, raw = FALSE, cache = TRUE)
covid19("ITA", level = 2)
covid19()
install.packages("devtools")
