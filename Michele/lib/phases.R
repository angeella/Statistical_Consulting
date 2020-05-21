
calc.phases <- function(dataset, low=0.25, mid=0.50, high=0.75) {
  require(dplyr)
  dataset %>%
    left_join(
      dataset %>%
        left_join(
          dataset %>%
            group_by(id) %>%
            summarise(max_stringency_index = max(stringency_index)),
          by="id"
        ) %>%
        group_by(id) %>%
        summarise(
          date25=min(date[stringency_index >= low*max_stringency_index], na.rm=T),
          date50=min(date[stringency_index >= mid*max_stringency_index], na.rm=T),
          date75=min(date[stringency_index >= high*max_stringency_index], na.rm=T)
        ),
      by="id"
    ) %>%
    mutate(
      phase=((date >= date25) + (date >= date50) + (date >= date75))/3
    )
}
