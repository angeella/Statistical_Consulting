require(rvest)

news <- read_html("https://www.thinkglobalhealth.org/article/updated-timeline-coronavirus") %>%
  html_nodes("div.o-article-body__content")

news %>% html_nodes("strong") %>% html_text()

news %>% html_nodes("p + ul")
