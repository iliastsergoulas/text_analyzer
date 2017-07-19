library(rvest)
eu_dgagri_press <- read_html("http://europa.eu/rapid/search-result.htm?query=42&locale=en&page=1")

titles <- eu_dgagri_press %>% 
    html_nodes("a") %>%
    head()

print(titles)