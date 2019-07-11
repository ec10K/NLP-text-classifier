## Web scraping project

library('rvest')


url <- "https://www.cia.gov/library/publications/the-world-factbook/rankorder/2004rank.html"

webpage <- read_html(url)

rank_data_html <- html_nodes(webpage,"td.region")

gdp_data_html <- html_nodes(webpage,"td[1]" )

rank_data <- html_text(rank_data_html)

gdp_data <- html_text(gdp_data_html)

##gdp_data <- as.numeric(gdp_data)


rank_data

gdp_data


gdP_DF <- data.frame(Rank = rank_data,GDP = gdp_data)
str(gdP_DF)

