library(dplyr)
library(httr)
library(xml2)

# https://www.boxofficemojo.com/year/world/2019/

url <- "https://www.boxofficemojo.com/year/world/"
for (i in seq(1977, 2019)){
  url_y <- paste0(url, i,"/")
  doc <- read_html(url_y)
  rank <- xml_find_all(doc, "//*[@id='a-page']/main//*[@id='table']//tr/td[1]")%>% xml_text()
  release_group <- xml_find_all(doc, "//*[@id='a-page']/main//*[@id='table']//tr/td[2]")%>% xml_text()
  worldwide <- xml_find_all(doc, "//*[@id='a-page']/main//*[@id='table']//tr/td[3]")%>% xml_text()
  domestic <- xml_find_all(doc, "//*[@id='a-page']/main//*[@id='table']//tr/td[4]")%>% xml_text()
  domestic_percent <- xml_find_all(doc, "//*[@id='a-page']/main//*[@id='table']//tr/td[5]")%>% xml_text()
  foreign <- xml_find_all(doc, "//*[@id='a-page']/main//*[@id='table']//tr/td[6]")%>% xml_text()
  foreign_percent <- xml_find_all(doc, "//*[@id='a-page']/main//*[@id='table']//tr/td[7]")%>% xml_text()
  df_y <- data.frame(rank = rank,
                     release_group = release_group,
                     worldwide = worldwide,
                     domestic = domestic,
                     domestic_percent = domestic_percent,
                     foreign = foreign, 
                     foreign_percent = foreign_percent, stringsAsFactors = FALSE) %>% 
    mutate(year = i)
  df <- rbind(df, df_y)
}

write.csv(df, "gross.csv")

gross <- read_csv("gross.csv")
colnames(gross)[3] <- "originalTitle"
gross <- gross[, -1]
gross$worldwide <- str_replace_all(gross$worldwide, "\\$", "") %>% 
  str_replace_all(",", "") %>% 
  as.numeric()
df_gross <- right_join(filter(df_ratings, titleType == "movie"), gross, by ="originalTitle")
df_gross <- filter(df_gross, startYear == year)
write.csv(df_gross, "gross_ratings.csv")

