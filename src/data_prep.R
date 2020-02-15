library(readr)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)


df_ratings <- read_tsv('title.ratings.tsv', na = "\\N", quote = '')
df_ratings %>% head()

df_basics <- read_tsv('title.basics.tsv', na = "\\N", quote = '')
df_basics %>% head()

df_gross <- read_csv('gross_ratings.csv')

df_ratings <- df_ratings %>% left_join(df_basics)

df_actors <- read_tsv('name.basics.tsv', na = "\\N", quote = '') %>%
  filter(str_detect(primaryProfession, "actor|actress"))  %>%
  select(nconst, primaryName, birthYear)
df_actors %>% head()

df_principals <- read_tsv('title.principals.tsv', na = "\\N", quote = '') %>%
  filter(str_detect(category, "actor|actress")) %>%
  select(tconst, ordering, nconst, category) %>%
  group_by(tconst) %>%
  filter(ordering == min(ordering))

df_principals <- df_principals %>% left_join(df_actors)
df_ratings <- df_ratings %>% left_join(df_principals)
head(df_ratings)


df_ratings_movie <- df_ratings %>%
  filter(titleType == "movie", !is.na(primaryName), numVotes >= 10) %>%
  select(tconst, averageRating, numVotes, titleType, primaryTitle, isAdult, startYear, genres, category, primaryName, birthYear) %>%
  mutate(leadAge = startYear- birthYear) 
df_genre_ratings <- df_ratings_movie %>%
  select(averageRating, numVotes, startYear, genres, category, leadAge) %>%
  unnest_tokens(genre, genres, token = str_split, pattern = ',') %>%
  filter(!(genre %in% c("game-show", "reality-tv", "short", "talk-show", "film-noir", NA))) %>%
  group_by(startYear, genre, category) %>%
  arrange(startYear)

df_gross_genre <- df_gross %>%
  unnest_tokens(genre, genres, token = str_split, pattern = ',') %>%
  filter(!(genre %in% c("game-show", "reality-tv", "short", "talk-show", "film-noir", NA))) %>%
  group_by(startYear, genre, category) %>%
  arrange(startYear)

df_gross_gender <- df_gross %>%
  group_by(startYear, category) %>%
  filter(!is.na(category)) %>%
  arrange(startYear)

df_gender_diff <- df_gross_gender%>%
  summarise(n = n()) %>%
  spread(category, n) %>%
  mutate(difference = actor - actress)


write.csv(df_ratings, "ratings.csv")
write.csv(df_basics, "basics.csv")
write.csv(df_actors, "actors.csv")
write.csv(df_principals, "principals.csv")
write.csv(df_ratings_movie, "ratings_movie.csv")
write.csv(df_genre_ratings, "genre_ratings.csv")
write.csv(df_gross_genre, "gross_genre.csv")
write.csv(df_gross_gender, "gross_gender.csv")
write.csv(df_gender_diff, "gender_diff.csv")

