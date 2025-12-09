df <- read.csv("games.csv")
library(dplyr)
library(tidyr)
library(ggplot2)

head(df)

df <- df[c("white_rating","black_rating","opening_name","id")]


max(df$white_rating)
min(df$white_rating)

"
bins: 
<1401
1401-1600
1601-1800
1801-2000
2001-2200
>2200 
"
bins <- c(1400,1600,1800,2000,2200,Inf)
labels <- c("1401-1600","1601-1800","1801-2000","2001-2200","2200+")
df$white <- cut(df$white_rating,breaks=bins,labels=labels,right=TRUE)
df$black <- cut(df$black_rating,breaks=bins,labels=labels,right=TRUE)

df <- df %>%
  mutate(white=as.character(white),black=as.character(black)) %>%
  filter(!is.na(white) & !is.na(black)) %>%
  filter(as.character(white)==as.character(black)) %>%
  rename(unified=white) %>%
  select(-black)

top_openings <- df %>%
  count(opening_name) %>%
  arrange(desc(n)) %>%
  head(25)

library(stringr)

df <- df %>%
  mutate(opening_name = case_when(
    str_detect(opening_name,regex("sicilian",ignore_case=TRUE)) ~ "Sicilian Defense",
    str_detect(opening_name,regex("scandinavian",ignore_case=TRUE)) ~ "Scandinavian Defense",
    str_detect(opening_name,regex("italian",ignore_case=TRUE)) ~ "Italian Game",
    str_detect(opening_name,regex("queen's gambit",ignore_case=TRUE)) ~ "Queen's Gambit",
    str_detect(opening_name,regex("philidor defense",ignore_case=TRUE)) ~ "Philidor Defense",
    str_detect(opening_name,regex("caro",ignore_case=TRUE)) ~ "Caro-Kann Defense",
    str_detect(opening_name,regex("french defense",ignore_case=TRUE)) ~ "French Defense",
    str_detect(opening_name,regex("queen's pawn",ignore_case=TRUE)) ~ "Queen's pawn game",
    str_detect(opening_name,regex("king's pawn",ignore_case=TRUE)) ~ "King's pawn game",
    TRUE ~ opening_name
  )
  )
  
top_openings2 <- df %>%
  count(opening_name) %>%
  arrange(desc(n)) %>%
  head(10)

top_cat <- df %>%
  group_by(unified,opening_name) %>%
  summarise(n=n(), .groups="drop") %>%
  arrange(unified,desc(n)) %>%
  group_by(unified) %>%
  slice_head(n=5) %>%
  mutate(rank=row_number()) %>%
  ungroup()

top_cat

keep_openings <- c(
  "French Defense",
  "Queen's pawn game",
  "Sicilian Defense",
  "Italian Game",
  "Queen's Gambit"
)

df_bump <- top_cat %>%
  filter(opening_name %in% keep_openings) %>%
  select(-rank) %>%
  group_by(unified) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()

prototype <- ggplot(df_bump, aes(x = unified,y = rank,group = opening_name,color = opening_name)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_y_reverse(breaks = 1:5) +
  scale_x_discrete(expand = expansion(add = 0.2)) +  
  labs(
    title = "Popularity of given openings in their respective rating ranges",
    y = NULL,
    x = "Rating",
    color = "Opening"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10))
  )

prototype

library(RColorBrewer)

final <- ggplot(top_cat, aes(x = unified, y = n, fill = opening_name)) +
  geom_col(position = position_dodge2(width = 0.8)) +
  scale_fill_brewer(palette = "Set1") + 
  labs(title = "Top 5 most popular openings in the given rating range", x = "Rating", y = "Number of Games", fill = "Opening") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

final




