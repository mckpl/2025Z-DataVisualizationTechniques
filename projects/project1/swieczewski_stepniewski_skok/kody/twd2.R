df <- read.csv("who.csv")
df <- df[,-c(4,5)]
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr) 

df <- df %>%
  pivot_longer(
    cols = c(male,female),
    names_to = "gender",
    values_to = "percentage"
  )

ggplot(df, aes(x = factor(year), y = percentage, fill = gender)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("male" = "steelblue", "female" = "lightpink"),
    labels = c("male" = "Mężczyźni", "female" = "Kobiety")
  ) +
  labs(
    title = "Osoby spełniające wytyczne WHO dotyczące aktywności fizycznej (w %)",
    subtitle = "Z uwzględnieniem aktywności transportowej, wyłączając chodzenie",
    x = NULL,
    y = NULL,
    fill = NULL 
  ) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle=element_text(hjust=0.5))


df2 <- read.csv("sport.csv",sep=";")
df2 <- df2[,-1]


df_long <- df2 %>%
  pivot_longer(
    cols = -Nazwa,
    names_to = "sport_code",
    values_to = "value"
  ) %>%
  mutate(
    year = str_extract(sport_code, "\\d{4}")  # Extract 4-digit years
  )


df_wide <- df_long %>%
  mutate(sport_main = str_extract(sport_code, "^[^.]+")) %>%
  # Remove duplicates if any exist
  distinct(Nazwa, year, sport_main, .keep_all = TRUE) %>%
  pivot_wider(
    names_from = sport_main,
    values_from = value,
    id_cols = c(Nazwa, year)
  )

df_clean <- df_wide %>%
  select(-1, -last_col()) %>%    
  rename(sportmotorowy = sport) %>%         
  slice(-n()) 


library(ggplot2)

# Line plot with years on x-axis, numbers on y-axis, and sports as colored lines
ggplot(df_clean, aes(x = year, y = value, color = xyz, group = xyz)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Sports Participation Over Time",
    x = "Year",
    y = "Number of Participants", 
    color = "Sport"
  ) +
  theme_minimal() +
  theme(legend.position = "right")


df_long2 <- df_clean %>%
  pivot_longer(
    cols = -year,  # Keep year column as identifier
    names_to = "sport",
    values_to = "participants"
  )

library(patchwork)


plot1 <- df_long2 %>%
  filter(sport %in% c("lekkoatletyka", "strzelectwo", "tenis")) %>%
  ggplot(aes(x = year, y = participants, color = sport, group = sport)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Liczba trenujących w sekcjach sportowych na przestrzeni lat",
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle=element_text(hjust=0.5))


library(RColorBrewer)

# Filter first
df_filtered <- df_long2 %>%
  filter(!sport %in% c("lekkoatletyka", "strzelectwo", "tenis")) %>%
  mutate(year = as.numeric(year))
# Get palette for *filtered* sports only
sports <- sort(unique(df_filtered$sport))
base_cols <- brewer.pal(length(sports), "Paired")
names(base_cols) <- sports

# Override the color for the one sport
base_cols["sportmotorowy"] <- "black"
base_cols["badminton"] <- "white"
base_cols["kulturystyka"] <- "darkred"

plot2 <- df_filtered %>%
  ggplot(aes(x = year, y = participants, color = sport)) +
  geom_line(linewidth = 1,alpha=0.9) +
  geom_point(size = 2,alpha=0.85) +
  scale_color_manual(values = base_cols) +
  theme_dark() +
  labs(x = NULL, y = NULL, color = NULL)
 
plot1 / plot2



dftemp <- df_long2 %>%
  filter(year==1999 | year==2018) %>% 
  pivot_wider(
    names_from = year,
    values_from = participants
  ) %>%
  drop_na() %>%
  mutate(change=(`2018`-`1999`)/`1999`*100) 

ggplot(dftemp,aes(x=reorder(sport,change),y=change,fill=change)) +
  geom_bar(stat="identity",color = "black") + 
  theme_minimal() + 
  coord_flip() + 
  labs(x=NULL,y=NULL,title="% zmiana popularności danego sportu między 1999,a 2018")


  
dft <- data.frame(
  sport = c("piłka nożna","siatkówka","pływanie","karate","strzelectwo","lekkoatletyka","koszykówka","piłka ręczna","tenis stołowy","sporty walki pozostałe","inne"),
  pct =  c(39.4,6.5,4.3,4.3,3.7,3.7,3.7,3.4,2.4,6.9,21.7)
)

# Create waffle data
total_squares <- 100  # total squares in the grid
dft <- dft %>%
  mutate(squares = round(pct / sum(pct) * total_squares))

# Expand into individual squares
waffle_data <- dft %>%
  uncount(squares) %>%  # replicate rows
  mutate(id = row_number())

# Compute grid positions
ncol <- 10
waffle_data <- waffle_data %>%
  mutate(x = (id - 1) %% ncol + 1,
         y = (id - 1) %/% ncol + 1)

# Plot
ggplot(waffle_data, aes(x = x, y = y, fill = sport)) +
  geom_tile(color = "white") +
  scale_y_reverse() +  # optional: start from top
  theme_minimal() +
  labs(title = "Participation in Sports in Poland", x = NULL, y = NULL) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


# Multiply percentages by 10 to get ~1000 people
dft <- dft %>%
  mutate(total_people = pct * 10)

# Expand into individual “people”
people_df <- dft %>%
  uncount(total_people) %>%
  mutate(id = row_number())

ncol <- 40

# Create grid positions
people_df <- people_df %>%
  mutate(
    x = (id - 1) %% ncol + 1,
    y = (id - 1) %/% ncol + 1
  )

# Plot
ggplot(people_df, aes(x = x, y = -y, color = sport)) +  # -y for top row first
  geom_text(label = "\U1F464", size = 6) +             # bigger emoji
  scale_x_continuous(expand = c(0,0), limits = c(0.5, ncol + 0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(-max(people_df$y) - 0.5, 0.5)) +
  coord_fixed() +
  theme_void() +
  labs(title = "Ćwiczący w klubach według sportu") +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )








