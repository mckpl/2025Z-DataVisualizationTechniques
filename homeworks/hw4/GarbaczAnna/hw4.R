#link do posta na platformie X:
#https://x.com/KaraGodsey/status/1716437609872810061

#link do repozytorium TidyTuesday z danymi (tak jak jest napisane w rezpozytorium, ja skorzystałam z pakietu "taylor" zamiast z csv):
#https://github.com/rfordatascience/tidytuesday/tree/main/data/2023/2023-10-17

#Błędy poprzedniej wizualizacji:
#1. Wykres typu donut jest złą formą wizualizacji danych, skala długości łuków jest myląca. 
#Co więcej jednostki nie są proporcjonalne na okręgu, łatwo jest przeszacować liczby w sektorach zewnętrznych.
#2. Brak legendy, kolory zostały wytłumaczone tylko poprzez napisy, które mogą być przeoczone przez odbiorcę. 




library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("taylor")
library(taylor)
library(patchwork)



major<-taylor_all_songs %>% 
  transmute(key_mode_new=case_when( key_mode=="A# major" ~ "Bb major",
                             key_mode=="C# major" ~ "Db major",
                             key_mode=="D# major" ~ "Eb major",
                             key_mode=="F# major" ~ "Gb major",
                             key_mode=="G# major" ~ "Ab major",
                             .default =  key_mode
  )) %>% 
  group_by(key_mode_new) %>% 
  drop_na(key_mode_new) %>% 
  summarise(n=n()) %>% 
  separate(key_mode_new,into=c("key","minor_major"),sep=" ") %>% 
  filter(minor_major=="major")%>% 
  ggplot(aes(x = key, y = n, fill = minor_major)) +
  geom_col() +
  labs(x="key",
       y="",
       fill="Mode name  ") +
  scale_fill_manual(
    values = c(
      "minor" = "#84455A",
      "major" = "#80D0EA"
    )
  )+
  theme_light()


minor<-taylor_all_songs %>% 
  transmute(key_mode_new=case_when( key_mode=="A# minor" ~ "Bbm minor",
                                    key_mode=="C# minor" ~ "C#m minor",   
                                    key_mode=="D# minor" ~ "Ebm minor",
                                    key_mode=="F# minor" ~ "F#m minor",  
                                    key_mode=="G# minor" ~ "G#m minor",
                                    .default =  key_mode
  )) %>% 
  group_by(key_mode_new) %>% 
  drop_na(key_mode_new) %>% 
  summarise(n=n()) %>% 
  separate(key_mode_new,into=c("key","minor_major"),sep=" ") %>% 
  filter(minor_major=="minor")%>% 
  ggplot(aes(x = key, y = n, fill = minor_major)) +
  geom_col() +
  labs(x="key",
       y="",
       fill="") +
  scale_fill_manual(
    values = c(
      "minor" = "#84455A",
      "major" = "#80D0EA"
    )
  )+
  theme_light()+ 
  scale_y_continuous(limits = c(0, 67))


  major+minor+
    plot_layout(guides = "collect") +plot_annotation(title="Number of songs written in each key across all Taylor Swift's albums")&
    theme(legend.position = 'bottom')

 
  
#Zastosowane ulepszenia:
#1. Zastosowałam wykres słupkowy, przez co dane są bardziej czytelniejsze.
#2. Mamy 2 wykresy obok siebie, więc łatwiej jest zauważyć różnicę w liczbie piosenek w kluczu minor i major.
#3. Dodana legenda poprawia czytelność wykresu.
  
    
    
    
    
    