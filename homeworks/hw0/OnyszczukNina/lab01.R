###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 1            ###
###########################################


# 0. Prowadzący  -------------------------------------------------------------

# Anna Kozak/Antoni Chudy/Iza Danielewska/Dawid Płudowski/Katarzyna Woźnica
# Kontakt: MS Teams lub mail


# 1. Sposób pracy na zajęciach laboratoryjnych -------------------------------

# a) pracujemy w R (większość semestru) i Python
# b) pracujemy na przygotowanych plikach, które będą na repozytorium przedmiotu
# c) podczas zajęć prowadzący będzie wprowadzał zagdanienia, a następnie będzie rozwiązywanie zadań w celu utrwalenia wiadomości
# d) kolejna porcja materiału będzie omawiana jeżeli większość grupy wykona zadane zadanie 
# e) wszelkie pytania czy to związane z kodem, pracą domową czy kwestie teoretyczne proszę śmiało zgłaszać prowadzącemu 


# 2. Materiały ------------------------------------------------------------

# Repozytorium na GitHub
# https://github.com/kozaka93/2025Z-DataVisualizationTechniques


# 3. Jak działa GitHub? ---------------------------------------------------

# Jak zgłosić pracę domową/projekt? (fork, commit, pull request)
# https://rogerdudler.github.io/git-guide/


# 4. Podstawy R - rozgrzewka ----------------------------------------------

data(mtcars)
head(mtcars)

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?


# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, c(2,3)]

# Jak wybieramy kolumny po nazwach? 
mtcars$disp
mtcars['disp']
mtcars[,'disp']

vars <- c('cyl', 'disp', 'mpg')
for (var in vars){
  x <- mtcars[, var]
  #x <- mtcars$var - nie zadziała
  print(x[1:5])
}

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[,c('am','wt','mpg')]

# Jak wybierać jedną kolumnę?

# Uwaga na przecinek i wybór kolumn poprzez indeksy
mtcars[c(1,2)] #<- pierwsza i druga kolumna
#mtcars[1,2] <- jeden element

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars['cyl'])
length(unique(mtcars$cyl))

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów 
# o wartości zmiennej "cyl" równej 4?
mtcars$cyl == 4
sum(mtcars$cyl==4)
mtcars$drat[mtcars$cyl == 4]
length(mtcars$drat[mtcars$cyl == 4])

mean(mtcars$drat[mtcars$cyl == 4])

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 
mtcars$am
table(mtcars$am)

# Prosty wykres


# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot
barplot(table(mtcars$cyl))

# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)

# W pliku zapisuj sposób rozwiązania gry.



# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------
#1
data(employees)
head(employees)
Insecure_login <- employees$login[employees$name == 'John' & employees$surname == 'Insecure']
Insecure_login
proton(action = "login", login=Insecure_login)
#2
top1000passwords
for (var in top1000passwords){
  if (proton(action = "login", login=Insecure_login, password=var)==TRUE){
    pass <- var
  }
}
pass
#lub
for (var in top1000passwords){
  proton(action = "login", login=Insecure_login, password=var)
}
#3
data(logs)
head(logs)
Pietraszko_login <- employees$login[employees$name == 'Slawomir' & employees$surname == 'Pietraszko']
Pietraszko_login
Pietraszko_logs <- logs[logs$login == Pietraszko_login,]
tmp <- table(Pietraszko_logs$host)
tmp[tmp>0]
host_most_often <- names(sort(tmp, decreasing = TRUE)[1])
proton(action = "server", host=host_most_often)
#4
data(bash_history)
head(bash_history)
unique(bash_history)
unique(strsplit(bash_history, ' '))
