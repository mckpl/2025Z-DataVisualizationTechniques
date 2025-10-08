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
head(mtcars,10)

?mtcars

colnames(mtcars)
rownames(mtcars)

# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?


# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10,1]
mtcars[1:10,c(2,3)]
mtcars[1:10, 2:3]

# Jak wybieramy kolumny po nazwach? 
mtcars$disp
mtcars['disp']
mtcars[,'disp']

vars <- c('cyl','disp','mpg')
for(var in vars){
  x <- mtcars[,var]
  print(x[1:5])
}

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[, c('am','wt','mpg')]

# Jak wybierać jedną kolumnę?

# Uwaga na przecinek i wybór kolumn poprzez indeksy
mtcars[c(1,2)]
mtcars[1,2]

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
# calkowita
# kategorie 
#


# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)
length(unique(mtcars$cyl))

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów 
# o wartości zmiennej "cyl" równej 4?
mtcars$cyl == 4
mean(mtcars$drat[mtcars$cyl == 4])

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 
mtcars$am
x <- table(mtcars$am)
x[1]
names(x)
# Prosty wykres


# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg, mtcars$hp)


# Zmienna "cyl" - barplot
barplot(mtcars$cyl)
barplot(table(mtcars$cyl))

# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)

# W pliku zapisuj sposób rozwiązania gry.
data("employees")
head(employees)
employees[employees$name == 'John',]

data("top1000passwords")
table(top1000passwords)
unique(top1000passwords)
for(password in top1000passwords){
  proton(action = "login", login="johnins", password=password)
}
data("logs")
head(logs)
employees[employees$surname == 'Pietraszko',]
logs[logs$login=="slap",]
hel <- table(logs[logs$login =="slap",'host'])

proton(action = "server", host="194.29.178.16")

data("bash_history")
head(bash_history)

y <- strsplit(bash_history, ' ')
table(y)

# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------

