# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)

# W pliku zapisuj sposób rozwiązania gry.

employees[employees$name=='John' & employees$surname=='Insecure', 'login']
proton(action="login", login="johnins")

for (pas in top1000passwords) {
  mes <- proton(action = "login", login="johnins", password=pas)
  if (mes == 'Success! User is logged in!') {
    break
  }
}

employees[employees$surname == "Pietraszko",]
pietraszko_hosts <- logs[logs$login=='slap', 'host']
top_host <- names(sort(table(pietraszko_hosts), decreasing=TRUE))[1]
proton(action="server", host=top_host)


# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------