# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)
proton()

employees[employees$name=="John" & employees$surname=="Insecure", "login"]

proton(action="login", login="johnins")

for (i in 1:length(top1000passwords)) {
  if (proton(action="login", login="johnins", password=top1000passwords[i]) == "Success! User is logged in!") {
    proton(action="login", login="johnins", password=top1000passwords[i])
  }
}

employees[employees$surname == "Pietraszko", "login"]
(host_num <- table(logs[logs$login=="slap", "host"]))
most <- names(which.max(host_num))

proton(action = "server", host=most)

table(sapply(strsplit(bash_history, " "), `[`, 1))
#password -> DHbb7QXppuHnaXGN

