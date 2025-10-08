# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)

proton()

# 1
head(employees)
login <-  employees[employees$name == "John" & employees$surname == "Insecure", "login"]
proton(action = "login", login=login)

# 2
head(top1000passwords)
for (p in top1000passwords) {
  result <- proton(action = "login", login=login, password=p)
  if (result == "Success! User is logged in!") {
    password <- p
  }
}
proton(action = "login", login=login, password=password)

# 3
head(logs)
unique(logs$login)
sort(table(logs$login))

sort(table(logs$host))
proton(action = "server", host="194.29.178.91")

login <-  employees[employees$name == "Slawomir" & employees$surname == "Pietraszko", "login"]
data.frame(table(logs[logs$login == "slap", "host"]))

proton(action = "server", host="194.29.178.16")

# 4
head(bash_history)
           