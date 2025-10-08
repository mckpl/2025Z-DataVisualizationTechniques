# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)

proton()

data("employees")
head(employees)

employees[employees$name == 'John' & employees$surname == 'Insecure',]
employees[employees$surname == 'Pietraszko',]
# johnins

proton(action='login',login="johnins")

data("top1000passwords")
head(top1000passwords)

for (p in top1000passwords){
  proton(action = 'login',login = 'johnins', password = p)
}

data("logs")
head(logs)


tmp <- data.frame(logs[logs$login=="slap",c("host",'login')])

