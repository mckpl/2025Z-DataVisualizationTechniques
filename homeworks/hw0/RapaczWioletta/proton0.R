install.packages("proton")
library(proton)
proton()
tmp <- employees[employees$name == "John",]
tmp[tmp$surname == "Insecure",]

proton(action = "login", login="johnins")

for(i in 1:length(top1000passwords)){
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

max(table(logs[logs$login == "johnins","host"])) #indeks
table(logs[logs$login == "johnins","host"])[max(table(logs[logs$login == "johnins","host"]))]

"193.0.96.13.6 "
proton(action = "server", host="193.0.96.13.6")