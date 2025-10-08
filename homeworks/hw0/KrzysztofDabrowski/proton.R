install.packages("proton")
library(proton)

proton()

employees[employees$name == "John",]

proton(action = "login", login="johnins")

top1000passwords
for (p in top1000passwords) {
  proton(action = "login", login="johnins", password=p)
}

logs

john.logs <- logs[logs$login == "johnins", ]
john.logs$host
tail(sort(table(john.logs$host)))
data.frame(table(john.logs$host))
proton(action = "server", host="194.29.178.13")
