install.packages("proton")
library(proton)

proton()
head(employees)
employees[employees$name == "John",]

employees[employees$surname == "Pietraszko",]

proton(action = "login", login="johnins")
top1000passwords

for (var in top1000passwords) {
  proton(action = "login", login="johnins", password=var)
}

dim(logs)
tmp <- table(logs$host[logs$login == "slap"])
tmp[tmp>0]
x <- sort(tmp, decreasing = TRUE)[1]

proton(action = "server", host=names(x))
head(bash_history)

strsplit(bash_history, ' ')
