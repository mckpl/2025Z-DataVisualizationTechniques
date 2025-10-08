install.packages("proton")
library(proton)
proton()
head(employees)
employees[(employees$name=="John") & (employees$surname=="Insecure"),]

proton(action = "login", login="johnins")
head(top1000passwords)
head(table(top1000passwords))
for(i in top1000passwords){
proton(action = "login", login="johnins", password=i)
}
head(logs)
employees[employees$surname=="Pietraszko",]

 data.frame(table(logs[logs$login=="slap","host"]))
x <- data.frame(table(logs[logs$login=="slap","host"]))
for(i in x){
  proton(action = "server", host=i)
}