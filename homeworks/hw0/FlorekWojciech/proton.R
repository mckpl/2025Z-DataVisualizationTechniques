install.packages("proton")
library(proton)
proton()
#1
employees[employees$name=="John",employees$surname=="Insecure"]
employees[210:230,]
proton(action="login",login="johnins")
proton(action = "login", login="johnins", password="ABC")
#2
top1000passwords
for (i in 1:1000)
  proton(action = "login", login="johnins", password=top1000passwords[i])
#3
logs
df <- data.frame(table(logs$host,logs$login=="johnins"))
df <- df(df$Var2==TRUE)
max(df$Freq)
df$Var1[df$Freq==573]
df$Freq[40:60]
df$Freq[48]
proton(action = "server", host="194.29.178.91")
