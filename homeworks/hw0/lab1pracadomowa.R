#install.packages("proton")
library(proton)
proton()

#1
employees[employees$name == "John",] #johnins


proton(action = "login", login="johnins")

#2
responses <- sapply(top1000passwords, function(x) proton(action='login', login='johnins', password=x))
which(responses != "Password or login is uncorrect") #120

proton(action = "login", login="johnins", password="q1w2e3r4t5")

#3
employees[employees$surname=="Pietraszko",] # login: slap
l1 <- logs[logs["login"] == "slap",]
which.max(table(logs[logs$login == 'slap','host']))
proton(action = "server", host="XYZ")