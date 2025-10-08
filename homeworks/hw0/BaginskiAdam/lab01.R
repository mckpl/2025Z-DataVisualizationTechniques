install.packages("proton")
library(proton)

proton()
head(employees)
employees[employees$name == "John" & employees$surname == "Insecure",]

proton(action = "login", login="johnins")

head(top1000passwords)
for (x in top1000passwords){
  proton(action = "login", login="johnins", password = x)
}
#proton(action = "server", host="XYZ")
head(logs)
d <- data.frame(logs[logs$login == 'slap',])
table(d$host)
employees[employees$name == "Slawomir" & employees$surname == "Pietraszko",]


proton(action = "server", host="194.29.178.16")
head(bash_history)
table(bash_history[!(grepl(" ", bash_history))])
proton(action = "login", login="slap", password = "DHbb7QXppuHnaXGN")
             