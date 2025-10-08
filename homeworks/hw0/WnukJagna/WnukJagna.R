install.packages("proton")
library("proton")
proton()
employees[((employees$name == "John")&(employees$surname == "Insecure")),]
proton(action = "login", login="johnins")
top1000passwords
for (pass in top1000passwords){
  proton(action = "login", login="johnins",password=pass)
}

head(logs)
employees[(employees$surname == "Pietraszko"),]
sort(table(logs$host[(logs$login=="slap")]),decreasing = TRUE)
proton(action = "server", host="194.29.178.16")

head(bash_history,40)

#unique(strsplit(bash_history, " "))

       