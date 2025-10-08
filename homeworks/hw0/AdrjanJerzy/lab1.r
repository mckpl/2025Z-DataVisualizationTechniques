install.packages("proton")
library(proton)

proton()

pietraszko = employees[employees$surname == "Pietraszko","login"]

proton(action="login", login=employees[employees$name == "John" & employees$surname == "Insecure", "login"])

for(i in 1:length(top1000passwords)){
  proton(action="login", login=employees[employees$name == "John" & employees$surname == "Insecure", "login"],
       password=top1000passwords[i])
}

proton(action="server", host=names(sort(table(logs[logs$login==pietraszko,"host"]), decreasing=TRUE)[1]))

proton(action="login", login=pietraszko, password=bash_history[!grepl("[a-z]+( [^ ]*|$)", bash_history)])