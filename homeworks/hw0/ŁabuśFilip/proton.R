install.packages("proton")
library(proton)

proton()
head(employees)
employees[employees["name"] == "John" & employees["surname"] == "Insecure","login"]
head(top1000passwords)
a <- 'Password or login is incorrect!'
i <- 1
for (i in top1000passwords){
  a <- proton(action = "login", login="johnins", password=i)
  if (a == "Success! User is logged in!"){
    cat(i)
  }
  
  
}
head(logs)
p <- employees[employees$surname =="pietraszko", "login"]

t <- table(logs[logs$login == p,"host"])
t[t>0]
h <- sort(t, decreasing = TRUE)[1]
h
proton(action = "server", host = h)