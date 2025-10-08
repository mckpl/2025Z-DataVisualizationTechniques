proton()
employees
head(employees)
employees[employees["name"]=="John"]
proton(action = "login", login="johnins")
head(top1000passwords)
top1000passwords[1000]
proton(action = "login", login="johnins",password="freepass")
for(i in 1:1000)
{
  proton(action = "login", login="johnins",password=top1000passwords[i])
}
head(logs)
aggregate(logs,"host",FUN = "sum")
employees[employees["surname"]=="Pietraszko"]
table(logs[logs["login"]=="slap"])
proton(action = "server", host="194.29.178.16")
head(bash_history)
bash_history
