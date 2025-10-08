install.packages("proton")
library(proton)
proton()
employees
employees[employees['name'] == "John"& employees['surname'] == "Insecure", 'login']
proton(action = "login", login="johnins")
top1000passwords[1]
for (var in top1000passwords){
  proton(action = "login", login="johnins", password=var)
}
proton(action = "login", login="johnins", password="freepass")
logs


#Well done! This is the right password!
#Bit used John Insecure's account in order to log into the Proton server.
#It turns out that John has access to server logs.
#Now, Bit wants to check from which workstation Pietraszko is frequently logging into the Proton server. Bit hopes that there will be some useful data.  

#Logs are in the `logs` dataset. 
#Consecutive columns contain information such as: who, when and from which computer logged into Proton.

#Problem 3: Check from which server Pietraszko logs into the Proton server most often.

#Use `proton(action = "server", host="XYZ")` command in order to learn more  about what can be found on the XYZ server.
#The biggest chance to find something interesting is to find a server from which Pietraszko logs in the most often.

piet_login <- employees[employees['surname'] == "Pietraszko", 'login']
tmp <- table(logs[logs$login==piet_login, 'host'])
top_host <- sort(tmp, decreasing=TRUE)[1]
top_host
proton(action = "server", host="194.29.178.16")

head(bash_history)

