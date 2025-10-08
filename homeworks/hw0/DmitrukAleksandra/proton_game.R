install.packages("proton")
library(proton)

proton()

# 1.

employees[employees$surname == "Insecure", ] #217 John Insecure johnins

proton(action = "login", login="johnins")

# 2. 

for(i in 1:1000){
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

# 3.
employees[employees$surname == "Pietraszko", ] #477 Slawomir Pietraszko  slap
tmp <- logs[logs$login == "slap", ]

hosty <- data.frame(table(tmp["host"]))
max(hosty$Freq)
hosty[hosty$Freq == 112, ]

proton(action = "server", host="194.29.178.16")

# 4. 
# Problem 4: Find the Pietraszko's password.
# 
# In the `bash_history` dataset you will find all commands and parameters which have ever been entered.
# Try to extract from this dataset only commands (only strings before space) and check whether one of them looks like a password.

split(bash_history, " ", drop = TRUE)

proton(action = "login", login="slap", password = "")
