install.packages("proton")
library(proton)
#1
employees$login[employees$name=='John' & employees$surname=='Insecure']
#2
for(password in top1000passwords){
  proton(action = "login", login="johnins", password=password)
}
#3
merged <- merge(employees, logs, by.y = "login")
tmp<-table(merged$host[merged$surname=='Pietraszko'])
sort(tmp, decreasing = TRUE)[1]
# 194.29.178.16

#4
head(bash_history, 10)
strsplit(bash_history, ' ')
