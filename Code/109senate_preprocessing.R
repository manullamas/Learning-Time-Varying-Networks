# Cleaning and preprocessing 109th senate data

#Load data
path <- 'C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/US109senate/'
senate <- read.table(paste0(path,'109senate.txt'), sep = "\t", header = T)

senate <- senate[-c(1,2)]   #Remove 1st (bills to vote) and 2nd (n missing votes) columns

senate[senate==0] <- -1
senators <- cbind(c(1:100),names(senate))

#missing samples (bills) when loading the data!!!???

write.csv(senate, file = paste0(path,"109senate_clean.csv"),row.names=FALSE)
write.csv(senators, file = paste0(path,"senatorsTags.csv"),row.names=FALSE)

#TODO
#  - include in senators dataset a party tag (0 democrats, 1 republicans, 2 independent): manually??
#  - explore why there is some missing rows