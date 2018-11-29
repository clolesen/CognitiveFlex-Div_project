library(tidyverse)
library(readxl)

# QUESTIONAIRES
#load questionaires
Qi = read_xlsx("Data/Questionnaires (Cond BL) (Responses).xlsx")
Qnd = read_xlsx("Data/Questionnaires (Dyad Cond ND) (Responses).xlsx")
Qd = read.csv("Data/Questionnaires (Dyad Cond D).csv", stringsAsFactors = F)

#Changing ID column name to "ID"
colnames(Qi)[2] = "ID"
colnames(Qnd)[2] = "ID"
colnames(Qd)[2] = "ID"

#removing rows with pilot data
Qi = Qi[6:53,]
Qnd = Qnd[5:94,]
Qd = Qd[13:99,]

#Fixing IDs
#From Arnaults notes
Qd$ID[Qd$ID == "31234"] = "3_33_1"
Qd$ID[Qd$ID == "26041"] = "3_33_2" 

#Qi - Things evident from looking at the data
Qi$ID[Qi$ID == "I_4_2"] = "1_04_2"
Qi$ID[Qi$ID == "25777.0"] = "1_05_1"
Qi$ID[Qi$ID == "1-20-1"] = "1_20_1"
Qi$ID[Qi$ID == "23128.0"] = "1_31_1"


#Qnd - Things evident from looking at the data
Qnd$ID[Qnd$ID == "5.0"] = "2_04_1"
Qnd$ID[Qnd$ID == "2141.0"] = "2_14_1"
Qnd$ID[Qnd$ID == "2_24_l"] = "2_24_1"
Qnd$ID[Qnd$ID == "2-31-2"] = "2_31_2"
Qnd$ID[Qnd$ID == "2-33-1"] = "2_33_1"
Qnd$ID[Qnd$ID == "2-40-1"] = "2_40_1"



#Qd - Things evident from looking at the data
Qd$ID[Qd$ID == "3041"] = "3_04_1" 
Qd$ID[Qd$ID == "3_0_4_2"] = "3_04_2" 
Qd$ID[Qd$ID == "3-07-1"] = "3_07_1" 
Qd$ID[Qd$ID == "30907"] = "3_14_2" 
Qd$ID[Qd$ID == "3_2_7_2"] = "3_27_2" 
Qd$ID[Qd$ID == "3351"] = "3_35_1"
Qd$ID[Qd$ID == "3_34_2"] = "3_43_2"
Qd$ID[Qd$ID == "5"] = "3_44_1"

#Removing the 0 infront of pair nr
for (id in Qi$ID){
  split = strsplit(id, "")[[1]]
 if (split[3] == 0){
   new_id = paste(split[1],split[2],split[4],split[5],split[6], sep = "")
   Qi$ID[Qi$ID == id] = new_id
 } 
}
for (id in Qnd$ID){
  split = strsplit(id, "")[[1]]
  if (split[3] == 0){
    new_id = paste(split[1],split[2],split[4],split[5],split[6], sep = "")
    Qnd$ID[Qnd$ID == id] = new_id
  } 
}
for (id in Qd$ID){
  split = strsplit(id, "")[[1]]
  if (split[3] == 0){
    new_id = paste(split[1],split[2],split[4],split[5],split[6], sep = "")
    Qd$ID[Qd$ID == id] = new_id
  } 
}




#LOG FILES
#in the following i write new files with changed filename. Aftewards i have manually deleted the old files in this folder. 
log1_14_1 = read.csv("Data/all_logs/CHANGE_logfile_1_14_1_2018_Sep_25_1002.csv", row.names = 1, colClasses = "character")
log1_14_1$unique_id = "1_14_1"
log1_14_1$pair_id = 14
write.csv(log1_14_1, file = "Data/all_logs/logfile_1_14_1_2018_Sep_25_1002.csv")

log3_2_2 = read.csv("Data/all_logs/logfile_1_2_2_2018_Sep_11_1112.csv", row.names = 1, colClasses = "character")
log3_2_2$unique_id = "3_2_2"
log3_2_2$pair_id = 2
write.csv(log3_2_2, file = "Data/all_logs/logfile_3_2_2_2018_Sep_11_1112.csv")

log1_46_1 = read.csv("Data/all_logs/logfile_1_19_1_2018_Sep_28_1307.csv", row.names = 1, colClasses = "character")
log1_46_1$unique_id = "1_46_1"
log1_46_1$pair_id = 46
write.csv(log1_46_1, file = "Data/all_logs/logfile_1_46_1_2018_Sep_28_1307.csv")
Qi$ID[21] = "1_46_1"

log1_47_1 = read.csv("Data/all_logs/logfile_1_38_2_2018_Oct_24_1410.csv", row.names = 1, colClasses = "character")
log1_47_1$unique_id = "1_47_2"
log1_47_1$pair_id = 47
write.csv(log1_47_1, file = "Data/all_logs/logfile_1_47_2_2018_Oct_24_1410.csv")
Qi$ID[41] = "1_47_2"


#List of log file locations
files = list.files(path = "Data/all_logs/", pattern="*.csv", full.names = T)

#make data frame from all the log files
full_data = data.frame()
for (f in files){
  log = read.csv(f, row.names = 1, colClasses = "character")
  full_data = rbind(full_data,log)
}


#writing csv files with the clean data
write.csv(Qi, file = "Data/Qi.csv")
write.csv(Qnd, file = "Data/Qnd.csv")
write.csv(Qd, file = "Data/Qd.csv")


write.csv(full_data, file = "Data/full_data.csv")
