library(ggplot2)


setwd("~/Dropbox/Cognitive Flexibility/Pilot_measurement_data/SBJ A")
logs <- list.files(pattern="*.csv")

# create empty data frame 
data <- data.frame()

# loop through file list, import and append
for (l in logs){
  d <- read.csv(l)
  d$cumulative <- cumsum(d$correct)
  data <- rbind(data, d) 
}

#data <- read.csv("logfile_3_4_1_2018_May_29_1508.csv")

data$cumulative <- cumsum(data$correct)

data1 <- data[data$block == 'training',]
data2 <- data[data$block == 'test' & data$rule_num == 2,]
data3 <- data[data$block == 'test' & data$rule_num == 3,]
data4 <- data[data$block == 'test' & data$rule_num == 4,]


data1 <- filter(data, block == 'test') %>%
  group_by(pair_id) %>% 
  mutate(cumulative2 <- cumulative - min(cumulative))
  
data_4 <- data[data$pair_id == 4 & data$block == 'test',]  
max(data_4$reaction_time)
max(data_4$reaction_time[data_4$trial %in% 1:20])
ggplot(data1, aes(wall_time, points, group = pair_id)) +
  geom_line()

