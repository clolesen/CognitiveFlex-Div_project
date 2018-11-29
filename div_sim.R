n = 2000
k = 3
l = 6
p = 1

agents = list()
i = 1
for (o1 in 1:l){
  for (o2 in 1:l){
    for (o3 in 1:l){
      agents[[i]] = c(o1,o2,o3)
      i = i + 1
    }
  }
}


data_list = list()
i = 1
for (round in 1:100){
  
  
  space = runif(n, 0, 100)
  
  i_pos = list()
  
  for (a in 1:l^k){
    pos = p
    optima = space[pos]
    pos_list = pos
    heu = agents[[a]]
    for (x in 1:1e4){
      success = 0
      
      for (h in 1:k){
        new_p = pos + heu[h]
        if (new_p > n){new_p = new_p - n}
        search = space[new_p]
        if (search > optima){
          optima = search
          pos = new_p
          success = success + 1
          pos_list = c(pos_list,new_p)
        }
      }
      
      if (success == 0){break}
      
    }
    i_pos[[a]] = pos_list
    
  }
  
  
  for (a1 in 1:l^k){
    for (a2 in 1:l^k){
      
      if (sum(i_pos[[a1]]) & sum(i_pos[[a2]]) != 1){
        same = 0
        for (x in 1:k){
          h1 = agents[[a1]][x]
          h2 = agents[[a2]][x]
          if (h1 == h2){same = same + 1}
        }
        div = round((k-same)/k,1)
        
        pos = p
        optima = space[pos]
        pos_list = pos
        heu1 = agents[[a1]]
        heu2 = agents[[a2]]
        
        for (xx in 1:1e4){
          
          check = optima
          
          #Agent 1 loop
          for (x in 1:1e4){
            success1 = 0
            
            for (h in 1:k){
              new_p = pos + heu1[h]
              if (new_p > n){new_p = new_p - n}
              search = space[new_p]
              if (search > optima){
                optima = search
                pos = new_p
                success1 = success1 + 1
                pos_list = c(pos_list,new_p)
              }
            }
            
            if (success1 == 0){break}
            
          }
          
          #Agent 2 loop
          for (x in 1:1e4){
            success2 = 0
            
            for (h in 1:k){
              new_p = pos + heu2[h]
              if (new_p > n){new_p = new_p - n}
              search = space[new_p]
              if (search > optima){
                optima = search
                pos = new_p
                success2 = success2 + 1
                pos_list = c(pos_list,new_p)
              }
            }
            
            if (success2 == 0){break}
            
          }
          if (check == optima){break}
        }
        
        data_list[[i]] = c(
          "actor1" = a1,
          "actor2" = a2,
          "exp1" = length(unique(i_pos[[a1]])),
          "exp2" = length(unique(i_pos[[a2]])),
          "joint_exp" = length(unique(c(i_pos[[a1]], i_pos[[a1]]))),
          "exp_pair" = length(unique(pos_list)),
          "div" = div,
          "round" = round
        )
        i = i + 1
        print(paste(a1," - ", a2, " - ", round))
      }
    }
  }
}

data = as.data.frame(do.call(rbind, data_list))

max(data$exp_pair)

library(tidyverse)
ggplot(data, aes(x = as.factor(div), y = exp_pair-joint_exp)) +
  geom_point(alpha = 0.1, position=position_jitter(width = 0.4, height = 0.3))


m = lm(exp_pair-joint_exp ~ div, data)
summary(m)
