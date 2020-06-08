  library(tidyverse)
  library(dplyr)
  require(forcats)
  
  bg <- read.csv("/home/antony/Desktop/R Tutorial/board_games.csv")  
  
  
View(bg)


bg%>%
  count(publisher,sort = T)

bg%>%
  count(year_published)%>%
  arrange(desc(year_published))
  ggplot(aes(year_published,n))+
  geom_line()

  
  
bg%>%
  ggplot(aes(average_rating))+geom_histogram()




bg%>%
  ggplot(aes(users_rated))+geom_histogram()+scale_x_log10()




bg%>%
  filter(users_rated>=500)%>%
  ggplot(aes(average_rating))+geom_histogram()
  




bg%>%
  filter(max_playtime>5,max_playtime<1000)%>%
  ggplot(aes(max_playtime/60))+geom_histogram(binwidth = 0.2)+scale_x_log10(breaks=2^seq(-2,4))




bg%>%
  count(name,sort = T)


list <- bg%>%
  select(game_id,name,family,expansion,category)


View(list)


list2 <- bg%>%
  select(game_id,name,family,expansion,category)%>%
  gather(type,value,family,expansion,category)%>%
  filter(!is.na(value))%>%
  separate_rows(value,sep = ",")%>%
  arrange(game_id)


View(list2)


categorical_variable <-  bg%>%
  select(game_id,name,family,category,artist,designer)%>%
  gather(type,value,-game_id,-name)%>%
  filter(!is.na(value))%>%
  separate_rows(value,sep = ",")%>%
  arrange(game_id)





View(categorical_variable)

categorical_variable%>%
  count(type,value,sort = T)

categorical_variable_list <- categorical_variable%>%
  count(type,value,sort = T)


categorical_variable_list%>%
  filter(type=="category")%>%
  head(20)%>%
  mutate(value=fct_reorder(value,n))%>%
  ggplot(aes(value,n))+geom_col()+coord_flip()+
  labs(title = "Most Comon Category")





categorical_variable_list%>%
  group_by(type)%>%
  top_n(20,n)%>%
  ungroup()%>%
  mutate(value=fct_reorder(value,n),type=fct_reorder(type,n,.desc=T))%>%
  ggplot(aes(value,n,fill=type))+
  geom_col(show.legend = F)+
  facet_wrap(~ type,scales = "free_y")+
    coord_flip()+
  labs(title = "Most Comon Category")
  

bg%>%
  ggplot(aes(max_players,average_rating))+geom_point()+scale_x_log10()+geom_smooth(method = "lm")


  
# to the see the trend in thee last 10 decades 
bg%>%
  group_by(decade=10*(year_published%/%10))%>%
  summarize(average_rating=mean(average_rating))%>%
  ggplot(aes(decade,average_rating))+geom_line()



## predict the average rating
#every time we double the number of players, expect the ave rating to do dowwn by 0.18
library(broom)
  lm(average_rating ~ log2(bg$max_players+1)+
       log2(bg$max_playtime+1)+
       bg$year_published,bg)%>%
    summary()%>%
    tidy()




by_catagorical <- bg%>%
  inner_join(categorical_variable,by=c("game_id","name"))%>%
  select(type,value,average_rating)%>%
  group_by(type,value)%>%
  summarize(games=n(),average_rating=mean(average_rating))%>%
  arrange(desc(games))



bg%>%
  inner_join(categorical_variable,by=c("game_id","name"))%>%
  filter(type=="category")%>%
    mutate(value=fct_lump(value,15),value=fct_reorder(value,average_rating))%>%
  ggplot(aes(value,average_rating))+geom_boxplot()+coord_flip()





bg%>%
  inner_join(categorical_variable,by=c("game_id","name"))%>%
  filter(type=="family")%>%
  mutate(value=fct_lump(value,15),value=fct_reorder(value,average_rating))%>%
  ggplot(aes(value,average_rating))+geom_boxplot()+coord_flip()





bg%>%
  inner_join(categorical_variable,by=c("game_id","name"))%>%
  filter(type=="designer")%>%
  mutate(value=fct_lump(value,15),value=fct_reorder(value,average_rating))%>%
  ggplot(aes(value,average_rating))+geom_boxplot()+coord_flip()





bg%>%
  inner_join(categorical_variable,by=c("game_id","name"))%>%
  filter(type=="artist")%>%
  mutate(value=fct_lump(value,15),value=fct_reorder(value,average_rating))%>%
  ggplot(aes(value,average_rating))+geom_boxplot()+coord_flip()



features <- categorical_variable%>%
  unite(feature,type,value)%>%
  add_count(feature)%>%
  filter(n>=50)
  

feature

library(glmnet)
library(tidytext)
library(Matrix)

#predictor 
feature_matrix <- features%>%
  cast_sparse(game_id,feature)
  
  
dim(feature_matrix)
View(as.matrix(feature_matrix))
rownames(feature_matrix)



d <- match(rownames(feature_matrix),bg$game_id)
View(d)
#output
rating <- bg$average_rating[match(rownames(feature_matrix),bg$game_id)]




rating
View(rating)

lasso_fit <- glmnet(feature_matrix,rating)
lasso_fit
plot(lasso_fit)
lasso_fit%>%
  tidy()%>%
  filter(step==30)%>%
  arrange((estimate))





cv_lasso_fit <- cv.glmnet(feature_matrix,rating)
plot(cv_lasso_fit)

#perfect lamda value
cv_lasso_fit$lambda.1se


cv_lasso_fit$glmnet.fit%>%
  tidy()%>%
  filter(lambda==cv_lasso_fit$lambda.1se)%>%
  arrange(desc(estimate))




new_list_of_features <- bg%>%
  transmute(game_id,name,year=year_published-1950,log2_max_players=log2(max_players+1),log2_max_playtime=log2(max_playtime+1))%>%
  gather(feature,value,-game_id,-name)
View(new_list_of_features)





features <- categorical_variable%>%
  unite(feature,type,value,sep =" :")%>%
  add_count(feature)%>%
  filter(n>=50)%>%
  mutate(value=1)%>%
  bind_rows(new_list_of_features)



View(features)


library(stringr)
str_trim("     abs ff")


name <- c("250","7500")

str_pad(name,width = 5,side = "right",pad="0")
