#---Some missing lines here, will soon be updated
#---------preliminary----------------
head(hh)
head(hhmembers)
head(women)
head(men)

sum(!men$LN==men$MWM3)  #Checking if man's line number is same as line number

"LN"%in%colnames(women)  #Checking if line number is in women

#selecting necessary information about household members
tab.members <- hhmembers %>% 
  select(HH1,HH2,LN=HL1,HL4,HL6,)

#selecting necessary information about household
tab1 <- hh %>% 
  select(HH1,HH2,HC1A,windex5)

#selecting necessary information about women
tabw <- women %>% 
  select(HH1,HH2,LN,MA1,SB1,MA6,LS1)

#selecting necessary information about men
tabm <- men %>% 
  select(HH1,HH2,LN,MWAGEM,MMA1,MSB1,MMA6,MLS1)

#----------cleaning data -----------
#creating a single table with all variables needed for analysis
t1 <- left_join(tab.members,tab1,join_by(HH1,HH2))

t2 <- full_join(tabm,tabw,join_by(HH1,HH2,LN))
  
t3 <- left_join(t1,t2,join_by(HH1,HH2,LN))

colnames(t3)  #cross-checking names
colnames(t3) <- c("Cluster_no.","Household_no.","person_ID","gender","age","Religion_of_head","Wealth_quintiles"
                  ,"MWAGEM","Married/Not.m","age_at_first_sex.m","ms.m","ls.m","Married/Not.w","age_at_first_sex.w",
                 "ms.w","ls.w")

write_csv(t3,"zim_data.csv")  #csv of the dataset
#---------- research question 1 -------------
study.1 <- t3 %>% 
  select(ls.m,ls.w,Wealth_quintiles) %>% 
  mutate(life_satisfaction=coalesce(ls.m,ls.w)) %>% 
  select(-ls.m,-ls.w)

#checking for missing values
sum(is.na(study.1$Wealth_quintiles))
sum(is.na(study.1$life_satisfaction))

cleaned.study.1 <- study.1[-which(is.na(study.1$life_satisfaction)), ]
str(cleaned.study.1)
sum(cleaned.study.1[["life_satisfaction"]]==9)  # sum of no responses

cleaned.study.1 <- cleaned.study.1 %>% #removing the no responses. This is to adjust for Fisher's theorem
  filter(!life_satisfaction==9)

#tabulating frequencies
tb1 <- table(cleaned.study.1$life_satisfaction,cleaned.study.1$Wealth_quintiles)

prop.table(tb1)

#Chi-squared test and CrammerV test
chisq.test(tb1)

cramerV(tb1)

#Visualisation  of findings
p <- cleaned.study.1 %>% 
  group_by(life_satisfaction,Wealth_quintiles) %>% 
  summarise(n=n()) %>% 
  mutate(percentage=n/sum(n))

p$life_satisfaction <- factor(p$life_satisfaction,labels=c("VERY HAPPY","SOMEWHAT HAPPY","NEITHER HAPPY NOR UNHAPPY","SOMEWHAT UNHAPPY","VERY UNHAPPY"),
         levels=c(1,2,3,4,5))

plot1 <- ggplot(p,aes(x=Wealth_quintiles,y=percentage))+
  geom_line(aes(colour = as.factor(life_satisfaction)))+
  theme_minimal()+
  labs(title = "A graph showing how happiness varies with wealth quintiles",
       caption = ("1=Poorest   2=Second   3=Middle  4=Fourth   5=Richest"),
       colour = "Happiness scale")+
  scale_y_continuous(labels = scales::percent)

ggplotly(plot1)
