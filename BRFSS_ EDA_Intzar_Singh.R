## ----load-packages, message = FALSE-----------------------------------------------------------------------
library(ggplot2)
library(dplyr)


## ----load-data--------------------------------------------------------------------------------------------
load("/Users/Intzar/Downloads/brffs2013.rd")


## ----Filter Our Data for Relevant Info--------------------------------------------------------------------
q1_data <- brfss2013 %>%
  select(marital, children, menthlth)

q1_data <- q1_data %>%
  filter_at(vars(marital,children,menthlth), all_vars(!is.na(.)))



## ----Do a preliminary Wrangling to see Important Info-----------------------------------------------------
q1_data %>%
  group_by(marital) %>%
  summarise(ment_med = median(menthlth), ment_mean= mean(menthlth))


## ----Visualize all the distributions based on the Categorical Variable - Marital--------------------------
ggplot(data = q1_data) +
    geom_bar(aes(x = menthlth), colour = "black", fill = "blue") +
    xlab("Days of Poor Mental Health")+
    facet_wrap(~ marital)
 


## ----Create a Box Plot to Visualize some of our Intuitions about the Data Spread and Difference-----------
ggplot(data = q1_data, aes(x= marital, y= menthlth)) +
  xlab("Marital Status")+
  ylab("Days of Poor Mental health")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2))+
  geom_boxplot(color = "steelblue")


## ---------------------------------------------------------------------------------------------------------
q1_data <- q1_data %>%
  mutate(cat_child = ifelse(children<1,"No Childen",ifelse(children<=3, "1-3 Children","More than 3 Children")))


## ----Visualize the Relationship between Children and Mental Health with Points----------------------------
ggplot(data = q1_data, aes( x= marital, y= menthlth, fill = cat_child))+
  geom_boxplot()+
  xlab("Marital Status")+
  ylab("Days of Poor Mental Health")+
  guides(fill = guide_legend("Children"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2))


## ----Create a table to see if we can get some Insights from the descriptive statistics--------------------
q1_data %>%
  group_by(cat_child,marital) %>%
  summarise(ment_med = median(menthlth), ment_mean= mean(menthlth), count = n())


## ----fig.height=5, fig.width=4----------------------------------------------------------------------------
ggplot(q1_data)+
  geom_bar(aes(x=menthlth), fill = "steelblue")+
  facet_wrap( ~ cat_child, ncol=1)+
  xlab("Days of Poor Mental Health")


## ---------------------------------------------------------------------------------------------------------
q2_data <- brfss2013 %>%
  select(X_incomg,renthom1,X_educag) %>%
  filter_at(vars(X_incomg,renthom1,X_educag),all_vars(!is.na(.)))
 
q2_data %>% 
  group_by(X_incomg) %>%
  summarise( count = n())

q2_data %>% 
  group_by(X_educag) %>%
  summarise( count = n())

q2_data %>% 
  group_by(renthom1) %>%
  summarise( count = n())


## ---------------------------------------------------------------------------------------------------------
ggplot(data = q2_data, aes(x= X_incomg))+
  geom_bar(fill = "steelblue")+
  xlab("Income Bracket")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2))
  
ggplot(data = q2_data, aes(x= renthom1))+
  geom_bar(fill = "steelblue")+
  xlab("Home Owwnership")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2))

ggplot(data = q2_data, aes(x= X_educag))+
  geom_bar(fill = "steelblue")+
  xlab("Education Level")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2))
  


## ----fig.height=5,fig.width=5-----------------------------------------------------------------------------
q2_data %>%
  group_by(X_educag, X_incomg) %>%
  summarise(count = n())


## ----fig.height=5,fig.width=5-----------------------------------------------------------------------------
ggplot(data=q2_data)+
  geom_bar(aes(x= X_incomg), fill ="steelblue")+
  facet_wrap(~ X_educag, ncol = 1)+
  xlab("Income Level")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2))


## ----fig.height=5,fig.width=5-----------------------------------------------------------------------------
ggplot(data=q2_data)+
  geom_bar(aes(x= renthom1), fill ="steelblue")+
  facet_wrap(~ X_incomg, ncol =1)+
  xlab("Income Level")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2))

## ----fig.height=5,fig.width=5-----------------------------------------------------------------------------
ggplot(data=q2_data)+
  geom_bar(aes(x= renthom1), fill ="steelblue")+
  facet_wrap(~ X_educag, ncol = 1)+
  xlab("Income Level")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2))


## ----fig.height=5,fig.width=5-----------------------------------------------------------------------------
q2_data %>%
  group_by(X_educag,renthom1) %>%
  summarise(count = n())
q2_data %>%
  group_by(X_incomg,renthom1) %>%
  summarise(count = n())


## ----fig.height=5,fig.width=8-----------------------------------------------------------------------------
ggplot(data=q2_data)+
  geom_bar(aes(x= renthom1, fill = X_incomg))+
  facet_wrap(~ X_educag ,ncol = 1)+
  xlab("Income Level")+
  guides(fill = guide_legend("Income Level"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2))


## ---------------------------------------------------------------------------------------------------------
q3_data <- brfss2013 %>%
  select(seatbelt,drvisits,flushot6) %>%
  filter_at(vars(seatbelt,drvisits,flushot6),all_vars(!is.na(.)))
q3_data <- q3_data %>%
  filter(seatbelt != "Never drive or ride in a car")


## ---------------------------------------------------------------------------------------------------------
ggplot(data = q3_data)+
  geom_bar(aes(x= seatbelt), fill = "steelblue")+
  xlab("Seatbelt worn while Driving")
ggplot(data = q3_data)+
  geom_bar(aes(x= flushot6), fill = "coral")+
  xlab("Recieved Flushot")
ggplot(data = q3_data)+
  geom_bar(aes(x= drvisits), fill = "seagreen")+
  xlab("# of Doctor Visits")
  


## ---------------------------------------------------------------------------------------------------------
ggplot(data = q3_data)+
  geom_bar(aes(x= seatbelt), fill = "coral2")+
  facet_wrap(~ flushot6, ncol = 1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2))+
  xlab("Seatbelt worn while Driving")


## ---------------------------------------------------------------------------------------------------------
ggplot(data = q3_data)+
  geom_bar(aes(x= drvisits), fill = "coral2")+
  facet_wrap(~ flushot6, ncol = 1)+
  xlab("# of Doctor Visits")
  


## ---------------------------------------------------------------------------------------------------------
q3_data%>%
  group_by(flushot6) %>%
  summarise( meanDR= mean(drvisits), medDR = median(drvisits))


## ---------------------------------------------------------------------------------------------------------
ggplot(data = q3_data, aes(x= seatbelt, y= drvisits, fill = flushot6)) + 
  geom_boxplot()+
  xlab("Seatbelt worn while Driving")+
  ylab("Doctor Visits")+
  guides(fill = guide_legend("Flushot Recieved"))

