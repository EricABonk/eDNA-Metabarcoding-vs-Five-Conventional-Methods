# Simulated data
library(readxl)
data<-read_excel("Data/Simulated.xlsx")
library(tidyverse)
eDNA<- data%>% filter(Method %in% c( "eDNA"))
Trad<- data%>% filter(Method %in% c( "EF"))
eDNA$Value<-1
Trad$Value<-1
eDNA$Method<-"eDNA-EF"
Trad$Method<-"eDNA-EF"

eDNA <-eDNA[eDNA $Value >= 1, ]
eDNA$Value[eDNA $Value>1]<-1
Trad<-Trad[Trad$Value >= 1, ]
Trad$Value[Trad$Value>1]<-1
unique(Trad$Value)
intersect(eDNA,Trad)
symdiff(eDNA,Trad)

Accuracy_Consistency(eDNA,Trad,"Method")
Accuracy_Consistency(Trad,eDNA,"Method")
by_method<-Accuracy_Consistency(eDNA, Trad, "Method")





# Diveristy 
v_EF<-Trad%>%
  group_by(Location) %>%
  summarise(Count= length(unique(Species)))

v_eDNA<-eDNA%>%
  group_by(Location) %>%
  summarise(Count= length(unique(Species)))
shapiro.test(v$Count)

t.test(v_EF$Count, v_eDNA$Count, paired = TRUE)

v_EF$Method<-"EF"
v_eDNA$Method<-"eDNA"
c<-combine(v_EF,v_eDNA)

ggplot(c, aes(x=factor(Method),Count))+
  geom_bar(stat="summary")+
  stat_summary(fun.data = mean_se,  
               geom = "errorbar") +
  labs(y="Diversity", x="Method")+
  theme_classic()+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(size = 6,face = "bold"))+
  guides(colour = guide_legend(title = "Method",ncol = 1))+
  theme(axis.text.x = element_text(size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))




#uniquness
data$Value<-1
data$`Total Number of Methods used`<-2
filtered_data <- data %>%
  group_by(Species, Location) %>%
  filter(n_distinct(Method) == 1)

filtered_data_mean<-filtered_data  %>%
  group_by(Method) %>%
  summarise(Count= sum(Value), 
            Average_Number_methods_used= mean(`Total Number of Methods used`))

filtered_data_mean_old<-filtered_data_mean
filtered_data_mean$Count*filtered_data_mean$Average_Number_methods_used
#similary ----

length(intersect(unique(eDNA$Species), unique(Trad$Species)))
length(symdiff(unique(eDNA$Species), unique(Trad$Species)))
data
(7/7)/6


# RV
data<-read_excel("Data/Simulated.xlsx")
library(tidyverse)
eDNA<- data%>% filter(Method %in% c( "eDNA"))
Trad<- data%>% filter(Method %in% c( "EF"))
eDNA$Method<-"eDNA-EF"
Trad$Method<-"eDNA-EF"

methods <- c("eDNA-EF")

result <- Arrange_And_Conduct_RV_Coefficient(eDNA, Trad, methods, "Method", "Species", "Location", "Value")
result



