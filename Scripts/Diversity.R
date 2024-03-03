# Diveristy 
library(tidyverse)
library(readxl)
library(plotrix)
S12<-read_excel("Data/12S Data.xlsx")
Trad<-read_excel("Data/Traditional Data.xlsx")
COI<-read_excel("Data/COI Data.xlsx")

S12$Method<-"12S"
COI$Method<-"COI"

Trad$variable<-Trad$Date_site
Trad$value<-Trad$Count
Trad<-subset(Trad, select=-c(Date_site,Count))
Trad$Cat<-"Conventional"
S12$Cat<-"eDNA"
COI$Cat<-"eDNA"
data1<-rbind(S12,COI, Trad)

data1 <- data1[data1$value >= 1, ]#
data1$value<-ifelse(data1$value>1,1,data1$value)

p1<-ggplot(data1,aes(variable, Species, group=Method, color =Method)) + 
  geom_point(aes(fill=Method), position=position_dodge(width=0.5),colour="black", shape=21,size=3,stroke=1)+
  labs(y="Species", x="Sample")+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(size = 6,face = "bold"))+
  guides(colour = guide_legend(title = "Method",ncol = 1))+
  theme(axis.text.x = element_text(size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))

# Plot 2----
S12<-read_excel("Data/12S Data.xlsx")
Trad<-read_excel("Data/Traditional Data.xlsx")
COI<-read_excel("Data/COI Data.xlsx")

S12$Method<-"12S"
COI$Method<-"COI"

Trad$variable<-Trad$Date_site
Trad$value<-Trad$Count
Trad<-subset(Trad, select=-c(Date_site,Count))
Trad$Cat<-"Conventional"
S12$Cat<-"eDNA"
COI$Cat<-"eDNA"
data1<-rbind(S12,COI, Trad)

data1 <- data1[data1$value >= 1, ]#
data1$value<-ifelse(data1$value>1,1,data1$value)
f<-data1 %>%
  group_by(variable,Method) %>%
  summarise(Count= sum(value))
f$Sample<-f$variable
# We need to account for the days the sampling emthod was used but no fish were found 
sampling<-read_excel("Data/Detour Sampling.xlsx")
sampling$combined <- paste(sampling$Method, "-", sampling$Sample)
f$combined <- paste(f$Method, "-", f$Sample)
diff<-symdiff(f$combined,sampling$combined)

values_to_add <- c(diff)
# Values from symdiff
values_to_add <- c("HN - 08-07-23 - SunC-REF", 
                   "HN - 09-07-23 - SunC-REF",
                   "MT - 09-07-23 - SunC-REF",
                   "MT - 08-07-23 - SunC-EXP",
                   "MT - 11-07-23 - KC-REF",
                   "EF - 12-07-23 - SunC-FF")

# Extract Method and Sample
methods <-c("HN","HN","MT","MT","MT","EF")

samples <-c("08-07-23 - SunC-REF", 
            "09-07-23 - SunC-REF",
            "09-07-23 - SunC-REF",
            "08-07-23 - SunC-EXP",
            "11-07-23 - KC-REF",
            "12-07-23 - SunC-FF")

# Create new rows with Method and Sample
new_rows <- data.frame(variable = values_to_add,
                       Method = methods,
                       Count = 0,
                       Sample = samples,
                       combined = values_to_add)

# Append new rows to dataframe f
f <- rbind(f, new_rows)





p2<-ggplot(f, aes(x=factor(Method),Count))+
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


shapiro.test(f$Count) # Fail

pairwise.wilcox.test(f$Count,f$Method)

t<-f%>%
  group_by(Method) %>%
  summarise(mean= mean(Count), error= std.error(Count))
t







library(plotrix)
#### Plot 3----



S12<-read_excel("Data/12S Data.xlsx")
Trad<-read_excel("Data/Traditional Data.xlsx")
COI<-read_excel("Data/COI Data.xlsx")

S12$Method<-"12S"
COI$Method<-"COI"

Trad$variable<-Trad$Date_site
Trad$value<-Trad$Count
Trad<-subset(Trad, select=-c(Date_site,Count))
Trad$Cat<-"Conventional"
S12$Cat<-"eDNA"
COI$Cat<-"eDNA"
data1<-rbind(S12,COI)
data1$value<-ifelse(data1$value>1,1,data1$value)
data1<-data1[!duplicated(data1), ]
Trad$value<-ifelse(Trad$value>1,1,Trad$value)
Trad1<-Trad1[!duplicated(Trad1), ]

data1<-rbind(data1,Trad)
data1 <- data1[data1$value >= 1, ]#
data1$Value<-ifelse(data1$value>1,1,data1$value)
f<-data1 %>%
  group_by(Species,variable,Cat) %>%
  summarise(Count= sum(value))
f$Count<-ifelse(f$Count>1,1,f$Count)
f<-f%>%
  group_by(variable,Cat) %>%
  summarise(Count= sum(Count))

p3<-ggplot(f, aes(x=factor(Cat),Count))+
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



library(ggpubr)
p2_p3<-ggarrange(p3,p2, labels = c("B", "C"), font.label = list(size=10))
ggarrange(p1,p2_p3, labels = c("A"), ncol=1,font.label = list(size=10))

library(tidyverse)
library(plotrix)
t<-f%>%
  group_by(Cat) %>%
  summarise(mean= mean(Count), error= std.error(Count))
shapiro.test(f$Count) # Pass 
library(car)
leveneTest(f$Count,f$Cat) # Pass

a<-aov(f$Count~f$Cat)
summary(a)
TukeyHSD(a)
# Is there a way to only do species detected with that method ----
S12<-read_excel("Data/12S Data.xlsx")
Trad<-read_excel("Data/Traditional Data.xlsx")
COI<-read_excel("Data/COI Data.xlsx")

S12$Method<-"12S"
COI$Method<-"COI"

Trad$variable<-Trad$Date_site
Trad$value<-Trad$Count
Trad<-subset(Trad, select=-c(Date_site,Count))
Trad$Cat<-"Conventional"
S12$Cat<-"eDNA"
COI$Cat<-"eDNA"
data1<-rbind(S12,COI, Trad)

data1 <- data1[data1$value >= 1, ]#
data1$value<-ifelse(data1$value>1,1,data1$value)
f<-data1 %>%
  group_by(variable,Method, Species) %>%
  summarise(Count= sum(value))
f$Sample<-f$variable
f$Combo<-NA

f$Combo[f$variable=="08-07-23 - SunC-REF"] <- "EF,GN,HN"
f$Combo[f$variable=="09-07-23 - SunC-REF"] <- "GN,HN,MT"
f$Combo[f$variable=="08-07-23 - SunC-EXP"] <- "EF,GN,HN,MT"
f$Combo[f$variable=="10-07-23 - KC-EXP"] <- "EF,MT"
f$Combo[f$variable=="11-07-23 - KC-REF"] <- "EF,MT, SN"
f$Combo[f$variable=="12-07-23 - SunC-FF"] <- "EF,GN"
f$Combo[f$variable=="09-07-23 - SunC-EXP"] <- "GN,HN,SN"
f$Combo[f$variable=="12-07-23 - RJ-POND"] <- "GN,MT"
f$Combo[f$variable=="12-07-23 - HC-REF"] <- "EF,GN,HN"
f$Combo[f$variable=="13-07-23 - HC-REF"] <- "GN,HN"



f <- f %>%
  group_by(variable) %>%
  mutate(`Total Number of Methods used` = length(unique(unlist(strsplit(Combo, ",")))))





filtered_data <- f %>%
  group_by(Species, variable) %>%
  filter(n_distinct(Method) == 1)

filtered_data_mean<-filtered_data  %>%
  group_by(Method) %>%
  summarise(Count= sum(Count), 
            Average_Number_methods_used= mean(`Total Number of Methods used`))
  
  
  
  
  # The number of occurrences when a single method was the only method to detect that species
# Number of times a method was the only method to detect a species during a sample*Average number of methods used 
# Unique species detected/ Average number of methods used 
  
Species_Only_detcted_with_one_method<-ggplot(filtered_data_mean, aes(x=factor(Method),Count*Average_Number_methods_used))+
  geom_bar(stat="summary")+
  stat_summary(fun.data = mean_se,  
               geom = "errorbar") +
  labs(y="Weighted Uniqueness", x="Method")+
  theme_classic()+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(size = 6,face = "bold"))+
  guides(colour = guide_legend(title = "Method",ncol = 1))+
  theme(axis.text.x = element_text(size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))
Species_Only_detcted_with_one_method

filtered_data_mean$Count*filtered_data_mean$Average_Number_methods_used


p2_p3<-ggarrange(p3,p2,Species_Only_detcted_with_one_method,nrow=1, labels = c("B", "C","D"), font.label = list(size=10))
ggarrange(p1,p2_p3, labels = c("A"), ncol=1,font.label = list(size=10))

# Plot 4----
S12<-read_excel("Data/12S Data.xlsx")
Trad<-read_excel("Data/Traditional Data.xlsx")
COI<-read_excel("Data/COI Data.xlsx")

S12$Method<-"12S"
COI$Method<-"COI"

Trad$variable<-Trad$Date_site
Trad$value<-Trad$Count
Trad<-subset(Trad, select=-c(Date_site,Count))
Trad$Cat<-"Conventional"
S12$Cat<-"eDNA"
COI$Cat<-"eDNA"
data1<-rbind(S12,COI, Trad)

data1 <- data1[data1$value >= 1, ]#
data1$value<-ifelse(data1$value>1,1,data1$value)
f<-data1 %>%
  group_by(variable,Method, Species) %>%
  summarise(Count= sum(value))

f$Sample<-f$variable
# We do not need to account for the days the sampling emthod was used but no fish were found becuase at every sample atleast one fish was captured from a combination prespective 
f$variable[f$variable=="08-07-23 - SunC-REF"] <- "EF,GN,HN"
f$variable[f$variable=="09-07-23 - SunC-REF"] <- "GN,HN,MT"
f$variable[f$variable=="08-07-23 - SunC-EXP"] <- "EF,GN,HN,MT"
f$variable[f$variable=="10-07-23 - KC-EXP"] <- "EF,MT"
f$variable[f$variable=="11-07-23 - KC-REF"] <- "EF,MT, SN"
f$variable[f$variable=="12-07-23 - SunC-FF"] <- "EF,GN"
f$variable[f$variable=="09-07-23 - SunC-EXP"] <- "GN,HN,SN"
f$variable[f$variable=="12-07-23 - RJ-POND"] <- "GN,MT"
f$variable[f$variable=="12-07-23 - HC-REF"] <- "EF,GN,HN"
f$variable[f$variable=="13-07-23 - HC-REF"] <- "GN,HN"
f<-f[!duplicated(f), ]

f<-f %>%
  group_by(variable, Sample) %>%
  summarise(Count= sum(Count))

p4<-ggplot(f, aes(x=factor(variable),Count))+
  geom_bar(stat="summary")+
  stat_summary(fun.data = mean_se,  
               geom = "errorbar") +
  labs(y="Diversity", x="Sample Technique")+
  theme_classic()+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(size = 6,face = "bold"))+
  guides(colour = guide_legend(title = "Method",ncol = 1))+
  theme(axis.text.x = element_text(size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))
p4

