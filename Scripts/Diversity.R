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

# Make first plot showing presence 
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
data1

v<-data1 %>%
  group_by(Method) %>%
  summarise(Count= length(unique(Species)))

v<-data1 %>%
  group_by(Method) %>%
  summarise(Species=list(unique(Species)))







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
# We need to account for the days the sampling method was used but no fish were found 
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
library(lmerTest)
model <- lmer(Count ~ as.factor(Method) + (1|variable), data = f)
summary(model)

resid_values <- residuals(model)  # raw residuals # passes assumption of normally distributed errors
shapiro.test(resid_values) # Pass again

qqnorm(residuals(model))
qqline(resid_values, col="red")
plot(ranef(model))
summary(model)
AIC(model)

plot(fitted(model), resid_values,
     main="Residuals vs Fitted",
     xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red") #No clear pattern -> Homoscedasticity assumption passed 



boxplot(resid_values ~ f$Method,
        main="Residuals by Method",
        xlab="Method", ylab="Residuals")
abline(h=0, col="red") # Pass linearity assumption 


ranef_values <- ranef(model)$variable
qqnorm(ranef_values$`(Intercept)`); qqline(ranef_values$`(Intercept)`, col="red")
hist(ranef_values$`(Intercept)`)
shapiro.test(ranef_values$`(Intercept)`) # passed Normality of random effects


library(emmeans)
citation("emmeans")
pairwise_comparisons <- emmeans(model, pairwise ~ Method,adjust = "BH")
pairwise_comparisons


# Fit a Poisson GLMM (for count data) just out of curiosity 
model <- glmer(Count ~ as.factor(Method) + (1|variable),
               data = f,
               family = poisson(link = "log"))
AIC(model)

# Post-hoc pairwise comparisons for Method
emmeans_model <- emmeans(model, ~ Method,adjust = "BH")
pairs(emmeans_model)

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


3.1/ 2 

p2


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

s<-data1 %>%
  group_by(Cat) %>%
  summarise(species= length(species))

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
Con<-f%>% filter(Cat == "Conventional")
eD<-f%>% filter(Cat == "eDNA")
eD$eDNA<-eD$Count
Con$Conventional<-Con$Count
w<-data.frame(Conventional=Con$Conventional,eDNA=eD$eDNA)
t.test(w$Conventional,w$eDNA, paired = TRUE)

# Weighted Uniqueness ----
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

f$Combo[f$variable=="08-07-23 - SunC-REF"] <- "12S,COI,EF,GN,HN"
f$Combo[f$variable=="09-07-23 - SunC-REF"] <- "12S,COI,GN,HN,MT"
f$Combo[f$variable=="08-07-23 - SunC-EXP"] <- "12S,COI,EF,GN,HN,MT"
f$Combo[f$variable=="10-07-23 - KC-EXP"] <- "12S,COI,EF,MT"
f$Combo[f$variable=="11-07-23 - KC-REF"] <- "12S,COI,EF,MT, SN"
f$Combo[f$variable=="12-07-23 - SunC-FF"] <- "12S,COI,EF,GN"
f$Combo[f$variable=="09-07-23 - SunC-EXP"] <- "12S,COI,GN,HN,SN"
f$Combo[f$variable=="12-07-23 - RJ-POND"] <- "12S,COI,GN,MT"
f$Combo[f$variable=="12-07-23 - HC-REF"] <- "12S,COI,EF,GN,HN"
f$Combo[f$variable=="13-07-23 - HC-REF"] <- "12S,COI,GN,HN"



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
  
filtered_data_mean_old<-filtered_data_mean
  
  
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
# We do not need to account for the days the sampling method was used but no fish were found because at every sample at least one fish was captured from a combination perspective 
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




# p5----
S12<-read_excel("Data/12S Data.xlsx")
Trad<-read_excel("Data/Traditional Data.xlsx")
Trad$value<-Trad$Count
Trad$variable<-Trad$Date_site
Trad<-subset(Trad, select=-c(Date_site,Count))
COI<-read_excel("Data/COI Data.xlsx")
S12$Method<-"12S"
COI$Method<-"COI"

Trad$Cat<-"Conventional"
S12$Cat<-"eDNA"
COI$Cat<-"eDNA"
x<-combine(S12,COI)
x<-combine(x,Trad)

f<-x %>%
  group_by(Cat) %>%
  summarise(Count= (unique(Species)))
f_edna<-f%>% filter(Cat== "eDNA") # total 15
f_trad<-f%>% filter(Cat== "Conventional") # TOTAL OF 10 on 7 in common, eDNA missed 3 species, eDNA detected 15-7, 8 species not detected with conventional 
intersect(f_edna$Count,f_trad$Count) #7

f<-x %>%
  group_by(Method, Species) %>%
  summarise(Count= sum(value))




f<- f[f$Count >= 1, ]#
f$Count<-ifelse(f$Count>1,1,f$Count)


S12<-f%>% filter(Method == "12S")
COI<-f%>% filter(Method == "COI")
EF<-f%>% filter(Method == "EF")
COI<-subset(COI, select=-c(Method))
S12<-subset(S12, select=-c(Method))
EF<-subset(EF, select=-c(Method))
all_all_not(S12,COI)
all_all_not(S12,EF)
all_all_not(EF,COI)
dplyr::intersect(S12,COI)

library(dplyr)
library(tidyverse)


#Plot 5-----
# Create function for Similarity  ----
# Function needs to be run line by line
# Initialize results data frame
results <- data.frame(
  Method1 = character(),
  Method2 = character(),
  CommonSpecies = integer(),
  UniqueSpecies = integer(),
  stringsAsFactors = FALSE
)

# Compare each method with every other method
for (i in 1:(length(methods) - 1)) {
  for (j in (i + 1):length(methods)) {
    method1 <- methods[i]
    method2 <- methods[j]
    
    df1 <- f %>% filter(Method == method1) %>% dplyr::select(Species)
    df2 <- f %>% filter(Method == method2) %>% dplyr::select(Species)
    
    common <- df1 %>% inner_join(df2, by = "Species") %>% nrow()
    unique <- df1 %>% anti_join(df2, by = "Species") %>% bind_rows(df2 %>% anti_join(df1, by = "Species")) %>% nrow()
    
    results <- rbind(results, data.frame(
      Method1 = method1,
      Method2 = method2,
      CommonSpecies = common,
      UniqueSpecies = unique
    ))
  }
}

# Print results
results_fish<-print(results)

sampling<-read_excel("Data/Detour Sampling.xlsx")

# Initialize results data frame
results <- data.frame(
  Method1 = character(),
  Method2 = character(),
  CommonSample = integer(),
  UniqueSample = integer(),
  stringsAsFactors = FALSE
)

# Get unique methods
methods <- unique(sampling$Method)

# Compare each method with every other method
for (i in 1:(length(methods) - 1)) {
  for (j in (i + 1):length(methods)) {
    method1 <- methods[i]
    method2 <- methods[j]
    
    # Filter data frames for each method
    df1 <- sampling %>% filter(Method == method1) %>% dplyr::select(Sample)
    df2 <- sampling %>% filter(Method == method2) %>% dplyr::select(Sample)
    
    # Calculate common and unique samples
    common <- df1 %>% inner_join(df2, by = "Sample") %>% nrow()
    unique <- df1 %>% anti_join(df2, by = "Sample") %>% bind_rows(df2 %>% anti_join(df1, by = "Sample")) %>% nrow()
    
    # Append results to the results data frame
    results <- rbind(results, data.frame(
      Method1 = method1,
      Method2 = method2,
      CommonSample = common,
      UniqueSample = unique
    ))
  }
}

# Print the results
results_sample<-print(results)

results<-merge(results_sample, results_fish)

results$Method<-paste(results$Method1,"-",results$Method2)
results$all_fish<-results$CommonSpecies+results$UniqueSpecies
results$sim<-((results$CommonSpecies/results$all)/(results$CommonSample+results$UniqueSample))

mean(results$sim)


p5<-ggplot(results, aes(x=factor(Method),sim))+
  geom_bar(stat="summary")+
  stat_summary(fun.data = mean_se,  
               geom = "errorbar") +
  labs(y="Similarity", x="Method")+
  theme_classic()+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(size = 6,face = "bold"))+
  guides(colour = guide_legend(title = "Method",ncol = 1))+
  theme(axis.text.x = element_text(angle=45,vjust=.5, size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))
  
p5



v<-results%>%
  group_by(Method) %>%
  summarise(mean= mean(sim))
# summary stats
v1<-results%>% filter(Method == "12S - COI")
v<-results%>% filter(!Method == "12S - COI")
S12<-v%>% filter(Method2 == "12S")
COI<-v%>% filter(Method2 == "COI")

mean(COI$sim,v1$sim)
c<-c(COI$sim,v1$sim)
mean(c)
c1<-c(S12$sim,v1$sim)
mean(c1)/mean(c)

#Final plots----
p2_p3<-ggarrange(p3,p2,nrow=1, labels = c("B", "C"), font.label = list(size=10))

p4_p5<-ggarrange(Species_Only_detcted_with_one_method,p5,nrow=1, labels = c("D", "E"), font.label = list(size=10))

p2_p3<-ggarrange(p2_p3,p4_p5,ncol=1, font.label = list(size=10))
ggarrange(p1,p2_p3, labels = c("A"), ncol=1,font.label = list(size=10))


# Summary stats for results
library(tidyverse) 
library(readxl)
library(plotrix)
S12<-read_excel("Data/12S Data.xlsx")
Trad<-read_excel("Data/Traditional Data.xlsx")
COI<-read_excel("Data/COI Data.xlsx")
data1 <-S12
data1 <- data1[data1$value >= 1, ]#

v<-data1 %>%
  group_by(variable) %>%
  summarise(max=max(value),
            min=min(value))

mean(v$max)
mean(v$min)
mean(data1$value)


data1 <-COI
data1 <- data1[data1$value >= 1, ]#

v<-data1 %>%
  group_by(variable) %>%
  summarise(max=max(value),
            min=min(value))

mean(v$max)

mean(v$min)
mean(data1$value)
