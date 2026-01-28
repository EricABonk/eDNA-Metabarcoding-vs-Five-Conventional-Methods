# Accuracy_Consistency Function ----
# This function is from Bonk et al. (2026) 
# Github: https://github.com/EricABonk/eDNA-EF-Meta-Analysis
library(tidyverse)
all_all_not <- function(a, b) {  
  all <- intersect(a, b)
  all_not <- symdiff(a, b)
  # Create data frames with a single column for 'all' and 'all_not'
  all<- data.frame(Value = all)
  all_not <- data.frame(Value = all_not)
  # Print the lengths of the data frames
  all<-(nrow(all))
  all_not <-(nrow(all_not))
  data.frame(all=all, not=all_not)
}
Accuracy_Consistency<- function(a, b, column_name, conf.level = 0.95) {
  # Check if the specified column is empty in either data frame
  if (all(is.na(unique(a[[column_name]]))) || all(is.na(unique(b[[column_name]])))) {
    print("The specified column is empty in one or both data frames. Comparing entire data frames.")
    result <- all_all_not(a, b)
    consistency <- sum(result$all) / (sum(result$all) + sum(result$not))
    ci_consistency <- prop.test(sum(result$all), sum(result$all) + sum(result$not), conf.level = conf.level)$conf.int
    ci_lower <- ci_consistency[1]
    ci_upper <- ci_consistency[2]
    accuracy <- sum(result$all) / nrow(a)
    result$Consistency <- consistency
    result$Consistency_Lower <- ci_lower
    result$Consistency_Upper <- ci_upper
    result$Accuracy <- accuracy
    acc_ci <- prop.test(sum(result$all), nrow(a), conf.level = conf.level)$conf.int
    result$Accuracy_Lower <- acc_ci[1]
    result$Accuracy_Upper <- acc_ci[2]
    return(result)
  }
  
  # Get unique values of the specified column from both data frames
  values_a <- unique(a[[column_name]])
  values_b <- unique(b[[column_name]])
  
  # Initialize lists to store subsets
  subsets_a <- list()
  subsets_b <- list()
  
  # Create subsets for data frame 'a'
  for (value in values_a) {
    subset_a <- subset(a, a[[column_name]] == value)
    subsets_a[[as.character(value)]] <- subset_a
  }
  
  # Create subsets for data frame 'b'
  for (value in values_b) {
    subset_b <- subset(b, b[[column_name]] == value)
    subsets_b[[as.character(value)]] <- subset_b
  }
  
  # Initialize a list to store results
  results <- list()
  
  # Apply 'all_all_not' function to each pair of non-empty subsets
  for (value in names(subsets_a)) {
    if (!is.null(subsets_a[[value]]) && !is.null(subsets_b[[value]])) {
      print(paste("Processing value:", value))
      result <- all_all_not(subsets_a[[value]], subsets_b[[value]])
      consistency <- sum(result$all) / (sum(result$all) + sum(result$not))
      ci_consistency <- prop.test(sum(result$all), sum(result$all) + sum(result$not), conf.level = conf.level)$conf.int
      ci_lower_consistency <- ci_consistency[1]
      ci_upper_consistency <- ci_consistency[2]
      accuracy <- sum(result$all) / nrow(subsets_a[[value]])
      result$Consistency <- consistency
      result$Consistency_Lower <- ci_lower_consistency
      result$Consistency_Upper <- ci_upper_consistency
      result$Accuracy <- accuracy
      acc_ci <- prop.test(sum(result$all), nrow(subsets_a[[value]]), conf.level = conf.level)$conf.int
      result$Accuracy_Lower <- acc_ci[1]
      result$Accuracy_Upper <- acc_ci[2]
      results[[value]] <- result
    }
  }
  
  # Combine results into a single data frame
  if (length(results) > 0) {
    df <- do.call(rbind, results)
    df <- data.frame(Variable = row.names(df), df, row.names = NULL)
    return(df)
  } else {
    print("No non-empty subsets found.")
    return(NULL)
  }
}


# By method----
library(readxl)
eDNA<-read_excel("Data/eDNA.xlsx")
Trad<-read_excel("Data/Trad.xlsx")
eDNA<-combine(eDNA,eDNA_12S) #Bob wanted 12s-COI added Run stuff at bottom
Trad<-combine(Trad,Trad_COI) #Bob wanted 12s-COI added Run stuff at bottom

eDNA <-eDNA[eDNA $Value >= 1, ]
eDNA$Value[eDNA $Value>1]<-1
Trad<-Trad[Trad$Value >= 1, ]
Trad$Value[Trad$Value>1]<-1
unique(Trad$Value)

Accuracy_Consistency(eDNA,Trad,"Method")
by_method<-Accuracy_Consistency(eDNA, Trad, "Method")
Overall<-Accuracy_Consistency(eDNA, Trad,"")
Overall$Variable<-" All Methods"
by_method<-combine(by_method,by_method_12S_COI) #Gotta run the stuff at bottom and add it to data file since bob wanted 12S-COI comparision 
c<-combine(by_method, Overall)
c2<-c
mean_con<-mean(by_method$Consistency)

CON_Plot<-ggplot(c, aes(x = Consistency, y = Variable)) +
  geom_point(size = 1) +
  geom_errorbarh(aes(xmin = `Consistency_Lower`, xmax = `Consistency_Upper`), height = 0.2) +
  geom_vline(aes(xintercept = mean_con,  linetype = "Unweighted Mean"),color = "red")+
  labs(x = "Consistency", y = "Method") +
  theme_classic()+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(size = 6,face = "bold"))+
  guides(linetype = guide_legend(title = ""))+
  theme(axis.text.x = element_text(size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))

mean_acc<-mean(by_method$Accuracy)

ACC_plot<-ggplot(c, aes(x = Accuracy, y = Variable)) +
  geom_point(size = 1) +
  geom_errorbarh(aes(xmin = `Accuracy_Lower`, xmax = `Accuracy_Upper`), height = 0.2) +
  geom_vline(aes(xintercept = mean_acc, linetype = "Unweighted Mean"), color = "red")+
  labs(x = "Accuracy", y = "Method") +
  theme_classic()+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(size = 6,face = "bold"))+
  guides(linetype = guide_legend(title = ""))+
  theme(axis.text.x = element_text(size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))


by_method2<-Accuracy_Consistency(Trad,eDNA, "Method")
Overall2<-Accuracy_Consistency(Trad,eDNA,"")
Overall2$Variable<-" All Methods"
c2<-combine(by_method2, Overall2)
c$A_Type <- paste0("AA")
c2$A_Type <- paste0("AB")
mean_acc2<-mean(by_method2$Accuracy)



diff<-c$Accuracy-c2$Accuracy
c_diff<-data.frame(Variable=c$Variable, diff=(diff))
a<-c_diff

P1<-ggplot(c, aes(x = Accuracy, y = Variable)) +
  geom_point(position = position_nudge(y = -0.2)) +
  geom_point(data = c2,aes(x = Accuracy, y = Variable),position = position_nudge(y = 0.2)) +
  
  geom_errorbarh(aes(xmin = `Accuracy_Lower`, xmax = `Accuracy_Upper`), color="black",height = 0.2, size=1, position = position_nudge(y = -0.2)) +
  
  geom_errorbarh(data = c2, aes(xmin = `Accuracy_Lower`, xmax = `Accuracy_Upper`),color="black", size=1, height = 0.2,position = position_nudge(y = 0.2)) +
  
  
  
  geom_errorbarh(aes(xmin = `Accuracy_Lower`, xmax = `Accuracy_Upper`,color=A_Type), height = 0.2,position = position_nudge(y = -0.2)) +
  geom_point(data = c2,aes(x = Accuracy, y = Variable),size =0.75,position = position_nudge(y = 0.2)) +
  geom_errorbarh(data = c2, aes(xmin = `Accuracy_Lower`, xmax = `Accuracy_Upper`,color=A_Type),size =0.75, height = 0.2,position = position_nudge(y = 0.2)) +
  scale_linetype_manual(
    values = c("AA" = "solid", "AB" = "solid"),
    labels = c(bquote(bold("Unweighted Mean A"[A])), bquote(bold("Unweighted Mean A"[B])))
  )  +
  scale_color_manual(
    values = c("AA"="#f8766d", "AB"="#00bfc4"),
    labels = c(bquote(bold(A[A])), bquote(bold(A[B])))
  )+
  
  geom_vline(aes(xintercept = mean_acc, linetype = "AA"), color = "#f8766d")+
  geom_vline(aes(xintercept = mean_acc2, linetype = "AB"), color = "#00bfc4")+
  labs(x = "Accuracy", y = "Method") +
  theme_classic()+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  guides(linetype = guide_legend(title = ""))+
  theme(axis.text.x = element_text(size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))+
  guides(colour = guide_legend(title = "", ncol = 1))
  

 ? scale_linetype()

e88c82

library(ggpubr)

S12<-by_method
S12<-by_method%>% filter(Variable %in% c("12S-EF","12S-HN","12S-SN","12S-MT","12S-GN"))
COI<-by_method%>% filter(Variable %in% c("COI-EF","COI-HN","COI-SN","COI-MT","COI-GN"))
S12$eDNA<-"12S"
COI$eDNA<-"COI"

f<-combine(S12,COI)


g<-ggplot(f, aes(x=eDNA,Consistency))+
  geom_bar(stat="summary")+
  stat_summary(fun.data = mean_se,  
               geom = "errorbar") +
  labs(y="Mean Consistency", x="Method")+
  theme_classic()+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(size = 6,face = "bold"))+
  guides(colour = guide_legend(title = "Method",ncol = 1))+
  theme(axis.text.x = element_text(size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))

g2<-ggplot(f, aes(x=eDNA,Accuracy))+
  geom_bar(stat="summary")+
  stat_summary(fun.data = mean_se,  
               geom = "errorbar") +
  labs(y="Mean Accuracy", x="Method")+
  theme_classic()+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(size = 6,face = "bold"))+
  guides(colour = guide_legend(title = "Method",ncol = 1))+
  theme(axis.text.x = element_text(size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))
x<-data.frame(eDNA=c("12S","COI"), Congruency=c(2,3))
g3<-ggplot(x, aes(x=eDNA,Congruency))+
  geom_bar(stat="summary")+
  stat_summary(fun.data = mean_se,  
               geom = "errorbar") +
  labs(y="Number of Methods Congruent With", x="Method")+
  theme_classic()+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(size = 6,face = "bold"))+
  guides(colour = guide_legend(title = "Method",ncol = 1))+
  theme(axis.text.x = element_text(size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))
p<-ggarrange(g,g2,g3,nrow=1, labels = c("A", "B","C"), font.label = list(size=10))
library(ggpubr)

# Combos methods ----
# We have to do this with the eDNA and Trad dataframes that don't have the conventional conventional methods the confidence intervals shrink when you include them due to repetition. 

eDNA<-read_excel("Data/eDNA.xlsx")
Trad<-read_excel("Data/Trad.xlsx")
eDNA<- eDNA%>% filter(Method %in% c("12S-EF","12S-HN", "12S-SN","12S-MT","12S-GN","COI-EF","COI-HN", "COI-SN","COI-MT","COI-GN"))
Trad<- Trad%>% filter(Method %in% c("12S-EF","12S-HN", "12S-SN","12S-MT","12S-GN","COI-EF","COI-HN", "COI-SN","COI-MT","COI-GN"))
eDNA$Sample[eDNA$Sample=="08-07-23 - SunC-REF"] <- "eDNA-EF,GN,HN"
eDNA$Sample[eDNA$Sample=="09-07-23 - SunC-REF"] <- "eDNA-GN,HN,MT"
eDNA$Sample[eDNA$Sample=="08-07-23 - SunC-EXP"] <- "eDNA-EF,GN,HN,MT"
eDNA$Sample[eDNA$Sample=="10-07-23 - KC-EXP"] <- "eDNA-EF,MT"
eDNA$Sample[eDNA$Sample=="11-07-23 - KC-REF"] <- "eDNA-EF,MT, SN"
eDNA$Sample[eDNA$Sample=="12-07-23 - SunC-FF"] <- "eDNA-EF,GN"
eDNA$Sample[eDNA$Sample=="09-07-23 - SunC-EXP"] <- "eDNA-GN,HN,SN"
eDNA$Sample[eDNA$Sample=="12-07-23 - RJ-POND"] <- "eDNA-GN,MT"
eDNA$Sample[eDNA$Sample=="12-07-23 - HC-REF"] <- "eDNA-EF,GN,HN"
eDNA$Sample[eDNA$Sample=="13-07-23 - HC-REF"] <- "eDNA-GN,HN"


Trad$Sample[Trad$Sample=="08-07-23 - SunC-REF"] <- "eDNA-EF,GN,HN"
Trad$Sample[Trad$Sample=="09-07-23 - SunC-REF"] <- "eDNA-GN,HN,MT"
Trad$Sample[Trad$Sample=="08-07-23 - SunC-EXP"] <- "eDNA-EF,GN,HN,MT"
Trad$Sample[Trad$Sample=="10-07-23 - KC-EXP"] <- "eDNA-EF,MT"
Trad$Sample[Trad$Sample=="11-07-23 - KC-REF"] <- "eDNA-EF,MT, SN"
Trad$Sample[Trad$Sample=="12-07-23 - SunC-FF"] <- "eDNA-EF,GN"
Trad$Sample[Trad$Sample=="09-07-23 - SunC-EXP"] <- "eDNA-GN,HN,SN"
Trad$Sample[Trad$Sample=="12-07-23 - RJ-POND"] <- "eDNA-GN,MT"
Trad$Sample[Trad$Sample=="12-07-23 - HC-REF"] <- "eDNA-EF,GN,HN"
Trad$Sample[Trad$Sample=="13-07-23 - HC-REF"] <- "eDNA-GN,HN"


eDNA <-eDNA[eDNA $Value >= 1, ]
eDNA$Value[eDNA $Value>1]<-1
Trad<-Trad[Trad$Value >= 1, ]
Trad$Value[Trad$Value>1]<-1


eDNA<-subset(eDNA, select=-c(Method))
Trad<-subset(Trad, select=-c(Method))

eDNA<-eDNA[!duplicated(eDNA), ]
Trad<-Trad[!duplicated(Trad), ]
by_method2<-Accuracy_Consistency(eDNA, Trad, "Sample")



Overall2<-Accuracy_Consistency(eDNA, Trad,"")
Overall2$Variable<-" All Methods"
c2<-combine(by_method2, Overall2)

mean_con2<-mean(by_method2$Consistency)


CON_Plot_Combo<-ggplot(c2, aes(x = Consistency, y = Variable)) +
  geom_point(size = 1) +
  geom_errorbarh(aes(xmin = `Consistency_Lower`, xmax = `Consistency_Upper`), height = 0.2) +
  geom_vline(aes(xintercept = mean_con2,  linetype = "Unweighted Mean"),color = "red")+
  labs(x = "Consistency", y = "Method") +
  theme_classic()+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(size = 6,face = "bold"))+
  guides(linetype = guide_legend(title = ""))+
  theme(axis.text.x = element_text(size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))

mean_acc3<-mean(by_method2$Accuracy)

ACC_plot_Combo<-ggplot(c2, aes(x = Accuracy, y = Variable)) +
  geom_point(size = 1) +
  geom_errorbarh(aes(xmin = `Accuracy_Lower`, xmax = `Accuracy_Upper`), height = 0.2) +
  geom_vline(aes(xintercept = mean_acc3, linetype = "Unweighted Mean"), color = "red")+
  labs(x = "Accuracy", y = "Method") +
  theme_classic()+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(size = 6,face = "bold"))+
  guides(linetype = guide_legend(title = ""))+
  theme(axis.text.x = element_text(size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))
library(ggpubr)



by_method4<-Accuracy_Consistency(Trad,eDNA, "Sample")
Overall4<-Accuracy_Consistency(Trad,eDNA,"")
Overall4$Variable<-" All Methods"
c4<-combine(by_method4, Overall4)
c2$A_Type <- paste0("AA")
c4$A_Type <- paste0("AB")
mean_acc4<-mean(by_method4$Accuracy)
P2<-ggplot(c2, aes(x = Accuracy, y = Variable)) +
  geom_point(position = position_nudge(y = -0.2)) +
  geom_point(data = c4,aes(x = Accuracy, y = Variable),position = position_nudge(y = 0.2)) +
  geom_errorbarh(data = c4, aes(xmin = `Accuracy_Lower`, xmax = `Accuracy_Upper`), color="black",height = 0.2,position = position_nudge(y = 0.2), size=1) +
  
  geom_errorbarh(data=c2,aes(xmin = `Accuracy_Lower`, xmax = `Accuracy_Upper`,color=A_Type), height = 0.2,position = position_nudge(y = -0.2), size=1, color="black") +
  
  geom_errorbarh(aes(xmin = `Accuracy_Lower`, xmax = `Accuracy_Upper`,color=A_Type), height = 0.2,position = position_nudge(y = -0.2),size =0.75) +
  
   geom_errorbarh(data = c4, aes(xmin = `Accuracy_Lower`, xmax = `Accuracy_Upper`,color=A_Type), size =0.75,height = 0.2,position = position_nudge(y = 0.2)) +
  
  scale_linetype_manual(
    values = c("AA" = "solid", "AB" = "solid"),
    labels = c(bquote(bold("Unweighted Mean A"[A])), bquote(bold("Unweighted Mean A"[B])))
  )  +
  scale_color_manual(
    values = c("AA"="#f8766d", "AB"="#00bfc4"),
    labels = c(bquote(bold(A[A])), bquote(bold(A[B])))
  )+
  
  geom_vline(aes(xintercept = mean_acc3, linetype = "AA"), color = "#f8766d")+
  geom_vline(aes(xintercept = mean_acc4, linetype = "AB"), color = "#00bfc4")+
  labs(x = "Accuracy", y = "Method") +
  theme_classic()+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  guides(linetype = guide_legend(title = ""))+
  theme(axis.text.x = element_text(size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))+
  guides(colour = guide_legend(title = "", ncol = 1))



P2





# Consistency plot

Consistency_plot<-ggarrange(CON_Plot,CON_Plot_Combo,labels = c("A", "B"), font.label = list(size=10), nrow = 1, common.legend = TRUE, legend = "right")
Consistency_plot
Accuracy_plot<-ggarrange(ACC_plot,ACC_plot_Combo,labels = c("A", "B"), font.label = list(size=10), nrow = 1, common.legend = TRUE, legend = "right")


Accuracy_plot<-ggarrange(P1,ACC_plot_Combo,labels = c("A", "B"), font.label = list(size=10), nrow = 1, legend = "right")
Accuracy_plot<-ggarrange(P1,P2,labels = c("A", "B"), font.label = list(size=10), nrow = 1, legend = "right", common.legend = TRUE)
0.367/0.102


0.538/0.367









individual<-data.frame(Accuracy=mean(by_method$Accuracy),Consistency=mean( by_method$Consistency))
individual$test<-"Individual"
Combo<-data.frame(Accuracy=mean(by_method2$Accuracy),Consistency=mean( by_method2$Consistency))
Combo$test<-"Combinations"
j<-combine(individual,Combo)




g<-ggplot(j, aes(x=test,Consistency))+
  geom_bar(stat="summary")+
  stat_summary(fun.data = mean_se,  
               geom = "errorbar") +
  labs(y="Mean Consistency", x="Comparison")+
  theme_classic()+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(size = 6,face = "bold"))+
  guides(colour = guide_legend(title = "Method",ncol = 1))+
  theme(axis.text.x = element_text(size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))

g2<-ggplot(j, aes(x=test,Accuracy))+
  geom_bar(stat="summary")+
  stat_summary(fun.data = mean_se,  
               geom = "errorbar") +
  labs(y="Mean Accuracy", x="Comparison")+
  theme_classic()+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(size = 6,face = "bold"))+
  guides(colour = guide_legend(title = "Method",ncol = 1))+
  theme(axis.text.x = element_text(size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))

x<-data.frame(eDNA=c("eDNA","Conventional"), Congruency=c(5,1))
g3<-ggplot(x, aes(x=eDNA,Congruency))+
  geom_bar(stat="summary")+
  stat_summary(fun.data = mean_se,  
               geom = "errorbar") +
  labs(y="Number of Methods Congruent With", x="Method")+
  theme_classic()+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(size = 6,face = "bold"))+
  guides(colour = guide_legend(title = "Method",ncol = 1))+
  theme(axis.text.x = element_text(size=6,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(size=6,face="bold"))
p<-ggarrange(g,g2,g3,nrow=1, labels = c("A", "B","C"), font.label = list(size=10))





# 12S and COI ----

eDNA<-read_excel("Data/12S Data.xlsx")
eDNA$Method<-"12S-COI"

Trad<-read_excel("Data/COI Data.xlsx")
Trad$Method<-"12S-COI"


colnames(eDNA)[colnames(eDNA) == "value"] <- "Value"
colnames(Trad)[colnames(Trad) == "value"] <- "Value"
colnames(Trad)[colnames(Trad) == "variable"] <- "Sample"
colnames(eDNA)[colnames(eDNA) == "variable"] <- "Sample"

eDNA_12S<-eDNA
Trad_COI<-Trad
eDNA <-eDNA[eDNA $Value >= 1, ]
eDNA$Value[eDNA $Value>1]<-1
Trad<-Trad[Trad$Value >= 1, ]
Trad$Value[Trad$Value>1]<-1

Accuracy_Consistency(Trad,eDNA,"Method")
by_method_12S_COI<-Accuracy_Consistency(eDNA, Trad, "Method")

Accuracy_Consistency(Trad,eDNA, "Method")

# 12S vs all 
Trad<-read_excel("Data/Traditional Data.xlsx")
eDNA<-read_excel("Data/12S Data.xlsx")
eDNA$Method<-"12S-All Trad"


Trad$Method<-"12S-All Trad"


colnames(eDNA)[colnames(eDNA) == "value"] <- "Value"
colnames(Trad)[colnames(Trad) == "Count"] <- "Value"
colnames(Trad)[colnames(Trad) == "Date_site"] <- "Sample"
colnames(eDNA)[colnames(eDNA) == "variable"] <- "Sample"
eDNA<-subset(eDNA, select=-c(Method))
Trad<-subset(Trad, select=-c(Method))
eDNA <-eDNA[eDNA $Value >= 1, ]
eDNA$Value[eDNA $Value>1]<-1
Trad<-Trad[Trad$Value >= 1, ]
Trad$Value[Trad$Value>1]<-1
Accuracy_Consistency(eDNA, Trad, "")
Accuracy_Consistency(Trad,eDNA,"")


# COI 
Trad<-read_excel("Data/Traditional Data.xlsx")
eDNA<-read_excel("Data/COI Data.xlsx")
eDNA$Method<-"COI-All Trad"


Trad$Method<-"COI-All Trad"


colnames(eDNA)[colnames(eDNA) == "value"] <- "Value"
colnames(Trad)[colnames(Trad) == "Count"] <- "Value"
colnames(Trad)[colnames(Trad) == "Date_site"] <- "variable"

eDNA <-eDNA[eDNA $Value >= 1, ]
eDNA$Value[eDNA $Value>1]<-1
Trad<-Trad[Trad$Value >= 1, ]
Trad$Value[Trad$Value>1]<-1
Accuracy_Consistency(eDNA, Trad, "Method")
Accuracy_Consistency(Trad,eDNA,"Method")
45.161-58.065
