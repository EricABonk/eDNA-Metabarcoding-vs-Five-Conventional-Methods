# Arrange_And_Conduct_RV_Coefficient() function ----
# A function that automates the RV analysis
library(tidyverse)
library(ade4)
library(tidyverse)
library(ade4)

Arrange_And_Conduct_RV_Coefficient <- function(eDNA, Trad, methods_vector, Subgrouping_Column, Species_Column, Sample_Column, Value_Column) {
  results <- list()
  
  for (method in methods_vector) {
    eDNA$type <- "eDNA"
    Trad$type <- "Trad"
    
    eDNA_filtered <- eDNA %>% filter(!!sym(Subgrouping_Column) == method)
    Trad_filtered <- Trad %>% filter(!!sym(Subgrouping_Column) == method)
    
    eDNA_combined <- rbind(eDNA_filtered, Trad_filtered)
    eDNA_combined[[Value_Column]] <- ifelse(eDNA_combined$type == "Trad", 0, eDNA_combined[[Value_Column]])
    
    Trad_combined <- rbind(eDNA_filtered, Trad_filtered)
    Trad_combined[[Value_Column]] <- ifelse(Trad_combined$type == "eDNA", 0, Trad_combined[[Value_Column]])
    
    species <- unique(eDNA_combined[[Species_Column]])
    samples <- unique(eDNA_combined[[Sample_Column]])
    
    eDNA_sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))
    
    for (i in 1:nrow(eDNA_combined)) {
      species_idx <- which(species == eDNA_combined[[Species_Column]][i])
      sample_idx <- which(samples == eDNA_combined[[Sample_Column]][i])
      eDNA_sum_matrix[species_idx, sample_idx] <- eDNA_sum_matrix[species_idx, sample_idx] + eDNA_combined[[Value_Column]][i]
    }
    
    eDNA_combined <- as.data.frame(eDNA_sum_matrix)
    
    species <- unique(Trad_combined[[Species_Column]])
    samples <- unique(Trad_combined[[Sample_Column]])
    
    Trad_sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))
    
    for (i in 1:nrow(Trad_combined)) {
      species_idx <- which(species == Trad_combined[[Species_Column]][i])
      sample_idx <- which(samples == Trad_combined[[Sample_Column]][i])
      Trad_sum_matrix[species_idx, sample_idx] <- Trad_sum_matrix[species_idx, sample_idx] + Trad_combined[[Value_Column]][i]
    }
    
    Trad_combined <- as.data.frame(Trad_sum_matrix)
    
    SN_MT_Results <- RV.rtest(Trad_combined, eDNA_combined, nrepet = 1000)
    results[[method]] <- data.frame(Observation = SN_MT_Results$obs, P_Value = SN_MT_Results$pvalue)
  }
  
  # Combine the data frames into one table
  combined_df <- do.call(rbind, Map(cbind, Method = names(results), results))
  return(combined_df)
}

# Validate Function ----
library(readxl)
library(tidyverse)
eDNA <- read_excel("Data/eDNA.xlsx")
Trad <- read_excel("Data/Trad.xlsx")
methods <- c("12S-EF", "12S-GN", "12S-HN", "12S-MT", "12S-SN", "COI-EF", "COI-GN", "COI-HN", "COI-MT", "COI-SN", "EF-GN", "EF-HN", "EF-MT", "EF-SN", "HN-GN", "HN-MT", "HN-SN", "SN-GN", "SN-MT", "MT-GN")


eDNA1<- eDNA %>% filter(Method %in% c("12S-EF", "12S-GN", "12S-HN", "12S-MT", "12S-SN", "COI-EF", "COI-GN", "COI-HN", "COI-MT", "COI-SN"))
eDNA2<- eDNA %>% filter(!Method %in% c("12S-EF", "12S-GN", "12S-HN", "12S-MT", "12S-SN", "COI-EF", "COI-GN", "COI-HN", "COI-MT", "COI-SN"))
eDNA1<- eDNA1 %>%
  group_by(Sample, Method) %>%
  mutate(Value = Value / sum(Value)) %>%
  ungroup()
eDNA<-combine(eDNA1,eDNA2)
result <- Arrange_And_Conduct_RV_Coefficient(eDNA, Trad, methods, "Method", "Species", "Sample", "Value")
#12S-COI---- # Needed to add 12S-COI
eDNA<-read_excel("Data/12S Data.xlsx")
eDNA$Method<-"12S-COI"
eDNA <-eDNA[eDNA $value >= 1, ]
Trad<-read_excel("Data/COI Data.xlsx")
Trad$Method<-"12S-COI"
Trad <-Trad[Trad$value >= 1, ]

colnames(eDNA)[colnames(eDNA) == "value"] <- "Value"
colnames(Trad)[colnames(Trad) == "value"] <- "Value"


eDNA1<- eDNA %>%
  group_by(variable, Method) %>%
  mutate(Value = Value / sum(Value)) %>%
  ungroup()

Trad1<- Trad %>%
  group_by(variable, Method) %>%
  mutate(Value = Value / sum(Value)) %>%
  ungroup()
methods <- c("12S-COI")


result1 <- Arrange_And_Conduct_RV_Coefficient(eDNA, Trad, methods, "Method", "Species", "variable", "Value")

result<-combine(result,result1)


# Lets compare the results to the values we got from manually doing it
#result$Observation<-as.numeric(result$Observation)
#RV_Table<-read_excel("Data/RV Table.xlsx")
#RV_Table$result<-result$Observation
#RV_Table$Observation  <- as.numeric(RV_Table$Observation)
#RV_Table$Observation <- round(RV_Table$Observation, 6)
#RV_Table$result <- as.numeric(RV_Table$result)
#RV_Table$result <- round(RV_Table$result, 6)
symdiff(RV_Table$result, RV_Table$Observation) # No Difference


# Now lets make plot----
result$Relationship<-result$Method
combined_df_plot<-result
# Convert Observation and P_Value to numeric
combined_df_plot$Observation <- as.numeric(combined_df_plot$Observation)
combined_df_plot$P_Value <- as.numeric(combined_df_plot$P_Value)
# Split the Relationship column into two separate columns
combined_df_plot <- separate(combined_df_plot, Relationship, into = c("Relationship_1", "Relationship_2"), sep = "-")
combined_df_plot_reversed <- combined_df_plot
# Reverse the Relationship columns for combined_df_plot_reversed
combined_df_plot_reversed$Relationship_1 <- combined_df_plot$Relationship_2
combined_df_plot_reversed$Relationship_2 <- combined_df_plot$Relationship_1
# Append combined_df_plot_reversed to combined_df_plot
combined_df_plot <- rbind(combined_df_plot, combined_df_plot_reversed)
# Convert Relationship_1 and Relationship_2 to characters
combined_df_plot$Relationship_1 <- as.character(combined_df_plot$Relationship_1)
combined_df_plot$Relationship_2 <- as.character(combined_df_plot$Relationship_2)
# Sort the levels of Relationship_1 and Relationship_2 alphabetically
combined_df_plot$Relationship_1 <- factor(combined_df_plot$Relationship_1, levels = sort(unique(combined_df_plot$Relationship_1)))
combined_df_plot$Relationship_2 <- factor(combined_df_plot$Relationship_2, levels = sort(unique(combined_df_plot$Relationship_2)))
# Filter out the lower triangle
combined_df_plot <- combined_df_plot[as.character(combined_df_plot$Relationship_1) <= as.character(combined_df_plot$Relationship_2), ]
# Create the heatmap
heatmap <- ggplot(combined_df_plot, aes(x = Relationship_1, y = Relationship_2, fill = Observation)) +
geom_tile(color = "white") +
scale_fill_gradient(low = "blue", high = "red") +
geom_text(aes(label = round(P_Value, 2)), color = "black") + # Add text for P-value
theme_minimal() +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
labs(title = "",
x = "Method",
y = "Method",
fill = "RV Coefficient
")+ theme_classic()+
theme(legend.text =element_text(size = 6,face = "bold"))+
theme(legend.key.size = unit(.2, 'cm')) +
theme(legend.title = element_text(size = 6,face = "bold"))+
guides(linetype = guide_legend(title = ""))+
theme(axis.text.x = element_text(size=6,face="bold"))+
theme(axis.title = element_text(size = 8,face="bold"))+
theme(axis.text.y = element_text(size=6,face="bold"))

heatmap

library()

write_xlsx(result, "RVTableForPaper.xlsx")












