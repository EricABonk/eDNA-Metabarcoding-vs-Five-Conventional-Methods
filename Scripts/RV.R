library(tidyverse)
library(ade4)
eDNA<-read_excel("Data/eDNA.xlsx")
Trad<-read_excel("Data/Trad.xlsx")

eDNA$type <- "eDNA"
Trad$type <- "Trad"
# 12S-EF ----

eDNA_RV_12S_EF<-eDNA %>% filter(Method == "12S-EF")
Trad_RV_12S_EF<-Trad %>% filter(Method == "12S-EF")
eDNA_RV_12S_EF<-rbind(eDNA_RV_12S_EF,Trad_RV_12S_EF)
eDNA_RV_12S_EF$Value <- ifelse(eDNA_RV_12S_EF$type == "Trad", 0, eDNA_RV_12S_EF$Value)
eDNA_RV_12S_EF_G<-eDNA_RV_12S_EF
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_12S_EF<-eDNA %>% filter(Method == "12S-EF")
Trad_RV_12S_EF<-Trad %>% filter(Method == "12S-EF")
Trad_RV_12S_EF<-rbind(eDNA_RV_12S_EF,Trad_RV_12S_EF)
Trad_RV_12S_EF$Value <- ifelse(Trad_RV_12S_EF$type == "eDNA", 0, Trad_RV_12S_EF$Value)

Trad_RV_12S_EF

eDNA_RV_12S_EF<-eDNA_RV_12S_EF_G


# Create an empty matrix to store sums
species <- unique(eDNA_RV_12S_EF$Species)
samples <- unique(eDNA_RV_12S_EF$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_12S_EF)) {
  species_idx <- which(species == eDNA_RV_12S_EF$Species[i])
  sample_idx <- which(samples == eDNA_RV_12S_EF$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_12S_EF$Value[i]
}

# Convert matrix to data frame
eDNA_RV_12S_EF <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_12S_EF)
# Get unique species and samples
species <- unique(Trad_RV_12S_EF$Species)
samples <- unique(Trad_RV_12S_EF$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_12S_EF)) {
  species_idx <- which(species == Trad_RV_12S_EF$Species[i])
  sample_idx <- which(samples == Trad_RV_12S_EF$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_12S_EF$Value[i]
}
# Convert matrix to data frame
Trad_RV_12S_EF <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_12S_EF)
S12_EF_Results<-RV.rtest(Trad_RV_12S_EF,eDNA_RV_12S_EF, nrepet = 1000)

# 12S-HN----

eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_12S_HN<-eDNA %>% filter(Method == "12S-HN")
Trad_RV_12S_HN<-Trad %>% filter(Method == "12S-HN")
eDNA_RV_12S_HN<-rbind(eDNA_RV_12S_HN,Trad_RV_12S_HN)
eDNA_RV_12S_HN$Value <- ifelse(eDNA_RV_12S_HN$type == "Trad", 0, eDNA_RV_12S_HN$Value)
eDNA_RV_12S_HN_G<-eDNA_RV_12S_HN
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_12S_HN<-eDNA %>% filter(Method == "12S-HN")
Trad_RV_12S_HN<-Trad %>% filter(Method == "12S-HN")
Trad_RV_12S_HN<-rbind(eDNA_RV_12S_HN,Trad_RV_12S_HN)
Trad_RV_12S_HN$Value <- ifelse(Trad_RV_12S_HN$type == "eDNA", 0, Trad_RV_12S_HN$Value)

Trad_RV_12S_HN

eDNA_RV_12S_HN<-eDNA_RV_12S_HN_G
# Assuming your data is stored in a data frame named 'eDNA_RV_12S_HN'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_12S_HN$Species)
samples <- unique(eDNA_RV_12S_HN$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_12S_HN)) {
  species_idx <- which(species == eDNA_RV_12S_HN$Species[i])
  sample_idx <- which(samples == eDNA_RV_12S_HN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_12S_HN$Value[i]
}

# Convert matrix to data frame
eDNA_RV_12S_HN <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_12S_HN)
# Get unique species and samples
species <- unique(Trad_RV_12S_HN$Species)
samples <- unique(Trad_RV_12S_HN$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_12S_HN)) {
  species_idx <- which(species == Trad_RV_12S_HN$Species[i])
  sample_idx <- which(samples == Trad_RV_12S_HN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_12S_HN$Value[i]
}
# Convert matrix to data frame
Trad_RV_12S_HN <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_12S_HN)
S12_HN_Results<-RV.rtest(Trad_RV_12S_HN,eDNA_RV_12S_HN, nrepet = 1000)
S12_HN_Results




#12S-SN ----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_12S_SN<-eDNA %>% filter(Method == "12S-SN")
Trad_RV_12S_SN<-Trad %>% filter(Method == "12S-SN")
eDNA_RV_12S_SN<-rbind(eDNA_RV_12S_SN,Trad_RV_12S_SN)
eDNA_RV_12S_SN$Value <- ifelse(eDNA_RV_12S_SN$type == "Trad", 0, eDNA_RV_12S_SN$Value)
eDNA_RV_12S_SN_G<-eDNA_RV_12S_SN
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_12S_SN<-eDNA %>% filter(Method == "12S-SN")
Trad_RV_12S_SN<-Trad %>% filter(Method == "12S-SN")
Trad_RV_12S_SN<-rbind(eDNA_RV_12S_SN,Trad_RV_12S_SN)
Trad_RV_12S_SN$Value <- ifelse(Trad_RV_12S_SN$type == "eDNA", 0, Trad_RV_12S_SN$Value)

Trad_RV_12S_SN

eDNA_RV_12S_SN<-eDNA_RV_12S_SN_G
# Assuming your data is stored in a data frame named 'eDNA_RV_12S_SN'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_12S_SN$Species)
samples <- unique(eDNA_RV_12S_SN$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_12S_SN)) {
  species_idx <- which(species == eDNA_RV_12S_SN$Species[i])
  sample_idx <- which(samples == eDNA_RV_12S_SN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_12S_SN$Value[i]
}

# Convert matrix to data frame
eDNA_RV_12S_SN <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_12S_SN)
# Get unique species and samples
species <- unique(Trad_RV_12S_SN$Species)
samples <- unique(Trad_RV_12S_SN$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_12S_SN)) {
  species_idx <- which(species == Trad_RV_12S_SN$Species[i])
  sample_idx <- which(samples == Trad_RV_12S_SN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_12S_SN$Value[i]
}
# Convert matrix to data frame
Trad_RV_12S_SN <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_12S_SN)
S12_SN_Results<-RV.rtest(Trad_RV_12S_SN,eDNA_RV_12S_SN, nrepet = 1000)
S12_SN_Results
#12S-MT----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_12S_MT<-eDNA %>% filter(Method == "12S-MT")
Trad_RV_12S_MT<-Trad %>% filter(Method == "12S-MT")
eDNA_RV_12S_MT<-rbind(eDNA_RV_12S_MT,Trad_RV_12S_MT)
eDNA_RV_12S_MT$Value <- ifelse(eDNA_RV_12S_MT$type == "Trad", 0, eDNA_RV_12S_MT$Value)
eDNA_RV_12S_MT_G<-eDNA_RV_12S_MT
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_12S_MT<-eDNA %>% filter(Method == "12S-MT")
Trad_RV_12S_MT<-Trad %>% filter(Method == "12S-MT")
Trad_RV_12S_MT<-rbind(eDNA_RV_12S_MT,Trad_RV_12S_MT)
Trad_RV_12S_MT$Value <- ifelse(Trad_RV_12S_MT$type == "eDNA", 0, Trad_RV_12S_MT$Value)

Trad_RV_12S_MT

eDNA_RV_12S_MT<-eDNA_RV_12S_MT_G
# Assuming your data is stored in a data frame named 'eDNA_RV_12S_MT'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_12S_MT$Species)
samples <- unique(eDNA_RV_12S_MT$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_12S_MT)) {
  species_idx <- which(species == eDNA_RV_12S_MT$Species[i])
  sample_idx <- which(samples == eDNA_RV_12S_MT$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_12S_MT$Value[i]
}

# Convert matrix to data frame
eDNA_RV_12S_MT <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_12S_MT)
# Get unique species and samples
species <- unique(Trad_RV_12S_MT$Species)
samples <- unique(Trad_RV_12S_MT$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_12S_MT)) {
  species_idx <- which(species == Trad_RV_12S_MT$Species[i])
  sample_idx <- which(samples == Trad_RV_12S_MT$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_12S_MT$Value[i]
}
# Convert matrix to data frame
Trad_RV_12S_MT <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_12S_MT)
S12_MT_Results<-RV.rtest(Trad_RV_12S_MT,eDNA_RV_12S_MT, nrepet = 1000)
# 12S-GN----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_12S_GN<-eDNA %>% filter(Method == "12S-GN")
Trad_RV_12S_GN<-Trad %>% filter(Method == "12S-GN")
eDNA_RV_12S_GN<-rbind(eDNA_RV_12S_GN,Trad_RV_12S_GN)
eDNA_RV_12S_GN$Value <- ifelse(eDNA_RV_12S_GN$type == "Trad", 0, eDNA_RV_12S_GN$Value)
eDNA_RV_12S_GN_G<-eDNA_RV_12S_GN
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_12S_GN<-eDNA %>% filter(Method == "12S-GN")
Trad_RV_12S_GN<-Trad %>% filter(Method == "12S-GN")
Trad_RV_12S_GN<-rbind(eDNA_RV_12S_GN,Trad_RV_12S_GN)
Trad_RV_12S_GN$Value <- ifelse(Trad_RV_12S_GN$type == "eDNA", 0, Trad_RV_12S_GN$Value)

Trad_RV_12S_GN

eDNA_RV_12S_GN<-eDNA_RV_12S_GN_G
# Assuming your data is stored in a data frame named 'eDNA_RV_12S_GN'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_12S_GN$Species)
samples <- unique(eDNA_RV_12S_GN$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_12S_GN)) {
  species_idx <- which(species == eDNA_RV_12S_GN$Species[i])
  sample_idx <- which(samples == eDNA_RV_12S_GN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_12S_GN$Value[i]
}

# Convert matrix to data frame
eDNA_RV_12S_GN <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_12S_GN)
# Get unique species and samples
species <- unique(Trad_RV_12S_GN$Species)
samples <- unique(Trad_RV_12S_GN$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_12S_GN)) {
  species_idx <- which(species == Trad_RV_12S_GN$Species[i])
  sample_idx <- which(samples == Trad_RV_12S_GN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_12S_GN$Value[i]
}
# Convert matrix to data frame
Trad_RV_12S_GN <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_12S_GN)
S12_GN_Results<-RV.rtest(Trad_RV_12S_GN,eDNA_RV_12S_GN, nrepet = 1000)
S12_GN_Results


#
# COI-EF ----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_COI_EF<-eDNA %>% filter(Method == "COI-EF")
Trad_RV_COI_EF<-Trad %>% filter(Method == "COI-EF")
eDNA_RV_COI_EF<-rbind(eDNA_RV_COI_EF,Trad_RV_COI_EF)
eDNA_RV_COI_EF$Value <- ifelse(eDNA_RV_COI_EF$type == "Trad", 0, eDNA_RV_COI_EF$Value)
eDNA_RV_COI_EF_G<-eDNA_RV_COI_EF
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_COI_EF<-eDNA %>% filter(Method == "COI-EF")
Trad_RV_COI_EF<-Trad %>% filter(Method == "COI-EF")
Trad_RV_COI_EF<-rbind(eDNA_RV_COI_EF,Trad_RV_COI_EF)
Trad_RV_COI_EF$Value <- ifelse(Trad_RV_COI_EF$type == "eDNA", 0, Trad_RV_COI_EF$Value)

Trad_RV_COI_EF

eDNA_RV_COI_EF<-eDNA_RV_COI_EF_G
# Assuming your data is stored in a data frame named 'eDNA_RV_COI_EF'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_COI_EF$Species)
samples <- unique(eDNA_RV_COI_EF$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_COI_EF)) {
  species_idx <- which(species == eDNA_RV_COI_EF$Species[i])
  sample_idx <- which(samples == eDNA_RV_COI_EF$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_COI_EF$Value[i]
}

# Convert matrix to data frame
eDNA_RV_COI_EF <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_COI_EF)
# Get unique species and samples
species <- unique(Trad_RV_COI_EF$Species)
samples <- unique(Trad_RV_COI_EF$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_COI_EF)) {
  species_idx <- which(species == Trad_RV_COI_EF$Species[i])
  sample_idx <- which(samples == Trad_RV_COI_EF$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_COI_EF$Value[i]
}
# Convert matrix to data frame
Trad_RV_COI_EF <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_COI_EF)
COI_EF_Results<-RV.rtest(Trad_RV_COI_EF,eDNA_RV_COI_EF, nrepet = 1000)

# COI-HN----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_COI_HN<-eDNA %>% filter(Method == "COI-HN")
Trad_RV_COI_HN<-Trad %>% filter(Method == "COI-HN")
eDNA_RV_COI_HN<-rbind(eDNA_RV_COI_HN,Trad_RV_COI_HN)
eDNA_RV_COI_HN$Value <- ifelse(eDNA_RV_COI_HN$type == "Trad", 0, eDNA_RV_COI_HN$Value)
eDNA_RV_COI_HN_G<-eDNA_RV_COI_HN
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_COI_HN<-eDNA %>% filter(Method == "COI-HN")
Trad_RV_COI_HN<-Trad %>% filter(Method == "COI-HN")
Trad_RV_COI_HN<-rbind(eDNA_RV_COI_HN,Trad_RV_COI_HN)
Trad_RV_COI_HN$Value <- ifelse(Trad_RV_COI_HN$type == "eDNA", 0, Trad_RV_COI_HN$Value)

Trad_RV_COI_HN

eDNA_RV_COI_HN<-eDNA_RV_COI_HN_G
# Assuming your data is stored in a data frame named 'eDNA_RV_COI_HN'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_COI_HN$Species)
samples <- unique(eDNA_RV_COI_HN$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_COI_HN)) {
  species_idx <- which(species == eDNA_RV_COI_HN$Species[i])
  sample_idx <- which(samples == eDNA_RV_COI_HN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_COI_HN$Value[i]
}

# Convert matrix to data frame
eDNA_RV_COI_HN <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_COI_HN)
# Get unique species and samples
species <- unique(Trad_RV_COI_HN$Species)
samples <- unique(Trad_RV_COI_HN$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_COI_HN)) {
  species_idx <- which(species == Trad_RV_COI_HN$Species[i])
  sample_idx <- which(samples == Trad_RV_COI_HN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_COI_HN$Value[i]
}
# Convert matrix to data frame
Trad_RV_COI_HN <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_COI_HN)
COI_HN_Results<-RV.rtest(Trad_RV_COI_HN,eDNA_RV_COI_HN, nrepet = 1000)
COI_HN_Results
#COI-SN----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_COI_SN<-eDNA %>% filter(Method == "COI-SN")
Trad_RV_COI_SN<-Trad %>% filter(Method == "COI-SN")
eDNA_RV_COI_SN<-rbind(eDNA_RV_COI_SN,Trad_RV_COI_SN)
eDNA_RV_COI_SN$Value <- ifelse(eDNA_RV_COI_SN$type == "Trad", 0, eDNA_RV_COI_SN$Value)
eDNA_RV_COI_SN_G<-eDNA_RV_COI_SN
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_COI_SN<-eDNA %>% filter(Method == "COI-SN")
Trad_RV_COI_SN<-Trad %>% filter(Method == "COI-SN")
Trad_RV_COI_SN<-rbind(eDNA_RV_COI_SN,Trad_RV_COI_SN)
Trad_RV_COI_SN$Value <- ifelse(Trad_RV_COI_SN$type == "eDNA", 0, Trad_RV_COI_SN$Value)

Trad_RV_COI_SN

eDNA_RV_COI_SN<-eDNA_RV_COI_SN_G
# Assuming your data is stored in a data frame named 'eDNA_RV_COI_SN'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_COI_SN$Species)
samples <- unique(eDNA_RV_COI_SN$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_COI_SN)) {
  species_idx <- which(species == eDNA_RV_COI_SN$Species[i])
  sample_idx <- which(samples == eDNA_RV_COI_SN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_COI_SN$Value[i]
}

# Convert matrix to data frame
eDNA_RV_COI_SN <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_COI_SN)
# Get unique species and samples
species <- unique(Trad_RV_COI_SN$Species)
samples <- unique(Trad_RV_COI_SN$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_COI_SN)) {
  species_idx <- which(species == Trad_RV_COI_SN$Species[i])
  sample_idx <- which(samples == Trad_RV_COI_SN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_COI_SN$Value[i]
}
# Convert matrix to data frame
Trad_RV_COI_SN <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_COI_SN)
COI_SN_Results<-RV.rtest(Trad_RV_COI_SN,eDNA_RV_COI_SN, nrepet = 1000)
# COI-MT----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_COI_MT<-eDNA %>% filter(Method == "COI-MT")
Trad_RV_COI_MT<-Trad %>% filter(Method == "COI-MT")
eDNA_RV_COI_MT<-rbind(eDNA_RV_COI_MT,Trad_RV_COI_MT)
eDNA_RV_COI_MT$Value <- ifelse(eDNA_RV_COI_MT$type == "Trad", 0, eDNA_RV_COI_MT$Value)
eDNA_RV_COI_MT_G<-eDNA_RV_COI_MT
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_COI_MT<-eDNA %>% filter(Method == "COI-MT")
Trad_RV_COI_MT<-Trad %>% filter(Method == "COI-MT")
Trad_RV_COI_MT<-rbind(eDNA_RV_COI_MT,Trad_RV_COI_MT)
Trad_RV_COI_MT$Value <- ifelse(Trad_RV_COI_MT$type == "eDNA", 0, Trad_RV_COI_MT$Value)

Trad_RV_COI_MT

eDNA_RV_COI_MT<-eDNA_RV_COI_MT_G
# Assuming your data is stored in a data frame named 'eDNA_RV_COI_MT'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_COI_MT$Species)
samples <- unique(eDNA_RV_COI_MT$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_COI_MT)) {
  species_idx <- which(species == eDNA_RV_COI_MT$Species[i])
  sample_idx <- which(samples == eDNA_RV_COI_MT$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_COI_MT$Value[i]
}

# Convert matrix to data frame
eDNA_RV_COI_MT <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_COI_MT)
# Get unique species and samples
species <- unique(Trad_RV_COI_MT$Species)
samples <- unique(Trad_RV_COI_MT$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_COI_MT)) {
  species_idx <- which(species == Trad_RV_COI_MT$Species[i])
  sample_idx <- which(samples == Trad_RV_COI_MT$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_COI_MT$Value[i]
}
# Convert matrix to data frame
Trad_RV_COI_MT <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_COI_MT)
COI_MT_Results<-RV.rtest(Trad_RV_COI_MT,eDNA_RV_COI_MT, nrepet = 1000)
# COI-GN----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_COI_GN<-eDNA %>% filter(Method == "COI-GN")
Trad_RV_COI_GN<-Trad %>% filter(Method == "COI-GN")
eDNA_RV_COI_GN<-rbind(eDNA_RV_COI_GN,Trad_RV_COI_GN)
eDNA_RV_COI_GN$Value <- ifelse(eDNA_RV_COI_GN$type == "Trad", 0, eDNA_RV_COI_GN$Value)
eDNA_RV_COI_GN_G<-eDNA_RV_COI_GN
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_COI_GN<-eDNA %>% filter(Method == "COI-GN")
Trad_RV_COI_GN<-Trad %>% filter(Method == "COI-GN")
Trad_RV_COI_GN<-rbind(eDNA_RV_COI_GN,Trad_RV_COI_GN)
Trad_RV_COI_GN$Value <- ifelse(Trad_RV_COI_GN$type == "eDNA", 0, Trad_RV_COI_GN$Value)

Trad_RV_COI_GN

eDNA_RV_COI_GN<-eDNA_RV_COI_GN_G
# Assuming your data is stored in a data frame named 'eDNA_RV_COI_GN'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_COI_GN$Species)
samples <- unique(eDNA_RV_COI_GN$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_COI_GN)) {
  species_idx <- which(species == eDNA_RV_COI_GN$Species[i])
  sample_idx <- which(samples == eDNA_RV_COI_GN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_COI_GN$Value[i]
}

# Convert matrix to data frame
eDNA_RV_COI_GN <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_COI_GN)
# Get unique species and samples
species <- unique(Trad_RV_COI_GN$Species)
samples <- unique(Trad_RV_COI_GN$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_COI_GN)) {
  species_idx <- which(species == Trad_RV_COI_GN$Species[i])
  sample_idx <- which(samples == Trad_RV_COI_GN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_COI_GN$Value[i]
}
# Convert matrix to data frame
Trad_RV_COI_GN <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_COI_GN)
COI_GN_Results<-RV.rtest(Trad_RV_COI_GN,eDNA_RV_COI_GN, nrepet = 1000)
COI_GN_Results



# EF-HN----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_EF_HN<-eDNA %>% filter(Method == "EF-HN")
Trad_RV_EF_HN<-Trad %>% filter(Method == "EF-HN")
eDNA_RV_EF_HN<-rbind(eDNA_RV_EF_HN,Trad_RV_EF_HN)
eDNA_RV_EF_HN$Value <- ifelse(eDNA_RV_EF_HN$type == "Trad", 0, eDNA_RV_EF_HN$Value)
eDNA_RV_EF_HN_G<-eDNA_RV_EF_HN
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_EF_HN<-eDNA %>% filter(Method == "EF-HN")
Trad_RV_EF_HN<-Trad %>% filter(Method == "EF-HN")
Trad_RV_EF_HN<-rbind(eDNA_RV_EF_HN,Trad_RV_EF_HN)
Trad_RV_EF_HN$Value <- ifelse(Trad_RV_EF_HN$type == "eDNA", 0, Trad_RV_EF_HN$Value)

Trad_RV_EF_HN

eDNA_RV_EF_HN<-eDNA_RV_EF_HN_G
# Assuming your data is stored in a data frame named 'eDNA_RV_EF_HN'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_EF_HN$Species)
samples <- unique(eDNA_RV_EF_HN$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_EF_HN)) {
  species_idx <- which(species == eDNA_RV_EF_HN$Species[i])
  sample_idx <- which(samples == eDNA_RV_EF_HN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_EF_HN$Value[i]
}

# Convert matrix to data frame
eDNA_RV_EF_HN <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_EF_HN)
# Get unique species and samples
species <- unique(Trad_RV_EF_HN$Species)
samples <- unique(Trad_RV_EF_HN$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_EF_HN)) {
  species_idx <- which(species == Trad_RV_EF_HN$Species[i])
  sample_idx <- which(samples == Trad_RV_EF_HN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_EF_HN$Value[i]
}
# Convert matrix to data frame
Trad_RV_EF_HN <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_EF_HN)
EF_HN_Results<-RV.rtest(Trad_RV_EF_HN,eDNA_RV_EF_HN, nrepet = 1000)
EF_HN_Results
# EF-SN----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_EF_SN<-eDNA %>% filter(Method == "EF-SN")
Trad_RV_EF_SN<-Trad %>% filter(Method == "EF-SN")
eDNA_RV_EF_SN<-rbind(eDNA_RV_EF_SN,Trad_RV_EF_SN)
eDNA_RV_EF_SN$Value <- ifelse(eDNA_RV_EF_SN$type == "Trad", 0, eDNA_RV_EF_SN$Value)
eDNA_RV_EF_SN_G<-eDNA_RV_EF_SN
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_EF_SN<-eDNA %>% filter(Method == "EF-SN")
Trad_RV_EF_SN<-Trad %>% filter(Method == "EF-SN")
Trad_RV_EF_SN<-rbind(eDNA_RV_EF_SN,Trad_RV_EF_SN)
Trad_RV_EF_SN$Value <- ifelse(Trad_RV_EF_SN$type == "eDNA", 0, Trad_RV_EF_SN$Value)

Trad_RV_EF_SN

eDNA_RV_EF_SN<-eDNA_RV_EF_SN_G
# Assuming your data is stored in a data frame named 'eDNA_RV_EF_SN'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_EF_SN$Species)
samples <- unique(eDNA_RV_EF_SN$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_EF_SN)) {
  species_idx <- which(species == eDNA_RV_EF_SN$Species[i])
  sample_idx <- which(samples == eDNA_RV_EF_SN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_EF_SN$Value[i]
}

# Convert matrix to data frame
eDNA_RV_EF_SN <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_EF_SN)
# Get unique species and samples
species <- unique(Trad_RV_EF_SN$Species)
samples <- unique(Trad_RV_EF_SN$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_EF_SN)) {
  species_idx <- which(species == Trad_RV_EF_SN$Species[i])
  sample_idx <- which(samples == Trad_RV_EF_SN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_EF_SN$Value[i]
}
# Convert matrix to data frame
Trad_RV_EF_SN <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_EF_SN)
EF_SN_Results<-RV.rtest(Trad_RV_EF_SN,eDNA_RV_EF_SN, nrepet = 1000)
# EF-MT----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_EF_MT<-eDNA %>% filter(Method == "EF-MT")
Trad_RV_EF_MT<-Trad %>% filter(Method == "EF-MT")
eDNA_RV_EF_MT<-rbind(eDNA_RV_EF_MT,Trad_RV_EF_MT)
eDNA_RV_EF_MT$Value <- ifelse(eDNA_RV_EF_MT$type == "Trad", 0, eDNA_RV_EF_MT$Value)
eDNA_RV_EF_MT_G<-eDNA_RV_EF_MT
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_EF_MT<-eDNA %>% filter(Method == "EF-MT")
Trad_RV_EF_MT<-Trad %>% filter(Method == "EF-MT")
Trad_RV_EF_MT<-rbind(eDNA_RV_EF_MT,Trad_RV_EF_MT)
Trad_RV_EF_MT$Value <- ifelse(Trad_RV_EF_MT$type == "eDNA", 0, Trad_RV_EF_MT$Value)

Trad_RV_EF_MT

eDNA_RV_EF_MT<-eDNA_RV_EF_MT_G
# Assuming your data is stored in a data frame named 'eDNA_RV_EF_MT'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_EF_MT$Species)
samples <- unique(eDNA_RV_EF_MT$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_EF_MT)) {
  species_idx <- which(species == eDNA_RV_EF_MT$Species[i])
  sample_idx <- which(samples == eDNA_RV_EF_MT$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_EF_MT$Value[i]
}

# Convert matrix to data frame
eDNA_RV_EF_MT <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_EF_MT)
# Get unique species and samples
species <- unique(Trad_RV_EF_MT$Species)
samples <- unique(Trad_RV_EF_MT$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_EF_MT)) {
  species_idx <- which(species == Trad_RV_EF_MT$Species[i])
  sample_idx <- which(samples == Trad_RV_EF_MT$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_EF_MT$Value[i]
}
# Convert matrix to data frame
Trad_RV_EF_MT <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_EF_MT)
EF_MT_Results<-RV.rtest(Trad_RV_EF_MT,eDNA_RV_EF_MT, nrepet = 1000)
# EF-GN----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_EF_GN<-eDNA %>% filter(Method == "EF-GN")
Trad_RV_EF_GN<-Trad %>% filter(Method == "EF-GN")
eDNA_RV_EF_GN<-rbind(eDNA_RV_EF_GN,Trad_RV_EF_GN)
eDNA_RV_EF_GN$Value <- ifelse(eDNA_RV_EF_GN$type == "Trad", 0, eDNA_RV_EF_GN$Value)
eDNA_RV_EF_GN_G<-eDNA_RV_EF_GN
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_EF_GN<-eDNA %>% filter(Method == "EF-GN")
Trad_RV_EF_GN<-Trad %>% filter(Method == "EF-GN")
Trad_RV_EF_GN<-rbind(eDNA_RV_EF_GN,Trad_RV_EF_GN)
Trad_RV_EF_GN$Value <- ifelse(Trad_RV_EF_GN$type == "eDNA", 0, Trad_RV_EF_GN$Value)

Trad_RV_EF_GN

eDNA_RV_EF_GN<-eDNA_RV_EF_GN_G
# Assuming your data is stored in a data frame named 'eDNA_RV_EF_GN'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_EF_GN$Species)
samples <- unique(eDNA_RV_EF_GN$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_EF_GN)) {
  species_idx <- which(species == eDNA_RV_EF_GN$Species[i])
  sample_idx <- which(samples == eDNA_RV_EF_GN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_EF_GN$Value[i]
}

# Convert matrix to data frame
eDNA_RV_EF_GN <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_EF_GN)
# Get unique species and samples
species <- unique(Trad_RV_EF_GN$Species)
samples <- unique(Trad_RV_EF_GN$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_EF_GN)) {
  species_idx <- which(species == Trad_RV_EF_GN$Species[i])
  sample_idx <- which(samples == Trad_RV_EF_GN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_EF_GN$Value[i]
}
# Convert matrix to data frame
Trad_RV_EF_GN <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_EF_GN)
EF_GN_Results<-RV.rtest(Trad_RV_EF_GN,eDNA_RV_EF_GN, nrepet = 1000)
EF_GN_Results
















# HN-SN----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_HN_SN<-eDNA %>% filter(Method == "HN-SN")
Trad_RV_HN_SN<-Trad %>% filter(Method == "HN-SN")
eDNA_RV_HN_SN<-rbind(eDNA_RV_HN_SN,Trad_RV_HN_SN)
eDNA_RV_HN_SN$Value <- ifelse(eDNA_RV_HN_SN$type == "Trad", 0, eDNA_RV_HN_SN$Value)
eDNA_RV_HN_SN_G<-eDNA_RV_HN_SN
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_HN_SN<-eDNA %>% filter(Method == "HN-SN")
Trad_RV_HN_SN<-Trad %>% filter(Method == "HN-SN")
Trad_RV_HN_SN<-rbind(eDNA_RV_HN_SN,Trad_RV_HN_SN)
Trad_RV_HN_SN$Value <- ifelse(Trad_RV_HN_SN$type == "eDNA", 0, Trad_RV_HN_SN$Value)

Trad_RV_HN_SN

eDNA_RV_HN_SN<-eDNA_RV_HN_SN_G
# Assuming your data is stored in a data frame named 'eDNA_RV_HN_SN'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_HN_SN$Species)
samples <- unique(eDNA_RV_HN_SN$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_HN_SN)) {
  species_idx <- which(species == eDNA_RV_HN_SN$Species[i])
  sample_idx <- which(samples == eDNA_RV_HN_SN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_HN_SN$Value[i]
}

# Convert matrix to data frame
eDNA_RV_HN_SN <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_HN_SN)
# Get unique species and samples
species <- unique(Trad_RV_HN_SN$Species)
samples <- unique(Trad_RV_HN_SN$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_HN_SN)) {
  species_idx <- which(species == Trad_RV_HN_SN$Species[i])
  sample_idx <- which(samples == Trad_RV_HN_SN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_HN_SN$Value[i]
}
# Convert matrix to data frame
Trad_RV_HN_SN <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_HN_SN)
HN_SN_Results<-RV.rtest(Trad_RV_HN_SN,eDNA_RV_HN_SN, nrepet = 1000)
# HN-MT----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_HN_MT<-eDNA %>% filter(Method == "HN-MT")
Trad_RV_HN_MT<-Trad %>% filter(Method == "HN-MT")
eDNA_RV_HN_MT<-rbind(eDNA_RV_HN_MT,Trad_RV_HN_MT)
eDNA_RV_HN_MT$Value <- ifelse(eDNA_RV_HN_MT$type == "Trad", 0, eDNA_RV_HN_MT$Value)
eDNA_RV_HN_MT_G<-eDNA_RV_HN_MT
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_HN_MT<-eDNA %>% filter(Method == "HN-MT")
Trad_RV_HN_MT<-Trad %>% filter(Method == "HN-MT")
Trad_RV_HN_MT<-rbind(eDNA_RV_HN_MT,Trad_RV_HN_MT)
Trad_RV_HN_MT$Value <- ifelse(Trad_RV_HN_MT$type == "eDNA", 0, Trad_RV_HN_MT$Value)

Trad_RV_HN_MT

eDNA_RV_HN_MT<-eDNA_RV_HN_MT_G
# Assuming your data is stored in a data frame named 'eDNA_RV_HN_MT'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_HN_MT$Species)
samples <- unique(eDNA_RV_HN_MT$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_HN_MT)) {
  species_idx <- which(species == eDNA_RV_HN_MT$Species[i])
  sample_idx <- which(samples == eDNA_RV_HN_MT$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_HN_MT$Value[i]
}

# Convert matrix to data frame
eDNA_RV_HN_MT <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_HN_MT)
# Get unique species and samples
species <- unique(Trad_RV_HN_MT$Species)
samples <- unique(Trad_RV_HN_MT$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_HN_MT)) {
  species_idx <- which(species == Trad_RV_HN_MT$Species[i])
  sample_idx <- which(samples == Trad_RV_HN_MT$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_HN_MT$Value[i]
}
# Convert matrix to data frame
Trad_RV_HN_MT <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_HN_MT)
HN_MT_Results<-RV.rtest(Trad_RV_HN_MT,eDNA_RV_HN_MT, nrepet = 1000)
# HN-GN----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_HN_GN<-eDNA %>% filter(Method == "HN-GN")
Trad_RV_HN_GN<-Trad %>% filter(Method == "HN-GN")
eDNA_RV_HN_GN<-rbind(eDNA_RV_HN_GN,Trad_RV_HN_GN)
eDNA_RV_HN_GN$Value <- ifelse(eDNA_RV_HN_GN$type == "Trad", 0, eDNA_RV_HN_GN$Value)
eDNA_RV_HN_GN_G<-eDNA_RV_HN_GN
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_HN_GN<-eDNA %>% filter(Method == "HN-GN")
Trad_RV_HN_GN<-Trad %>% filter(Method == "HN-GN")
Trad_RV_HN_GN<-rbind(eDNA_RV_HN_GN,Trad_RV_HN_GN)
Trad_RV_HN_GN$Value <- ifelse(Trad_RV_HN_GN$type == "eDNA", 0, Trad_RV_HN_GN$Value)

Trad_RV_HN_GN

eDNA_RV_HN_GN<-eDNA_RV_HN_GN_G
# Assuming your data is stored in a data frame named 'eDNA_RV_HN_GN'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_HN_GN$Species)
samples <- unique(eDNA_RV_HN_GN$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_HN_GN)) {
  species_idx <- which(species == eDNA_RV_HN_GN$Species[i])
  sample_idx <- which(samples == eDNA_RV_HN_GN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_HN_GN$Value[i]
}

# Convert matrix to data frame
eDNA_RV_HN_GN <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_HN_GN)
# Get unique species and samples
species <- unique(Trad_RV_HN_GN$Species)
samples <- unique(Trad_RV_HN_GN$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_HN_GN)) {
  species_idx <- which(species == Trad_RV_HN_GN$Species[i])
  sample_idx <- which(samples == Trad_RV_HN_GN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_HN_GN$Value[i]
}
# Convert matrix to data frame
Trad_RV_HN_GN <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_HN_GN)
HN_GN_Results<-RV.rtest(Trad_RV_HN_GN,eDNA_RV_HN_GN, nrepet = 1000)
HN_GN_Results


# SN-MT----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_SN_MT<-eDNA %>% filter(Method == "SN-MT")
Trad_RV_SN_MT<-Trad %>% filter(Method == "SN-MT")
eDNA_RV_SN_MT<-rbind(eDNA_RV_SN_MT,Trad_RV_SN_MT)
eDNA_RV_SN_MT$Value <- ifelse(eDNA_RV_SN_MT$type == "Trad", 0, eDNA_RV_SN_MT$Value)
eDNA_RV_SN_MT_G<-eDNA_RV_SN_MT
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_SN_MT<-eDNA %>% filter(Method == "SN-MT")
Trad_RV_SN_MT<-Trad %>% filter(Method == "SN-MT")
Trad_RV_SN_MT<-rbind(eDNA_RV_SN_MT,Trad_RV_SN_MT)
Trad_RV_SN_MT$Value <- ifelse(Trad_RV_SN_MT$type == "eDNA", 0, Trad_RV_SN_MT$Value)

Trad_RV_SN_MT

eDNA_RV_SN_MT<-eDNA_RV_SN_MT_G
# Assuming your data is stored in a data frame named 'eDNA_RV_SN_MT'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_SN_MT$Species)
samples <- unique(eDNA_RV_SN_MT$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_SN_MT)) {
  species_idx <- which(species == eDNA_RV_SN_MT$Species[i])
  sample_idx <- which(samples == eDNA_RV_SN_MT$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_SN_MT$Value[i]
}

# Convert matrix to data frame
eDNA_RV_SN_MT <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_SN_MT)
# Get unique species and samples
species <- unique(Trad_RV_SN_MT$Species)
samples <- unique(Trad_RV_SN_MT$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_SN_MT)) {
  species_idx <- which(species == Trad_RV_SN_MT$Species[i])
  sample_idx <- which(samples == Trad_RV_SN_MT$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_SN_MT$Value[i]
}
# Convert matrix to data frame
Trad_RV_SN_MT <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_SN_MT)
SN_MT_Results<-RV.rtest(Trad_RV_SN_MT,eDNA_RV_SN_MT, nrepet = 1000)
# SN-GN----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_SN_GN<-eDNA %>% filter(Method == "SN-GN")
Trad_RV_SN_GN<-Trad %>% filter(Method == "SN-GN")
eDNA_RV_SN_GN<-rbind(eDNA_RV_SN_GN,Trad_RV_SN_GN)
eDNA_RV_SN_GN$Value <- ifelse(eDNA_RV_SN_GN$type == "Trad", 0, eDNA_RV_SN_GN$Value)
eDNA_RV_SN_GN_G<-eDNA_RV_SN_GN
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_SN_GN<-eDNA %>% filter(Method == "SN-GN")
Trad_RV_SN_GN<-Trad %>% filter(Method == "SN-GN")
Trad_RV_SN_GN<-rbind(eDNA_RV_SN_GN,Trad_RV_SN_GN)
Trad_RV_SN_GN$Value <- ifelse(Trad_RV_SN_GN$type == "eDNA", 0, Trad_RV_SN_GN$Value)

Trad_RV_SN_GN

eDNA_RV_SN_GN<-eDNA_RV_SN_GN_G
# Assuming your data is stored in a data frame named 'eDNA_RV_SN_GN'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_SN_GN$Species)
samples <- unique(eDNA_RV_SN_GN$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_SN_GN)) {
  species_idx <- which(species == eDNA_RV_SN_GN$Species[i])
  sample_idx <- which(samples == eDNA_RV_SN_GN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_SN_GN$Value[i]
}

# Convert matrix to data frame
eDNA_RV_SN_GN <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_SN_GN)
# Get unique species and samples
species <- unique(Trad_RV_SN_GN$Species)
samples <- unique(Trad_RV_SN_GN$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_SN_GN)) {
  species_idx <- which(species == Trad_RV_SN_GN$Species[i])
  sample_idx <- which(samples == Trad_RV_SN_GN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_SN_GN$Value[i]
}
# Convert matrix to data frame
Trad_RV_SN_GN <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_SN_GN)
SN_GN_Results<-RV.rtest(Trad_RV_SN_GN,eDNA_RV_SN_GN, nrepet = 1000)
SN_GN_Results

# MT-GN----
eDNA$type <- "eDNA"
Trad$type <- "Trad"
unique(eDNA$Method)

eDNA_RV_MT_GN<-eDNA %>% filter(Method == "MT-GN")
Trad_RV_MT_GN<-Trad %>% filter(Method == "MT-GN")
eDNA_RV_MT_GN<-rbind(eDNA_RV_MT_GN,Trad_RV_MT_GN)
eDNA_RV_MT_GN$Value <- ifelse(eDNA_RV_MT_GN$type == "Trad", 0, eDNA_RV_MT_GN$Value)
eDNA_RV_MT_GN_G<-eDNA_RV_MT_GN
eDNA$type <- "eDNA"
Trad$type <- "Trad"
eDNA_RV_MT_GN<-eDNA %>% filter(Method == "MT-GN")
Trad_RV_MT_GN<-Trad %>% filter(Method == "MT-GN")
Trad_RV_MT_GN<-rbind(eDNA_RV_MT_GN,Trad_RV_MT_GN)
Trad_RV_MT_GN$Value <- ifelse(Trad_RV_MT_GN$type == "eDNA", 0, Trad_RV_MT_GN$Value)

Trad_RV_MT_GN

eDNA_RV_MT_GN<-eDNA_RV_MT_GN_G
# Assuming your data is stored in a data frame named 'eDNA_RV_MT_GN'

# Create an empty matrix to store sums
species <- unique(eDNA_RV_MT_GN$Species)
samples <- unique(eDNA_RV_MT_GN$Sample)

sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples), dimnames = list(species, samples))

# Fill in the sums
for (i in 1:nrow(eDNA_RV_MT_GN)) {
  species_idx <- which(species == eDNA_RV_MT_GN$Species[i])
  sample_idx <- which(samples == eDNA_RV_MT_GN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + eDNA_RV_MT_GN$Value[i]
}

# Convert matrix to data frame
eDNA_RV_MT_GN <- as.data.frame(sum_matrix)

# Print the result
print(eDNA_RV_MT_GN)
# Get unique species and samples
species <- unique(Trad_RV_MT_GN$Species)
samples <- unique(Trad_RV_MT_GN$Sample)
# Create an empty sum matrix
sum_matrix <- matrix(0, nrow = length(species), ncol = length(samples),
                     dimnames = list(species, samples))
# Fill in the sums
for (i in 1:nrow(Trad_RV_MT_GN)) {
  species_idx <- which(species == Trad_RV_MT_GN$Species[i])
  sample_idx <- which(samples == Trad_RV_MT_GN$Sample[i])
  sum_matrix[species_idx, sample_idx] <- sum_matrix[species_idx, sample_idx] + Trad_RV_MT_GN$Value[i]
}
# Convert matrix to data frame
Trad_RV_MT_GN <- as.data.frame(sum_matrix)

# Print the result
print(Trad_RV_MT_GN)
MT_GN_Results<-RV.rtest(Trad_RV_MT_GN,eDNA_RV_MT_GN, nrepet = 1000)
MT_GN_Results


# Table---- 
# Create data frames for S12 results
S12_EF_df <- data.frame(Observation = S12_EF_Results$obs, P_Value = S12_EF_Results$pvalue)
S12_GN_df <- data.frame(Observation = S12_GN_Results$obs, P_Value = S12_GN_Results$pvalue)
S12_HN_df <- data.frame(Observation = S12_HN_Results$obs, P_Value = S12_HN_Results$pvalue)
S12_MT_df <- data.frame(Observation = S12_MT_Results$obs, P_Value = S12_MT_Results$pvalue)
S12_SN_df <- data.frame(Observation = S12_SN_Results$obs, P_Value = S12_SN_Results$pvalue)

# Create data frames for COI results
COI_EF_df <- data.frame(Observation = COI_EF_Results$obs, P_Value = COI_EF_Results$pvalue)
COI_GN_df <- data.frame(Observation = COI_GN_Results$obs, P_Value = COI_GN_Results$pvalue)
COI_HN_df <- data.frame(Observation = COI_HN_Results$obs, P_Value = COI_HN_Results$pvalue)
COI_MT_df <- data.frame(Observation = COI_MT_Results$obs, P_Value = COI_MT_Results$pvalue)
COI_SN_df <- data.frame(Observation = COI_SN_Results$obs, P_Value = COI_SN_Results$pvalue)

# For EF
EF_GN_df <- data.frame(Observation = EF_GN_Results$obs, P_Value = EF_GN_Results$pvalue)
EF_HN_df <- data.frame(Observation = EF_HN_Results$obs, P_Value = EF_HN_Results$pvalue)
EF_MT_df <- data.frame(Observation = EF_MT_Results$obs, P_Value = EF_MT_Results$pvalue)
EF_SN_df <- data.frame(Observation = EF_SN_Results$obs, P_Value = EF_SN_Results$pvalue)

#FOR HN 
HN_GN_df <- data.frame(Observation = HN_GN_Results$obs, P_Value = HN_GN_Results$pvalue)
HN_MT_df <- data.frame(Observation = HN_MT_Results$obs, P_Value = HN_MT_Results$pvalue)
HN_SN_df <- data.frame(Observation = HN_SN_Results$obs, P_Value = HN_SN_Results$pvalue)


# FOr SN 

SN_GN_df <- data.frame(Observation = SN_GN_Results$obs, P_Value = SN_GN_Results$pvalue)
SN_MT_df <- data.frame(Observation = SN_MT_Results$obs, P_Value = SN_MT_Results$pvalue)
# For MT
MT_GN_df <- data.frame(Observation = MT_GN_Results$obs, P_Value = MT_GN_Results$pvalue)




# Add a relationship column with modified format
relationship_format <- function(x, y) {
  if (x == "S12") {
    x <- "12S"
  }
  return(paste(x, y, sep = "-"))
}
S12_EF_df$Relationship <- relationship_format("S12", "EF")
S12_GN_df$Relationship <- relationship_format("S12", "GN")
S12_HN_df$Relationship <- relationship_format("S12", "HN")
S12_MT_df$Relationship <- relationship_format("S12", "MT")
S12_SN_df$Relationship <- relationship_format("S12", "SN")
COI_EF_df$Relationship <- relationship_format("COI", "EF")
COI_GN_df$Relationship <- relationship_format("COI", "GN")
COI_HN_df$Relationship <- relationship_format("COI", "HN")
COI_MT_df$Relationship <- relationship_format("COI", "MT")
COI_SN_df$Relationship <- relationship_format("COI", "SN")
EF_GN_df$Relationship <- relationship_format("EF", "GN")
EF_HN_df$Relationship <- relationship_format("EF", "HN")
EF_MT_df$Relationship <- relationship_format("EF", "MT")
EF_SN_df$Relationship <- relationship_format("EF", "SN")

HN_GN_df$Relationship <- relationship_format("HN", "GN")
HN_MT_df$Relationship <- relationship_format("HN", "MT")
HN_SN_df$Relationship <- relationship_format("HN", "SN")

SN_GN_df$Relationship <- relationship_format("SN", "GN")
SN_MT_df$Relationship <- relationship_format("SN", "MT")
MT_GN_df$Relationship <- relationship_format("MT", "GN")
# Combine all data frames into one
combined_df <- rbind(S12_EF_df,S12_GN_df, S12_HN_df, S12_MT_df, S12_SN_df,
                     COI_EF_df, COI_GN_df, COI_HN_df, COI_MT_df, COI_SN_df,
                      EF_GN_df, EF_HN_df, EF_MT_df, EF_SN_df, 
                     HN_GN_df, HN_MT_df, HN_SN_df, 
                     SN_GN_df, SN_MT_df, MT_GN_df)

# Print the combined dataframe
print(combined_df)

library(writexl)
write_xlsx(combined_df, "Data/RV Table1.xlsx")

# Heat plot -----

combined_df_plot<-read_excel("RV Table.xlsx")
# Load the required libraries
library(ggplot2)
library(tidyr)


# Convert Observation and P_Value to numeric
combined_df_plot$Observation <- as.numeric(combined_df_plot$Observation)
combined_df_plot$P_Value <- as.numeric(combined_df_plot$P_Value)

# Split the Relationship column into two separate columns
combined_df_plot <- separate(combined_df_plot, Relationship, into = c("Relationship_1", "Relationship_2"), sep = "-")

# Create a copy of combined_df_plot to store the reversed relationships
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

# Print the heatmap
print(heatmap)
