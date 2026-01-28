# 12 S ----
library(readxl)
x2<-read_excel("Data/12s200pb_species_table.xls")
x2[x2<=75 ] <- 0 # filter data
x2<-x2[rowSums(x2[7:70])>0,]
colnames(x2) <-sub("\\...*", "", colnames(x2))
unique(colnames(x2))
c<-colnames(x2)
c<-as.data.frame(c)
library(tidyverse)
v<-c %>%
  group_by(c) %>%
  summarise(Count= length(c))
z<-t(rowsum(t(x2[7:70]), group = colnames(x2[7:70]), na.rm = T))
z<-as.data.frame(z)
z$Species<-x2$Species
x2<-z
#Remove data for locations that we do not have traditional data for 
x2<-subset(x2, select=-c(`10-07-23-Negative_Control`,
                         `13-07-23-LIB-REF`,
                         `11-07-23-KC-DIVERT`,
                         `13-07-23-Negative_Control`,
                         `13-07-23-KC-EXP`,
                         `11-07-23-LIB-REF`,
                         `13-07-23-Negative_Control`,
                         `13-07-23-KC-EXP`,
                         `13-07-23-LIN-REF`,
                         `13-07-23-LIB-EXP`,
                         `13-07-23-LIN-EXP`,`09-07-23-Negative_Control`, `10-07-23-SC-REF`,`12-07-23-Negative_Control`))


# Change site names to consistent format
colnames(x2)[which(names(x2) == "08-07-23-SC-REF")] <- "08-07-23 - SunC-REF"
colnames(x2)[which(names(x2) == "09-07-23-SC-REF")] <- "09-07-23 - SunC-REF"
colnames(x2)[which(names(x2) == "08-07-23-SC-NF")] <- "08-07-23 - SunC-EXP"
colnames(x2)[which(names(x2) == "10-07-23-KC-EXP")] <- "10-07-23 - KC-EXP"
colnames(x2)[which(names(x2) == "11-07-23-KC-REF")] <- "11-07-23 - KC-REF"
colnames(x2)[which(names(x2) == "12-07-23-SC-FF")] <- "12-07-23 - SunC-FF"
colnames(x2)[which(names(x2) == "09-07-23-SC-NF")] <- "09-07-23 - SunC-EXP"
colnames(x2)[which(names(x2) == "11-07-23-FISH-POND")] <- "12-07-23 - RJ-POND"
colnames(x2)[which(names(x2) == "12-07-23-HC-REF")] <- "12-07-23 - HC-REF"
colnames(x2)[which(names(x2) == "13-07-23-HC-REF")] <- "13-07-23 - HC-REF"

unique(colnames(x2))

x2
library(reshape2)
S12<- melt(x2, id.vars = "Species")
unique(S12$variable)


write_xlsx(S12, "Data/12S Data.xlsx")
#COI Data----
x2<-read_excel("Data/COI_species_table.xls")

x2[x2<=75 ] <- 0 # Filter data 
x2<-x2[rowSums(x2[7:70])>0,]
colnames(x2) <-sub("\\...*", "", colnames(x2))
unique(colnames(x2))
z<-t(rowsum(t(x2[7:70]), group = colnames(x2[7:70]), na.rm = T))
z<-as.data.frame(z)
z$Species<-x2$Species
x2<-z
x2$`13-07-23-HC-REF`
#Remove data for locations that we do not have traditional data for 
x2<-subset(x2, select=-c(`10-07-23-Negative_Control`,
                         `13-07-23-LIB-REF`,
                         `11-07-23-KC-DIVERT`,
                         `13-07-23-Negative_Control`,
                         `13-07-23-KC-EXP`,
                         `11-07-23-LIB-REF`,
                         `13-07-23-Negative_Control`,
                         `13-07-23-KC-EXP`,
                         `13-07-23-LIN-REF`,
                         `13-07-23-LIB-EXP`,
                         `13-07-23-LIN-EXP`,`09-07-23-Negative_Control`, `10-07-23-SC-REF`,`12-07-23-Negative_Control`))


# Assuming x2 is your vector or dataframe column
colnames(x2)[which(names(x2) == "08-07-23-SC-REF")] <- "08-07-23 - SunC-REF"
colnames(x2)[which(names(x2) == "09-07-23-SC-REF")] <- "09-07-23 - SunC-REF"
colnames(x2)[which(names(x2) == "08-07-23-SC-NF")] <- "08-07-23 - SunC-EXP"
colnames(x2)[which(names(x2) == "10-07-23-KC-EXP")] <- "10-07-23 - KC-EXP"
colnames(x2)[which(names(x2) == "11-07-23-KC-REF")] <- "11-07-23 - KC-REF"
colnames(x2)[which(names(x2) == "12-07-23-SC-FF")] <- "12-07-23 - SunC-FF"
colnames(x2)[which(names(x2) == "09-07-23-SC-NF")] <- "09-07-23 - SunC-EXP"
colnames(x2)[which(names(x2) == "11-07-23-FISH-POND")] <- "12-07-23 - RJ-POND"
colnames(x2)[which(names(x2) == "12-07-23-HC-REF")] <- "12-07-23 - HC-REF"
colnames(x2)[which(names(x2) == "13-07-23-HC-REF")] <- "13-07-23 - HC-REF"

unique(colnames(x2))

x2
library(reshape2)
COI<- melt(x2, id.vars = "Species")
write_xlsx(COI, "Data/COI Data.xlsx")
# Traditional Data----
x1<-Detour_fish_data # Import CSV File by hand in order to keep right format
# Change species codes to name to match with eDNA data
x1$Species[x1$Species == "BURB"] <- "Burbot"
x1$Species[x1$Species == "YLPR"] <- "Yellow Perch"
x1$Species[x1$Species == "WHSC"] <- "White Sucker"
x1$Species[x1$Species == "NRPK"] <- "Northern Pike"
x1$Species[x1$Species == "BRTR"] <- "Brook Trout"
x1$Species[x1$Species == "LKCH"] <- "Lake Chub"
x1$Species[x1$Species == "WALL"] <- "Walleye"
x1$Species[x1$Species == "CRCH"] <- "Creek Chub"
x1$Species[x1$Species == "MTSC"] <- "Mottled Sculpin"
x1$Species[x1$Species == "IWDR"] <- "Iowa Darter"
library(tidyverse)
x1$Date_site <- paste(x1$`Date (dd-MM-yy)`, "-", x1$Site)

x1<- x1%>% filter(Date_site %in% c( "08-07-23 - SunC-REF",
                                    "09-07-23 - SunC-REF",
                                    "08-07-23 - SunC-EXP",
                                    "10-07-23 - KC-EXP",
                                    "11-07-23 - KC-REF",
                                    "12-07-23 - SunC-FF",
                                    "09-07-23 - SunC-EXP",
                                    "12-07-23 - RJ-POND",
                                    "12-07-23 - HC-REF",
                                    "13-07-23 - HC-REF"
))
x1$Count<-c(1)
x1<-x1 %>%
  group_by(Species,Date_site, Method) %>%
  summarise(Count= sum(Count))



library(writexl)
write_xlsx(x1, "Data/Traditional Data.xlsx")

