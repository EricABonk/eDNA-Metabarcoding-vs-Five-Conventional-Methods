# Make dataframe for Function 
sampling<-read_excel("Data/Detour Sampling.xlsx")
library(readxl)
S12<-read_excel("Data/12S Data.xlsx")
S12 <-S12[S12$value >= 1, ]

Trad<-read_excel("Data/Traditional Data.xlsx")
# EF----
EF_Sampling_Days<-sampling%>% filter(Method %in% c( "EF"))
S12$Sample<-S12$variable
S12_EF<-S12%>% filter(Sample %in% EF_Sampling_Days$Sample)
S12_EF$Relationship<-"12S-EF"
Trad_12S_EF<-Trad%>% filter(Method %in% c("EF"))
Trad_12S_EF$Relationship<-"12S-EF"
#HN----
HN_Sampling_Days<-sampling%>% filter(Method %in% c("HN"))
S12$Sample<-S12$variable
S12_HN<-S12%>% filter(Sample %in% HN_Sampling_Days$Sample)
S12_HN$Relationship<-"12S-HN"
Trad_12S_HN<-Trad%>% filter(Method %in% c("HN"))
Trad_12S_HN$Relationship<-"12S-HN"
# SN----
SN_Sampling_Days<-sampling%>% filter(Method %in% c("SN"))
S12$Sample<-S12$variable
S12_SN<-S12%>% filter(Sample %in% SN_Sampling_Days$Sample)
S12_SN$Relationship<-"12S-SN"
Trad_12S_SN<-Trad%>% filter(Method %in% c("SN"))
Trad_12S_SN$Relationship<-"12S-SN"
# MT----
MT_Sampling_Days<-sampling%>% filter(Method %in% c("MT"))
S12$Sample<-S12$variable
S12_MT<-S12%>% filter(Sample %in% MT_Sampling_Days$Sample)
S12_MT$Relationship<-"12S-MT"
Trad_12S_MT<-Trad%>% filter(Method %in% c("MT"))
Trad_12S_MT$Relationship<-"12S-MT"


# GN----
GN_Sampling_Days<-sampling%>% filter(Method %in% c("GN"))
S12$Sample<-S12$variable
S12_GN<-S12%>% filter(Sample %in% GN_Sampling_Days$Sample)
S12_GN$Relationship<-"12S-GN"
Trad_12S_GN<-Trad%>% filter(Method %in% c("GN"))
Trad_12S_GN$Relationship<-"12S-GN"


#COI----

COI<-read_excel("Data/COI Data.xlsx")
COI <-COI[COI$value >= 1, ]
# EF ----

EF_Sampling_Days<-sampling%>% filter(Method %in% c("EF"))
COI$Sample<-COI$variable
COI_EF<-COI%>% filter(Sample %in% EF_Sampling_Days$Sample)
COI_EF$Relationship<-"COI-EF"
Trad_COI_EF<-Trad%>% filter(Method %in% c("EF"))
Trad_COI_EF$Relationship<-"COI-EF"
#HN----
HN_Sampling_Days<-sampling%>% filter(Method %in% c("HN"))
COI$Sample<-COI$variable
COI_HN<-COI%>% filter(Sample %in% HN_Sampling_Days$Sample)
COI_HN$Relationship<-"COI-HN"
Trad_COI_HN<-Trad%>% filter(Method %in% c("HN"))
Trad_COI_HN$Relationship<-"COI-HN"
# SN----
SN_Sampling_Days<-sampling%>% filter(Method %in% c("SN"))
COI$Sample<-COI$variable
COI_SN<-COI%>% filter(Sample %in% SN_Sampling_Days$Sample)
COI_SN$Relationship<-"COI-SN"
Trad_COI_SN<-Trad%>% filter(Method %in% c("SN"))
Trad_COI_SN$Relationship<-"COI-SN"
# MT----
MT_Sampling_Days<-sampling%>% filter(Method %in% c("MT"))
COI$Sample<-COI$variable
COI_MT<-COI%>% filter(Sample %in% MT_Sampling_Days$Sample)
COI_MT$Relationship<-"COI-MT"
Trad_COI_MT<-Trad%>% filter(Method %in% c("MT"))
Trad_COI_MT$Relationship<-"COI-MT"


# GN----
GN_Sampling_Days<-sampling%>% filter(Method %in% c("GN"))
COI$Sample<-COI$variable
COI_GN<-COI%>% filter(Sample %in% GN_Sampling_Days$Sample)
COI_GN$Relationship<-"COI-GN"
Trad_COI_GN<-Trad%>% filter(Method %in% c("GN"))
Trad_COI_GN$Relationship<-"COI-GN"





#EF Relationships----
#HN----
HN_Sampling_Days<-sampling%>% filter(Method %in% c("HN"))
EF<- Trad%>%filter(Method %in% c("EF"))
EF$Sample<-EF$Date_site
EF_HN<-EF%>% filter(Sample %in% HN_Sampling_Days$Sample)
EF_HN$Relationship<-"EF-HN"
Trad_EF_HN<-Trad%>% filter(Method %in% c("HN"))
Trad_EF_HN$Relationship<-"EF-HN"
#SN----
SN_Sampling_Days<-sampling%>% filter(Method %in% c("SN"))
EF<- Trad%>%filter(Method %in% c("EF"))
EF$Sample<-EF$Date_site
EF_SN<-EF%>% filter(Sample %in% SN_Sampling_Days$Sample)
EF_SN$Relationship<-"EF-SN"
Trad_EF_SN<-Trad%>% filter(Method %in% c("SN"))
Trad_EF_SN$Relationship<-"EF-SN"
#MT----
MT_Sampling_Days<-sampling%>% filter(Method %in% c("MT"))
EF<- Trad%>%filter(Method %in% c("EF"))
EF$Sample<-EF$Date_site
EF_MT<-EF%>% filter(Sample %in% MT_Sampling_Days$Sample)
EF_MT$Relationship<-"EF-MT"
Trad_EF_MT<-Trad%>% filter(Method %in% c("MT"))
Trad_EF_MT$Relationship<-"EF-MT"


#GN----
GN_Sampling_Days<-sampling%>% filter(Method %in% c("GN"))
EF<- Trad%>%filter(Method %in% c("EF"))
EF$Sample<-EF$Date_site
EF_GN<-EF%>% filter(Sample %in% GN_Sampling_Days$Sample)
EF_GN$Relationship<-"EF-GN"
Trad_EF_GN<-Trad%>% filter(Method %in% c("GN"))
Trad_EF_GN$Relationship<-"EF-GN"

#HN Relationahips----
#SN----
SN_Sampling_Days<-sampling%>% filter(Method %in% c("SN"))
HN<- Trad%>%filter(Method %in% c("HN"))
HN$Sample<-HN$Date_site
HN_SN<-HN%>% filter(Sample %in% SN_Sampling_Days$Sample)
HN_SN$Relationship<-"HN-SN"
Trad_HN_SN<-Trad%>% filter(Method %in% c("SN"))
Trad_HN_SN$Relationship<-"HN-SN"
#MT----
MT_Sampling_Days<-sampling%>% filter(Method %in% c("MT"))
HN<- Trad%>%filter(Method %in% c("HN"))
HN$Sample<-HN$Date_site
HN_MT<-HN%>% filter(Sample %in% MT_Sampling_Days$Sample)
HN_MT$Relationship<-"HN-MT"
Trad_HN_MT<-Trad%>% filter(Method %in% c("MT"))
Trad_HN_MT$Relationship<-"HN-MT"


#GN----
GN_Sampling_Days<-sampling%>% filter(Method %in% c("GN"))
HN<- Trad%>%filter(Method %in% c("HN"))
HN$Sample<-HN$Date_site
HN_GN<-HN%>% filter(Sample %in% GN_Sampling_Days$Sample)
HN_GN$Relationship<-"HN-GN"
Trad_HN_GN<-Trad%>% filter(Method %in% c("GN"))
Trad_HN_GN$Relationship<-"HN-GN"





# SN Relationships ----
#MT----
MT_Sampling_Days<-sampling%>% filter(Method %in% c("MT"))
SN<- Trad%>%filter(Method %in% c("SN"))
SN$Sample<-SN$Date_site
SN_MT<-SN%>% filter(Sample %in% MT_Sampling_Days$Sample)
SN_MT$Relationship<-"SN-MT"
Trad_SN_MT<-Trad%>% filter(Method %in% c("MT"))
Trad_SN_MT$Relationship<-"SN-MT"


#GN----
GN_Sampling_Days<-sampling%>% filter(Method %in% c("GN"))
SN<- Trad%>%filter(Method %in% c("SN"))
SN$Sample<-SN$Date_site
SN_GN<-SN%>% filter(Sample %in% GN_Sampling_Days$Sample)
SN_GN$Relationship<-"SN-GN"
Trad_SN_GN<-Trad%>% filter(Method %in% c("GN"))
Trad_SN_GN$Relationship<-"SN-GN"






# MT Relationahips ----
#GN----
GN_Sampling_Days<-sampling%>% filter(Method %in% c("GN"))
MT<- Trad%>%filter(Method %in% c("MT"))
MT$Sample<-MT$Date_site
MT_GN<-MT%>% filter(Sample %in% GN_Sampling_Days$Sample)
MT_GN$Relationship<-"MT-GN"
Trad_MT_GN<-Trad%>% filter(Method %in% c("GN"))
Trad_MT_GN$Relationship<-"MT-GN"







#Combine----
eDNA<-combine(S12_EF,S12_HN, S12_SN,S12_MT,S12_GN,
              COI_EF,COI_HN, COI_SN,COI_MT,COI_GN)
Trad_For_eDNA<-combine(EF_HN, EF_SN,EF_MT,EF_GN,
                       HN_SN,HN_MT,HN_GN, 
                       SN_MT,SN_GN, 
                       MT_GN)
Trad_For_eDNA$variable<-Trad_For_eDNA$Sample
Trad_For_eDNA$value<-Trad_For_eDNA$Count
Trad_For_eDNA<-subset(Trad_For_eDNA, select=c(Species, variable, Sample,Relationship, value))

eDNA<-combine(eDNA, Trad_For_eDNA)


Trad1<-combine(Trad_12S_EF,Trad_12S_HN,Trad_12S_SN,Trad_12S_MT,Trad_12S_GN,
              Trad_COI_EF,Trad_COI_HN,Trad_COI_SN,Trad_COI_MT,Trad_COI_GN,
              Trad_EF_HN,Trad_EF_SN,Trad_EF_MT,Trad_EF_GN, 
              Trad_HN_SN,Trad_HN_MT,Trad_HN_GN, 
              Trad_SN_MT,Trad_SN_GN, 
              Trad_MT_GN)

eDNA$Value<-eDNA$value
eDNA$Method<-eDNA$Relationship
eDNA<-subset(eDNA, select=c(Species, Sample,Method, Value))
Trad1$Sample<-Trad1$Date_site
Trad1$Value<-Trad1$Count
Trad1$Method<-Trad1$Relationship
Trad<-subset(Trad1, select=c(Species, Sample,Method, Value))


write_xlsx()
write_xlsx(Trad, "Data/Trad.xlsx")
write_xlsx(eDNA, "Data/eDNA.xlsx")

