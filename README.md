# eDNA-Metabarcoding-vs-Five-Conventional-Methods
eDNA Metabarcoding vs Five Traditional Methods a True Comparison of Congruency, Consistency, Accuracy and Diversity


# Citation 



# Layout

Data: The data folders contain the data files used in the analysis 

Map: The map folder contains the data and the R script used to develop the map. 

Script: The script folder contains the scripts for running the analyses 

Function: The function folder contains the script for the function used to conduct the RV analysis

# Arrange_And_Conduct_RV_Coefficient()

Overview:

The Arrange_And_Conduct_RV_Coefficient function takes two data frames, puts them into homologous configurations and calculates the RV coefficient and p-values. The Arrange_And_Conduct_RV_Coefficient can also handle subgrouping. So RV analyses are conducted for each subgroup. 

Data frame Structure:


eDNA:
| Species | Sample| Method | Value| 
| :---         |     :---:      |          ---: |:---         



EF:
| Species | Sample| Method | Value| 
| :---         |     :---:      |          ---: |:---   


Function variables:
```
 Arrange_And_Conduct_RV_Coefficient(dataframe1,dataframe2,methods_vector, Subgrouping_Column, Species_Column, Sample_Column, Value_Column)
```


methods_vector= A vector with the names of the subgroupings. For Example:

```
methods <- c("12S-EF", "12S-GN", "12S-HN", "12S-MT", "12S-SN", "COI-EF", "COI-GN", "COI-HN", "COI-MT", "COI-SN", "EF-GN", "EF-HN", "EF-MT", "EF-SN", "HN-GN", "HN-MT", "HN-SN", "SN-GN", "SN-MT", "MT-GN")
```

Subgrouping_Column = the name of the column where the subgrouping values are present

Species_Column= the name of column of the categorical variable that will be placed vertically 

Sample_Column= the name of the column of the categorical variable that will be placed horizontally

Value_Column= the name of the column of the numerical data


Homologous Configuration
The Arrange_And_Conduct_RV_Coefficient() function automatically arranges data into homologues configurations however in order to make it easier to know what columns to assign to what variables when calling the function we will walk through the structure that is used: 

If our original data frames are structured like: 

| Species | Sample| Method | Value| 
| :---         |     :---:      |          ---: |:---         


and we use the function:

result <- Arrange_And_Conduct_RV_Coefficient(eDNA, Trad, methods, "Method", "Species", "Sample", "Value")

Then the transformed data frame will look like:
| Species | Sample 1| Sample 3 |Sample 3| 
|  :---:         |      :---:     |   :---:   | :---:   
| Species 1 | Value|Value | Value| 
| Species 2 | Value|Value | Value| 
| Species 3 | Value|Value | Value| 
| Species 4 | Value|Value | Value| 

Function useage example:

```
eDNA <- read_excel("Data/eDNA.xlsx")
Trad <- read_excel("Data/Trad.xlsx")
methods <- c("12S-EF", "12S-GN", "12S-HN")

result <- Arrange_And_Conduct_RV_Coefficient(eDNA, Trad, methods, "Method", "Species", "Sample", "Value")

 result 
       Method Observation     P_Value
12S-EF 12S-EF  0.13444733 0.163836164
12S-GN 12S-GN  0.92612567 0.005994006
12S-HN 12S-HN  0.75027183 0.033966034
```

Uses and applications:

The Arrange_And_Conduct_RV_Coefficient() function is versatile and flexible. It can be applied in a variety of situations where one is conducting RV analyses. However, it's strongest attribute is it can conduct multiple RV analyses for different subgroupings at the same time and automatically arranges the data frames in the necessary format.




