library(plyr)
library(readxl)
library(tidyr)
library(dplyr)
library(flextable)
library(officer)

## set working directory
setwd("...")

########### 
##### READ IN AND PREPARE DATA
########### 
data<-read_excel("LakeChangeData.xlsx", sheet = "Sorted by Location")

data$permafrost<-recode_factor(data$permafrost, continuous="Continuous", discontinuous="Discontinuous")
data<-data %>% drop_na(c("net")) 

##### add start and end dates
data<-data %>%
  separate(Years, 
           c("Start", "End"),
           sep = "[^0-9]")


data$Start<-as.numeric(data$Start)
data$End<-as.numeric(data$End)


#### how many studies?
length(unique(data$Study))
length(data$Study)

#### how many negative/positive?
dis<-data[data$permafrost=="Discontinuous",]
cont<-data[data$permafrost=="Continuous",]

nrow(dis[dis$net=="negative",])/nrow(dis)

nrow(cont[cont$net=="negative",])/nrow(cont)
nrow(cont[cont$net=="positive",])/nrow(cont)
nrow(cont[cont$net=="no trend",])/nrow(cont)


#### average study length
data$totaltime<-data$End-data$Start
mean(data$totaltime)
min(data$totaltime)
max(data$totaltime)

### most common starting decade
data$decade<-floor(data$Start/10)*(10)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(data$decade)
nrow(data[data$decade=='1970',])/nrow(data)

## studies beginning before 1970
nrow(data[data$decade<1970,])/nrow(data)
nrow(data[data$decade>1990,])/nrow(data)

## earliest start year
min(data$Start)
max(data$Start)

## latest end year
max(data$End)
min(data$End)

## number of studies based on 2 time points
nrow(data[data$points=="two",])/nrow(data)
nrow(data[data$points=="many",])/nrow(data)

### studies in TK lakes
nrow(data[data$overburden=='thick',])/nrow(data)


#### how many in each region?
### West siberian lowlands
nrow(data[data$region_subset=="WSL",])
### Alaska
nrow(data[data$region=="Alaska",])


##########
### SUMMARY TABLE
##########

data$continent<-ifelse(data$region=="Russia", 'Eurasia', 'North America')

T1<-data %>%
  group_by(AerialExtent, permafrost) %>%
  dplyr::summarise(count=n()) %>% 
  arrange(desc(permafrost))%>%
  spread(permafrost,count) %>%
  arrange(desc(AerialExtent))%>%
  mutate(AerialExtent= 
           replace(AerialExtent, AerialExtent == 'large', 'Large'))%>%
  mutate(AerialExtent= 
           replace(AerialExtent, AerialExtent == 'medium', 'Medium'))%>%
  mutate(AerialExtent= 
           replace(AerialExtent, AerialExtent == 'small', 'Small'))%>%     
  mutate(newcol = "Spatial extent")%>%
  relocate(newcol)%>%
  setNames( c(" "," ", "Continuous", "Discontinuous") )

T2<-data %>% 
  drop_na(`Landsat/Highres`)%>%
  group_by(`Landsat/Highres`, permafrost) %>%
  mutate(`Landsat/Highres`= 
           replace(`Landsat/Highres`, `Landsat/Highres` == 'HighRes', 'High resolution'))%>%
  mutate(`Landsat/Highres`= 
           replace(`Landsat/Highres`, `Landsat/Highres` == 'Fieldwork', 'Field measurements'))%>%      
  dplyr::summarise(count=n())%>% 
  spread(permafrost,count) %>%
  mutate(across(where(is.numeric), coalesce, 0))%>%
  mutate(newcol = "Method")%>%
  relocate(newcol)%>%
  setNames( c(" "," ", "Continuous", "Discontinuous") )


T3<-data %>%
  group_by(continent, permafrost) %>%
  dplyr::summarise(count=n())%>% 
  spread(permafrost,count) %>%
  mutate(newcol = "Location")%>%
  relocate(newcol)%>%
  setNames( c(" "," ", "Continuous", "Discontinuous") )

T4<-data %>%
  group_by(points, permafrost) %>%
  dplyr::summarise(count=n())%>% 
  spread(permafrost,count) %>%
  arrange(match(points, c("two", "few", "many")))%>%
  mutate(points= 
           replace(points, points == 'two', 'Two'))%>%
  mutate(points= 
           replace(points, points == 'few', 'Few'))%>%
  mutate(points= 
           replace(points, points == 'many', 'Many'))%>% 
  mutate(newcol = "Time points")%>%
  relocate(newcol)%>%
  setNames( c(" "," ", "Continuous", "Discontinuous") )




tabledata<-rbind(T1,T2,T3,  T4)
tabledata$total<-round((tabledata$Discontinuous + tabledata$Continuous)/length(data$Study)*100,0)
colnames(tabledata)<-c("var_group", " ", "Continuous (n)",  "Discontinuous (n)","Total (%)")


tib <- as_grouped_data(x = tabledata, groups = c("var_group"), columns = NULL)
finaltable<-tib %>% 
  as_flextable( ) %>%
  flextable::compose(
    i = ~ !is.na(var_group), # when var_group not NA
    j = " ", # on column "var"
    value = as_paragraph(as_chunk(var_group))) %>%
  bg(i = ~ !is.na(var_group), bg = "#DCDCDC", part = "body") %>%
  bold(i = ~ !is.na(var_group), bold = TRUE) %>% 
  bold(i = 1, bold = TRUE, part='header') %>% 
  width( width = 2.5) %>% 
  hline(i=1,j=1:2, part="body", border = officer::fp_border( width=1.5) )%>% 
  hline(i=4,j=1:2, part="body", border = officer::fp_border(width=1.5) )%>% 
  hline(i=5,j=1:2, part="body", border = officer::fp_border(width=1.5) )%>% 
  hline(i=9,j=1:2, part="body", border = officer::fp_border(width=1.5) )%>% 
  hline(i=10,j=1:2, part="body", border = officer::fp_border(width=1.5) ) %>%
  hline(i=12,j=1:2, part="body", border = officer::fp_border(width=1.5) )%>%
  hline(i=13,j=1:2, part="body", border = officer::fp_border(width=1.5) ) %>%
  border_outer(border=officer::fp_border(color="black", style='solid', width=1),part="all")%>%
  flextable::fix_border_issues()

### save table as docx
read_docx() %>%                                
  body_add_flextable(value = finaltable) %>%  
  print(target = "SummaryTable.docx") 

