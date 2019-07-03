#Packages und Libraries laden
library(readr)
library(tidyverse)

###Begriffserklärung Abkürzungen:----
#DPC = Daily Per Capita
#YPC = Year Per Capita
#M = Male
#F = Female
#indicator_over = "Indicator:Prevalence of overweight among adults, BMI &GreaterEqual (25 (crude estimate) (%) - Age Group:18+  years - Sex:Both sexes (%))"
#indicator_obes = "Indicator:Prevalence of obesity among adults, BMI &GreaterEqual; 30 (age-standardized estimate) (%) - Age Group:18+  years - Sex:Both sexes (%)"

###CSV in Dataframes umwandeln ----
#Caloric_Supply_DPC <- read_csv("daily-per-capita-caloric-supply.csv")
#Exercise <- readxl::read_xlsx("week16_exercise (1).xlsx")
#FatSupply_DPC <- read_csv("daily-per-capita-fat-supply.csv")
#FatSupply_region <- read_csv("fat-supply-by-region.csv")
#Obesity_GenderCompare <- read_csv("obesity-in-men-vs-obesity-in-women.csv")
#Obesity_general <- read_csv("share-of-adults-defined-as-obese.csv") %>% mutate(indicator =`Indicator:Prevalence of obesity among adults, BMI &GreaterEqual; 30 (age-standardized estimate) (%) - Age Group:18+  years - Sex:Both sexes (%)`) %>% 
#  mutate(indicator = str_remove(indicator, ";")) %>%  mutate(indicator_obes = as.numeric(indicator))
#ObesityRegion <- read_csv("prevalence-of-obesity-in-adults-by-region.csv")
#SupplyCalories <- read_csv("daily-per-capita-supply-of-calories-vs-gdp-per-capita.csv")
#SupplyFat <- read_csv("daily-per-capita-fat-supply-vs-gdp-per-capita.csv")
#WeightOver_general <- read_csv("share-of-adults-who-are-overweight.csv") %>% mutate(indicator =`Indicator:Prevalence of overweight among adults, BMI &GreaterEqual (25 (crude estimate) (%) - Age Group:18+  years - Sex:Both sexes (%))`) %>% 
#  mutate(indicator = str_remove(indicator, ";")) %>%  mutate(indicator_over = as.numeric(indicator))
#WeightUnder_vs_CalorieSupply <- read_csv("prevalence-of-undernourishment-vs-daily-supply-of-calories.csv")
#WeightOver_vs_CalorieSupply_M <- read_csv("share-of-adult-men-overweight-or-obese-vs-daily-supply-of-calories.csv")

###Datensätze, die in einem anderen Dataframe gereinigt oder zusammengefasst und danach neu eingelesen wurden ----
#GlobalMortality <- readxl::read_xlsx("global_mortality Kopie.xlsx")
#MeatSupply_PC <- read_csv("meat-supply-per-person.csv")
#MeatConsumption_YPC <- read_csv("per-capita-meat-consumption-by-type-kilograms-per-year.csv")
#WeightCompare_F <- read_csv("share-of-women-defined-as-underweight-healthy-overweight-or-obese.csv")
#WeightOver_M_Categories <- read_csv("men-weight-categories.csv")

###Combining Datasets ----
#Meat_YPC <- MeatConsumption_YPC %>% left_join(MeatSupply_PC)
#UnderOver_gender <- WeightCompare_F %>% left_join(WeightOver_M_Categories)
#prevalence_overweight <- WeightOver_general %>% left_join(Obesity_general, by = c("Entity","Code","Year"))
#Supply <- SupplyFat %>% left_join(SupplyCalories, by = c("Entity","Code","Year","GDP per capita (2011 international-$)"))
#Weight <- WeightOver_vs_CalorieSupply_M %>% left_join((WeightUnder_vs_CalorieSupply))
#weight_new <- UnderOver_fm %>% left_join(Weight)

###Deleting irrelevant columns in GlobalMortality ----
#mortality <- GlobalMortality[,c(-2,-6, -9:-11, -13, -14, -16, -17, -19, -22,-24:-34)]
#prevalence_overweight <- prevalence_overweight[,c(-4, -5, -7, -8)]
#Supply <- Supply[,c(-6)]
#Weight <- Weight[,c(-7,-9)]
#weight_new <- weight_new[,c(-11)]
#obes_region <- ObesityRegion[,c(-2)]
#supply <- supply %>% filter(`Daily caloric supply (kcal/person/day)` != "NA")

###Writing Rds and reading in new Dataframes ----
#write_rds(Meat_YPC,"meatnew.rds")
#write_rds(mortality, "mortality.rds")
#write_rds(UnderOver_gender,"underover_gender.rds")
#write_rds(prevalence_overweight, "prevalence_overweight.rds")
#write_rds(supply,"supply.rds")
#write_rds(weight_new,"weight.rds")
#write_rds(obes_region,"obes_region.rds")

###Neue Datensätze einlesen ----
meat <- read_rds("meatnew.rds")
mortality <- read_rds("mortality.rds")
indicator_weight <- read_rds("prevalence_overweight.rds")
supply <- read_rds("supply.rds")
weight <- read_rds("weight.rds")
obes_region <- read_rds("obes_region.rds")
