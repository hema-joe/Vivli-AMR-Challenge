---
#title: "Vivli 2"
#author: "Hema J"
#date: "2023-06-27"
#output:
  #word_document: default
#pdf_document: default
#html_document: default
---
  ## R Markdown
  
  
 
install.packages("tidyverse")

library(tidyverse)

#Loading Data

antibiotics<-read.csv("C:/Users/conta/OneDrive/Desktop/Vivli AMR/07. Vivli AMR Surveillance/2023_06_15 atlas_antibiotics.csv")


#A Glance at Data; Observations and Variables and their characteristics

head(antibiotics)

tail(antibiotics)

names(antibiotics)

summary(antibiotics)

str(antibiotics)


#Number of Rows(Observations)

nrow(antibiotics)


#Number of Columns (Variables)

ncol(antibiotics)


#Data from low and middle income countries (LMIC)

antibiotics_lmic <- antibiotics %>% filter( Country %in% c("Argentina",
                                                           "Cameroon",
                                                           "Guatemala",
                                                           "Jordan", "Namibia",
                                                           "Ukraine",
                                                           "Honduras", "Kenya",
                                                           "Malawi", "Pakistan",
                                                           "Malaysia", "Panama",
                                                           "Thailand",
                                                           "Dominic Republic",
                                                           "Ivory Coast",
                                                           "Mauritius",
                                                           "Nicaragua",
                                                           "Philippines",
                                                           "South Africa",
                                                           "Tunisia",
                                                           "Venezuela",
                                                           "Colombia",
                                                           "Egypt", "Ghana",
                                                           "India", "Jamaica",
                                                           "Mexico", "Nigeria",
                                                           "Turkey", "Vietnam",
                                                           "Costa Rica",
                                                           "El Salvador",
                                                           "Indonesia",
                                                           "Lebabon", "Morocco",
                                                           "Serbia", "Uganda"))

antibiotics_lmic 

#Extracting Carbapenems Data

carbapenem <- antibiotics %>% select (c(Study, Species, Country,
                                        Gender, Age.Group, Speciality, Source,
                                        In...Out.Patient,Year, Phenotype,
                                        Imipenem, Imipenem_I,
                                        Meropenem, Meropenem_I,
                                        Meropenem.vaborbactam,
                                        Meropenem.vaborbactam_I,
                                        Doripenem, Doripenem_I,
                                        Ertapenem, Ertapenem_I, ))

#A little Glance

carbapenem

summary(carbapenem)

str(carbapenem)

ncol(carbapenem)

nrow(carbapenem)


#Extracting Meropenems only from Carbapenems

meropenem <- carbapenem %>% select (c(Study, Species, Country, Gender,
                                      Age.Group, Speciality, Source,
                                      In...Out.Patient,Year, Phenotype,
                                      Meropenem, Meropenem_I ))

meropenem


#And a Glance again

summary (meropenem)

str(meropenem)

names(meropenem) #to see the variables present

nrow(meropenem)

ncol(meropenem)


#What Countries are represented?
 
meropenem %>% select(Country)

sort(unique(meropenem$Country))


#From developed countries

meropenem_developed <- meropenem %>% filter( Country %in% c("Australia",
                                                            "Austria",
                                                            "Belgium", "Brazil",
                                                            "Bulgaria",
                                                            "Canada", "Chile",
                                                            "China", "Croatia",
                                                            "Czech Republic",
                                                            "Denmark",
                                                            "Estonia",
                                                            "Finland", "France",
                                                            "Germany", "Greece",
                                                            "Hungary",
                                                            "Ireland", "Israel",
                                                            "Italy", "Japan",
                                                            "Korea,South",
                                                            "Kuwait",
                                                            "Latvia",
                                                            "Lebanon",
                                                            "Lithuania",
                                                            "Netherlands",
                                                            "New Zealand",
                                                            "Norway", "Oman",
                                                            "Poland",
                                                            "Portugal",
                                                            "Puerto Rico",
                                                            "Qatar", "Romania",
                                                            "Russia",
                                                            "Saudi Arabia",
                                                            "Singapore",
                                                            "Slovak Republic",
                                                            "Slovenia", "Spain",
                                                            "Sweden",
                                                            "Switzerland",
                                                            "Taiwan",
                                                            "Thailand",
                                                            "Turkey",
                                                            "United Kingdom",
                                                            "United States"))


meropenem_developed


#Filtering LMIC

meropenem_lmic <- meropenem %>% filter( Country %in% c("Argentina",
                                                       "Cameroon", "Guatemala",
                                                       "Jordan", "Namibia",
                                                       "Ukraine", "Honduras",
                                                       "Kenya", "Malawi",
                                                       "Pakistan", "Malaysia",
                                                       "Panama", "Thailand",
                                                       "Dominic Republic",
                                                       "Ivory Coast",
                                                       "Mauritius", "Nicaragua",
                                                       "Philippines",
                                                       "South Africa",
                                                       "Tunisia", "Venezuela",
                                                       "Colombia", "Egypt",
                                                       "Ghana", "India",
                                                       "Jamaica", "Mexico",
                                                       "Nigeria", "Turkey",
                                                       "Vietnam", "Costa Rica",
                                                       "El Salvador",
                                                       "Indonesia", "Lebabon",
                                                       "Morocco", "Serbia",
                                                       "Uganda"))

meropenem_lmic

#Pseudpmonas Aeruginosa Data

meropenem_psa <- meropenem %>% count(nrow(filter(meropenem,
                                        Species == "Pseudomonas aeruginosa")))

meropenem_psa


#Sorting Countries

sort(unique(meropenem_lmic$Country)) #35 Countries
sort(unique(meropenem_developed$Country))  #47 Countries


#Filtering PSA species

meropenem_developed_psa<- meropenem_developed %>% 
  filter (Species %in% c("Pseudomonas aeruginosa"))

nrow(meropenem_developed_psa)


#Dealing/Focusing with LMIC data

meropenem_lmic_psa<- meropenem_lmic %>%  filter (Species %in%
                                                   c("Pseudomonas aeruginosa"))

meropenem_lmic_psa


nrow(meropenem_lmic_psa)

ncol(meropenem_lmic_psa)


sort(unique(meropenem_lmic_psa$Study))

sort(unique(meropenem_lmic_psa$Gender))

sort(unique(meropenem_lmic_psa$Age.Group))

sort(unique(meropenem_lmic_psa$Speciality))

sort(unique(meropenem_lmic_psa$Source))

sort(unique(meropenem_lmic_psa$In...Out.Patient))

sort(unique(meropenem_lmic_psa$Year))

sort(unique(meropenem_lmic_psa$Phenotype))

sort(unique(meropenem_lmic_psa$Meropenem))



#A Glance on our desirable observation (Test Results)

sort(unique(meropenem_lmic_psa$Meropenem_I))

total_meropenem_I <- nrow(meropenem_lmic_psa)

total_meropenem_I

total_meropenem_I_developed <- nrow(meropenem_developed_psa)

total_meropenem_I_developed

meropenem_lmic_psa %>%  count(Meropenem_I)


#Results per country

results_developed<- meropenem_developed_psa %>%  count(Meropenem_I, Country)

results_developed

results_lmic<- meropenem_lmic_psa %>%  count(Meropenem_I, Country)

results_lmic


#Resistance (Resistant results) Prevalence


#Resistance

resistant_lmic <- meropenem_lmic_psa %>% count(nrow(filter(meropenem_lmic_psa,
                                                  Meropenem_I == "Resistant")))

resistant_developed <- meropenem_developed_psa %>%
      count(nrow(filter(meropenem_developed_psa, Meropenem_I == "Resistant")))

resistant_developed

resistant_lmic


#Prevalence

prevalence_lmic <- (4745/(total_meropenem_I))*100

prevalence_lmic

prevalence_developed <- (14050/(total_meropenem_I_developed))*100

prevalence_developed


#Resistant pattern over the Years

res_year <- meropenem_lmic_psa %>% select(Year, Meropenem_I) %>%
                          arrange(Year) %>% filter(Meropenem_I == "Resistant")

res_year


#Percentage

res_year_per <- res_year %>% filter(Meropenem_I == "Resistant") %>%
                                          group_by(Year) %>% summarise (n=n(),
                        percentage_change = round((n/total_meropenem_I)*100, 2))

res_year_per

plot(percentage_change ~ Year, data = res_year_per)


#Linear Regression Model


lm_model <- lm(percentage_change~Year, data = res_year_per)


summary(lm_model)

#Visualization of the lm model

ggplot(res_year_per, mapping = aes(x = Year, y = percentage_change)) + geom_point() +
           stat_smooth(method = "lm") + ggtitle("Linear Model Fitted to Data")


ggplot(res_year_per, mapping = aes(x = Year, y = percentage_change)) + geom_point() +
  stat_smooth(method = "lm", se=FALSE) + ggtitle("Linear Model Fitted to Data")


#Future Prediction (20 years)

predict(lm_model, data.frame(Year= 2042))

#Percent Increase

res_year_per_increase <- res_year_per %>%
         mutate (percent_increase = 
        (percentage_change- lag(percentage_change))/lag(percentage_change)*100)

res_year_per_increase


#Relationship to other factors

meropenem_lmic_psa %>%  count(Gender)

meropenem_lmic_psa %>%  count(In...Out.Patient)

meropenem_lmic_psa %>%  count(Age.Group)

#1.Gender

res_male <- meropenem_lmic_psa %>%
                               count(nrow(filter(meropenem_lmic_psa,
                               Gender == "Male" & Meropenem_I == "Resistant")))

res_male

res_female <- meropenem_lmic_psa %>%
                            count(nrow(filter(meropenem_lmic_psa,
                            Gender == "Female" & Meropenem_I == "Resistant")))

res_female

total_male <- meropenem_lmic_psa %>%
                       count(nrow(filter(meropenem_lmic_psa, Gender== "Male")))

total_male

total_female <- meropenem_lmic_psa %>%
                     count(nrow(filter(meropenem_lmic_psa, Gender== "Female")))

total_female

per_male <- (res_male/total_male)*100

per_male

per_female <- (res_female/total_female)*100

per_female


Gen <- c("male", "female")
Gender_Percent = c(27, 24)
Gender_Per <- data.frame(Gen, Gender_Percent)

Gender_Per

#Visuals

barplot(Gender_Per$Gender_Percent, names.arg = Gender_Per$Gen, 
        xlab = "Gender", ylab = "Percentage", 
        main = "Gender Percentage")

pie(Gender_Per$Gender_Percent, labels = Gender_Per$Gen,
    main = "Gender Percentage")

ggplot(Gender_Per, aes(x = "", y = Gender_Percent, fill = Gen)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Gender Percentage")

#2.Inpatient vs Outpatient

total_In <- meropenem_lmic_psa %>%
        count(nrow(filter(meropenem_lmic_psa, In...Out.Patient =="Inpatient" )))
total_Out <- meropenem_lmic_psa %>%
       count(nrow(filter(meropenem_lmic_psa, In...Out.Patient =="Outpatient" )))
res_In <- meropenem_lmic_psa %>%
                count(nrow(filter(meropenem_lmic_psa,
                 In...Out.Patient == "Inpatient" & Meropenem_I == "Resistant")))
res_Out <- meropenem_lmic_psa %>%
              count(nrow(filter(meropenem_lmic_psa,
                In...Out.Patient == "Outpatient" & Meropenem_I == "Resistant")))
per_In <- (res_In/total_In)*100
per_Out<- (res_Out/total_Out)*100

total_In

total_Out

res_In

res_Out

per_In

per_Out

In_Out <- c("Inpatient", "Outpatient")
In_Out_Percent = c(26.3, 18.4)
In_Out_Per <- data.frame(In_Out, In_Out_Percent)


#Visuals

pie(In_Out_Per$In_Out_Percent, 
    labels = paste(In_Out_Per$In_Out, "\n", In_Out_Per$In_Out_Percent, "%"),
    main = "Inpatient vs Outpatient Percentage")
legend("topright", legend = paste(In_Out_Per$In_Out, " (", In_Out_Per$In_Out_Percent, "%)"), 
       cex = 0.8, bty = "n")




#3.Age Groups

total_0_to_2 <- meropenem_lmic_psa %>%
          count(nrow(filter(meropenem_lmic_psa, Age.Group =="0 to 2 Years" )))
total_3_12 <- meropenem_lmic_psa %>%
         count(nrow(filter(meropenem_lmic_psa, Age.Group =="3 to 12 Years" )))
total_13_18 <- meropenem_lmic_psa %>%
        count(nrow(filter(meropenem_lmic_psa, Age.Group =="13 to 18 Years" )))
total_19_64 <- meropenem_lmic_psa %>%
        count(nrow(filter(meropenem_lmic_psa, Age.Group =="19 to 64 Years" )))
total_65_84 <- meropenem_lmic_psa %>%
        count(nrow(filter(meropenem_lmic_psa, Age.Group =="65 to 84 Years" )))
total_85_up <- meropenem_lmic_psa %>%
           count(nrow(filter(meropenem_lmic_psa, Age.Group =="85 and Over" )))
total_Unknown <- meropenem_lmic_psa %>%
               count(nrow(filter(meropenem_lmic_psa, Age.Group =="Unknown" )))


res_0_to_2 <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
                    Age.Group == "0 to 2 Years" & Meropenem_I == "Resistant")))
res_3_12 <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
                  Age.Group == "3 to 12 Years" & Meropenem_I == "Resistant")))
res_13_18 <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
                  Age.Group == "13 to 18 Years" & Meropenem_I == "Resistant")))
res_19_64 <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
                  Age.Group == "19 to 64 Years" & Meropenem_I == "Resistant")))
res_65_84 <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
                  Age.Group == "65 to 84 Years" & Meropenem_I == "Resistant")))
res_85_up <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
                     Age.Group == "85 and Over" & Meropenem_I == "Resistant")))
res_Unknown <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
                         Age.Group == "Unknown" & Meropenem_I == "Resistant")))


per_0_to_2 <- (res_0_to_2/total_0_to_2)*100
per_3_12 <- (res_3_12/total_3_12)*100
per_13_18 <- (res_13_18/total_13_18)*100
per_19_64 <- (res_19_64/total_19_64)*100
per_65_84 <- (res_65_84/total_65_84)*100
per_85_up <- (res_85_up/total_85_up)*100
per_Unknown <- (res_Unknown/total_Unknown)*100


total_0_to_2

res_0_to_2

per_0_to_2

total_3_12

res_3_12

per_3_12

total_13_18

res_13_18

per_13_18

total_19_64

res_19_64

per_19_64

total_65_84

res_65_84

per_65_84

total_85_up

res_85_up

per_85_up

total_Unknown

res_Unknown

per_Unknown

per_0_to_2 
per_3_12 
per_13_18 
per_19_64 
per_65_84 
per_85_up 
per_Unknown 

library(plotly)

Age_group <- c("0 to 2", "3 to 12","13 to 18","16 to 64","65 to 84","84 and Above")
Age_Percent = c(23.6, 19.5, 28.0, 27.0, 26.2, 23.32 )
Age_Per <- data.frame(Age_group, Age_Percent)

Age_Per

# Create the bar plot using Plotly
plot_ly(data = Age_Per, x = ~Age_group, y = ~Age_Percent, type = "bar") %>%
  layout(title = "Age Group Percentage",
         xaxis = list(title = "Age Group"),
         yaxis = list(title = "Percentage"))


#4.Speciality

meropenem_lmic_psa %>%  count(Speciality)

total_Clinic <- meropenem_lmic_psa %>%
      count(nrow(filter(meropenem_lmic_psa, Speciality =="Clinic / Office" )))
total_Emergency <- meropenem_lmic_psa %>%
        count(nrow(filter(meropenem_lmic_psa, Speciality =="Emergency Room" )))
total_General_ICU <- meropenem_lmic_psa %>%
count(nrow(filter(meropenem_lmic_psa, Speciality =="General Unspecified ICU" )))
total_None <- meropenem_lmic_psa %>%
           count(nrow(filter(meropenem_lmic_psa, Speciality =="None Given" )))
total_Nursing_Rehab <- meropenem_lmic_psa %>%
count(nrow(filter(meropenem_lmic_psa, Speciality =="Nursing Home / Rehab" )))
total_Other <- meropenem_lmic_psa %>%
                count(nrow(filter(meropenem_lmic_psa, Speciality =="Other" )))
total_Pediatric_General <- meropenem_lmic_psa %>%
    count(nrow(filter(meropenem_lmic_psa, Speciality =="Pediatric General" )))
total_Pediatric_ICU <- meropenem_lmic_psa %>%
        count(nrow(filter(meropenem_lmic_psa, Speciality =="Pediatric ICU" )))
total_Medicine_General <- meropenem_lmic_psa %>%
     count(nrow(filter(meropenem_lmic_psa, Speciality =="Medicine General" )))
total_Medicine_ICU <- meropenem_lmic_psa %>%
         count(nrow(filter(meropenem_lmic_psa, Speciality =="Medicine ICU" )))

res_Clinic <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
                          Speciality == "Clinic" & Meropenem_I == "Resistant")))
res_Emergency <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
                  Speciality == "Emergency Room" & Meropenem_I == "Resistant")))
res_General_ICU <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
         Speciality == "General Unspecified ICU" & Meropenem_I == "Resistant")))
res_None <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
                      Speciality == "None Given" & Meropenem_I == "Resistant")))
res_Nursing_Rehab <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
            Speciality == "Nursing Home / Rehab" & Meropenem_I == "Resistant")))
res_Other <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
                          Speciality == "Clinic" & Meropenem_I == "Resistant")))
res_Pediatric_General <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
               Speciality == "Pediatric General" & Meropenem_I == "Resistant")))
res_Pediatric_ICU <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
                   Speciality == "Pediatric ICU" & Meropenem_I == "Resistant")))
res_Medicine_General <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
                Speciality == "Medicine General" & Meropenem_I == "Resistant")))
res_Medicine_ICU <- meropenem_lmic_psa %>%
  count(nrow(filter(meropenem_lmic_psa,
                    Speciality == "Medicine ICU" & Meropenem_I == "Resistant")))


per_Clinic <- (res_Clinic/total_Clinic)*100
per_Emergency <- (res_Emergency/total_Emergency)*100
per_General_ICU <- (res_General_ICU/total_General_ICU)*100
per_None <- (res_None/total_None)*100
per_Nursing_Rehab <- (res_Nursing_Rehab/total_Nursing_Rehab)*100
per_Other <- (res_Other/total_Other)*100
per_Pediatric_General <- (res_Pediatric_General/total_Pediatric_General)*100
per_Pediatric_ICU <- (res_Pediatric_ICU/total_Pediatric_ICU)*100
per_Medicine_General <- (res_Medicine_General/total_Medicine_General)*100
per_Medicine_ICU <- (res_Medicine_ICU/total_Medicine_ICU)*100


total_Clinic
res_Clinic
per_Clinic

total_Emergency
res_Emergency
per_Emergency

total_General_ICU
res_General_ICU
per_General_ICU

total_Medicine_General
res_Medicine_General
per_Medicine_General


total_Medicine_ICU
res_Medicine_ICU
per_Medicine_ICU


total_None
res_None
per_None


total_Nursing_Rehab
res_Nursing_Rehab
per_Nursing_Rehab

total_Pediatric_General
res_Pediatric_General
per_Pediatric_General


total_Pediatric_ICU
res_Pediatric_ICU
per_Pediatric_ICU

total_Other
res_Other
per_Other

Speciality_category <- c("Clinic ", "Emergency ","General ICU","None",
                         "Nursing Rehab","Other ", "Pediatric General",
                         "Pediatric ICU", "Medicine General", "Medicine ICU")
Speciality_Percent = c(0, 18.8, 36.4, 25.1, 16.1, 0, 16.1, 30.0, 24.7, 32.4  )
Speciality_Per <- data.frame(Speciality_category, Speciality_Percent)

Speciality_Per

ggplot(Speciality_Per, aes(x = 1, y = Speciality_category)) +
  geom_tile(aes(fill = Speciality_Percent), colour = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  xlab("") + ylab("") +
  theme_minimal() +
  ggtitle("Speciality Resistance") +
  coord_equal()

