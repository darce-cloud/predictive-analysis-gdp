library(dplyr)
adjusted_savings<-Adjusted_net_savings_of_GNI_ %>%
  select(Country,"2012") %>%
  rename(adjusted_savings="2012")
exportsimports_of_GDP<-Exports_and_imports_of_GDP_ %>%
  select(Country, "2012") %>%
  rename(GDP_imports_exports="2012")
GDP_PPP<-Gross_domestic_product_GDP_per_capita_2011_PPP_%>%
  select(Country, "2012")%>%
  rename(GDP_per_capita="2012")
Laborforce_participation<-Labour_force_participation_rate_ages_15_and_older_%>%
  select(Country,"2012")%>%
  rename(Laborparticipation="2012")
Life_expectancy<-Life_expectancy_Index%>%
  select(Country,"2012")%>%
  rename(Life_expctancy_index="2012")
Prison_Population<-Prison_population_per_100_000_people_%>%
  select(Country,"2012")%>%
  rename(PrisonPop_100kpeople="2012")
Youth_not_participating<-Youth_not_in_school_or_employment_ages_15_24_%>%
  select(Country,"2012")%>%
  rename(youth_not_in_school_or_employment="2012")
Education_Index<-Education_Index%>%
  select(Country, "2012")%>%
  rename(Edu_Index="2012")

complete_data<-adjusted_savings%>% #joining datasets#
  left_join(Education_Index, by="Country")%>%
  left_join(exportsimports_of_GDP,by="Country")%>%
  left_join(GDP_PPP,by="Country")%>%
  left_join(Laborforce_participation,by="Country")%>%
  left_join(Life_expectancy,by="Country")%>%
  left_join(Prison_Population,by="Country")%>%
  left_join(Youth_not_participating,by="Country")

complete_data[complete_data==".."]<-NA

complete_data<-complete_data%>%
  mutate(GDP_per_capita=as.numeric(GDP_per_capita),
         Edu_Index=as.numeric(Edu_Index),
         GDP_imports_exports=as.numeric(GDP_imports_exports),
         Laborparticipation=as.numeric(Laborparticipation),
         Life_expctancy_index=as.numeric(Life_expctancy_index),
         PrisonPop_100kpeople=as.numeric(PrisonPop_100kpeople),
         adjusted_savings=as.numeric(adjusted_savings),
         youth_not_in_school_or_employment=as.numeric(youth_not_in_school_or_employment))

government_type<-dpi2012%>% ##importing categorical data##
  filter(year==2012)%>%
  select(countryname,system)

government_type[government_type=="-999"]<-NA ##changing label for unreported data##

##renaming column name to match complete_data##
colnames(government_type)
 names(government_type)[names(government_type)=="countryname"]<-"Country" 
 
num_cat_data<-complete_data%>%
  left_join(government_type,by="Country")
  
rm(Adjusted_net_savings_of_GNI_,adjusted_savings,dpi2012,Education_Index,Exports_and_imports_of_GDP_,
   exportsimports_of_GDP,GDP_PPP,Gross_domestic_product_GDP_per_capita_2011_PPP_,Laborforce_participation,
   Labour_force_participation_rate_ages_15_and_older_,Life_expectancy,Life_expectancy_Index,
   Prison_Population,Prison_population_per_100_000_people_,Youth_not_participating,
   Youth_not_in_school_or_employment_ages_15_24_) ##clear environment of objects I no longer need##

##saving TIDY data##
write.table(num_cat_data,file="NumericalCategoricalDataset.csv")
write.table(complete_data,file="Complete_Data.csv")

##exploratory analysis##
GDPPC<-complete_data$GDP_per_capita
    hist(GDPPC)
    
ggplot(data=government_type)+
  geom_bar(mapping=aes(x=system))

LaborForceParticipation<-complete_data$Laborparticipation
hist(LaborForceParticipation)

##creating testing,training sets##
library(caret)
set.seed(10)
num_cat_data <- na.omit(num_cat_data)
M1.1_partition_vector <- createDataPartition(num_cat_data$Edu_Index, p = .8, list = FALSE)                                     
M1.1_Training <- num_cat_data[M1.1_partition_vector,]
M1.1_Test <- num_cat_data[-M1.1_partition_vector,]

##validation##
##first model##
model1<-lm(GDP_per_capita~adjusted_savings+Edu_Index+GDP_imports_exports+
             Laborparticipation+Life_expctancy_index+PrisonPop_100kpeople+
             youth_not_in_school_or_employment,data = M1.1_Training)
summary(model1)
plot(model1)
Model1_predictionstrain<-predict(model1, M1.1_Training)
MSE(Model1_predictionstrain, M1.1_Training$GDP_per_capita)

Model1_predictions_test<-predict(model1, M1.1_Test)
MSE(Model1_predictions_test, M1.1_Test$GDP_per_capita)

mean(Model1_predictions_test == M1.1_Test$GDP_per_capita)

##second model##
model2<-lm(GDP_per_capita~Edu_Index+youth_not_in_school_or_employment,
           data=M1.1_Training)
summary(model2)
plot(model2)
Model2_predictionstrain<-predict(model2,M1.1_Training)
MSE(Model2_predictionstrain,M1.1_Training$GDP_per_capita)

Model2predictions_test<-predict(model2,M1.1_Test)
MSE(Model2predictions_test,M1.1_Test$GDP_per_capita)
mean(Model2predictions_test == M1.1_Test$GDP_per_capita)

##third model##
model3<-lm(GDP_per_capita~GDP_imports_exports+Laborparticipation,
           data=M1.1_Training)
summary(model3)
plot(model3)
Model3_predictionstrain<-predict(model3,M1.1_Training)
MSE(Model3_predictionstrain,M1.1_Training$GDP_per_capita)

Model3predictions_test<-predict(model3,M1.1_Test)
MSE(Model3predictions_test,M1.1_Test$GDP_per_capita)
mean(Model3predictions_test==M1.1_Test$GDP_per_capita)

##fourth model##
model4<-lm(GDP_per_capita~adjusted_savings+GDP_imports_exports,
           data=M1.1_Training)
summary(model4)
plot(model4)
Model4_predictionstrain<-predict(model4,M1.1_Training)
MSE(Model4_predictionstrain,M1.1_Training$GDP_per_capita)

Model4predictions_test<-predict(model4,M1.1_Test)
MSE(Model4predictions_test,M1.1_Test$GDP_per_capita)
mean(Model4predictions_test==M1.1_Test$GDP_per_capita)

##fifth model##
model5<-lm(GDP_per_capita~PrisonPop_100kpeople+Laborparticipation,
           data=M1.1_Training)
summary(model5)
plot(model5)
Model5_predictionstrain<-predict(model5,M1.1_Training)
MSE(Model5_predictionstrain,M1.1_Training$GDP_per_capita)

Model5predictions_test<-predict(model5,M1.1_Test)
MSE(Model5predictions_test,M1.1_Test$GDP_per_capita)
mean(Model3predictions_test==M1.1_Test$GDP_per_capita)

##sixth model##
model6<-lm(GDP_per_capita~Edu_Index+PrisonPop_100kpeople,data=M1.1_Training)
summary(model6)
plot(model6)
Model6_predictionstrain<-predict(model6,M1.1_Training)
MSE(Model6_predictionstrain,M1.1_Training$GDP_per_capita)

Model6predictions_test<-predict(model6,M1.1_Test)
MSE(Model6predictions_test,M1.1_Test$GDP_per_capita)
mean(Model6predictions_test==M1.1_Test$GDP_per_capita)

##classification##
##changing from numeric to binomial##
education_index_binomial<-M1.1_Training$Edu_Index[M1.1_Training$Edu_Index>=.6]<-1
education_index_binomial<-M1.1_Training$Edu_Index[M1.1_Training$Edu_Index<.6]<-0

education_index_binomial<-M1.1_Test$Edu_Index[M1.1_Test$Edu_Index>=.6]<-1
education_index_binomial<-M1.1_Test$Edu_Index[M1.1_Test$Edu_Index<.6]<-0
summary(M1.1_Test)

M1.1_Training <- na.omit(M1.1_Training)
M1.1_Test <- na.omit(M1.1_Test)

M1.1_Training$Edu_Index <- as.factor(M1.1_Training$Edu_Index)
M1.1_Test$Edu_Index <- as.factor(M1.1_Test$Edu_Index)

M1.1<- glm(Edu_Index ~ adjusted_savings + GDP_imports_exports + GDP_per_capita +
             Laborparticipation + Life_expctancy_index + PrisonPop_100kpeople + 
             youth_not_in_school_or_employment, data = M1.1_Training, 
           family = binomial("logit"))
summary(M1.1)
signal<-predict(M1.1, M1.1_Test, type = "response")
signal[signal >= .6] <- 1
signal[signal < .6] <- 0
signal <- as.factor(signal)
summary(signal)
mean(signal==M1.1_Test$Edu_Index)
table(actual = M1.1_Test$Edu_Index, predicted = signal)

M2.2<-glm(Edu_Index ~ adjusted_savings + GDP_imports_exports + GDP_per_capita +
            Laborparticipation + PrisonPop_100kpeople + 
            youth_not_in_school_or_employment, data = M1.1_Training, 
          family = binomial("logit"))
summary(M2.2)
signal2<-predict(M2.2, M1.1_Test)
signal2[signal2 >= .6] <- 1
signal2[signal2 < .6] <- 0
signal2 <- as.factor(signal2)
summary(signal2)
mean(signal2==M1.1_Test$Edu_Index)
table(actual = M1.1_Test$Edu_Index, predicted = signal2)

M3.3<-glm(Edu_Index ~ adjusted_savings + GDP_imports_exports + GDP_per_capita +
            Laborparticipation + PrisonPop_100kpeople + 
            youth_not_in_school_or_employment, data = M1.1_Training, 
          family = binomial("logit"))
summary(M3.3)
signal3<-predict(M3.3, M1.1_Test, type = "response")
signal3[signal3 >= .6] <- 1
signal3[signal3 < .6] <- 0
signal3 <- as.factor(signal3)
summary(signal3)
mean(signal3==M1.1_Test$Edu_Index)
table(actual = M1.1_Test$Edu_Index, predicted = signal3)

M4.4<-glm(Edu_Index ~ adjusted_savings + GDP_imports_exports +
            Laborparticipation + PrisonPop_100kpeople + 
            youth_not_in_school_or_employment, data = M1.1_Training, 
          family = binomial("logit"))
summary(M4.4)
signal4<-predict(M4.4, M1.1_Test, type = "response")
signal4[signal4 >= .6] <- 1
signal4[signal4 < .6] <- 0
signal4 <- as.factor(signal4)
summary(signal4)
mean(signal4==M1.1_Test$Edu_Index)
table(actual = M1.1_Test$Edu_Index, predicted = signal4)

M5.5<-glm(Edu_Index ~ GDP_imports_exports +
            Laborparticipation + PrisonPop_100kpeople + 
            youth_not_in_school_or_employment, data = M1.1_Training, 
          family = binomial("logit"))
summary(M5.5)
signal5<-predict(M5.5, M1.1_Test, type = "response")
signal5[signal5 >= .6] <- 1
signal5[signal5 < .6] <- 0
signal5 <- as.factor(signal5)
levels(signal5) <- c(1,0)
summary(signal5)
mean(signal5==M1.1_Test$Edu_Index)

M6.6<-glm(Edu_Index ~ Laborparticipation + PrisonPop_100kpeople + 
            youth_not_in_school_or_employment, data = M1.1_Training, 
          family = binomial("logit"))
summary(M6.6)
signal6<-predict(M6.6, M1.1_Test, type = "response")
signal6[signal6 >= .6] <- 1
signal6[signal6 < .6] <- 0
signal6 <- as.factor(signal6)
levels(signal6) <- c(1,0)
summary(signal6)
mean(signal6==M1.1_Test$Edu_Index)
