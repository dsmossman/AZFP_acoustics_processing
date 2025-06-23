# Author: Delphine Mossman
# Date Created: 22 May 2025
# Date Last Modified: 23 June 2025

# !!!!!!!!!!!!!!! EXTREMELY WORK IN PROGRESS !!!!!!!!!!!!!!!

#####
## Initialization/Setup

rm(list = ls())

library(tidyverse)
library(readxl)
library(ncdf4)
library(R.utils)
library(tcltk)
library(rsample)
library(mgcv)
library(mgcViz)
library(sf)
library(GGally)
library(gridExtra)
library(MASS)

sourceDirectory(
  "H:/dm1679/Code/R Functions",
  modifiedOnly = F
)

figure_dir = "H:/dm1679/Data/Glider Data/Statistics Plots/"

# home_dir = "H:/dm1679/Data/"
home_dir = "C:/Users/Delphine/Box/"

#####
load(paste0(home_dir,"Glider Data/ru39-20230420T1636/Derived Biomass Data/Zooplankton_FTLE_Correlation_Data.rda"))
assign("zoop_data_spring_2023", zoop_data)
rm(zoop_data)
assign("zoop_data_spring_2023_2", zoop_data2)
rm(zoop_data2)

load(paste0(home_dir,"Glider Data/ru39-20231103T1413/Derived Biomass Data/Zooplankton_FTLE_Correlation_Data.rda"))
assign("zoop_data_fall_2023", zoop_data)
rm(zoop_data)
assign("zoop_data_fall_2023_2", zoop_data2)
rm(zoop_data2)

load(paste0(home_dir,"Glider Data/ru39-20240215T1646/Derived Biomass Data/Zooplankton_FTLE_Correlation_Data.rda"))
assign("zoop_data_winter_2024", zoop_data)
rm(zoop_data)
assign("zoop_data_winter_2024_2", zoop_data2)
rm(zoop_data2)

load(paste0(home_dir,"Glider Data/ru39-20240429T1522/Derived Biomass Data/Zooplankton_FTLE_Correlation_Data.rda"))
assign("zoop_data_spring_2024", zoop_data)
rm(zoop_data)
assign("zoop_data_spring_2024_2", zoop_data2)
rm(zoop_data2)

load(paste0(home_dir,"Glider Data/ru43-20240904T1539/Derived Biomass Data/Zooplankton_FTLE_Correlation_Data.rda"))
assign("zoop_data_summer_2024", zoop_data)
rm(zoop_data)
assign("zoop_data_summer_2024_2", zoop_data2)
rm(zoop_data2)

zoop_data_spring_2023$Season = "Spring"
zoop_data_fall_2023$Season = "Fall"
zoop_data_winter_2024$Season = "Winter"
zoop_data_spring_2024$Season = "Spring"
zoop_data_summer_2024$Season = "Summer"

zoop_data_spring_2023_2$Season = "Spring"
zoop_data_fall_2023_2$Season = "Fall"
zoop_data_winter_2024_2$Season = "Winter"
zoop_data_spring_2024_2$Season = "Spring"
zoop_data_summer_2024_2$Season = "Summer"

zoop_data_full = rbind(zoop_data_spring_2023,zoop_data_fall_2023,zoop_data_winter_2024,zoop_data_spring_2024,zoop_data_summer_2024) %>% 
  filter(Abundance > 0) %>%
  arrange(Date)
zoop_data_full$Season = factor(zoop_data_full$Season, levels = c("Spring","Summer","Fall","Winter"), ordered = T)

fname = "H:/dm1679/Data/Glider Data/RMI_Zoop_FTLE_Correlation_Data_Full.rda"
save(zoop_data_full, file = fname)

zoop_data_full_2 = rbind(zoop_data_spring_2023_2,zoop_data_fall_2023_2,zoop_data_winter_2024_2,zoop_data_spring_2024_2,zoop_data_summer_2024_2) %>% 
  filter(D_Int_Abundance > 0) %>%
  arrange(Date)
zoop_data_full_2$Season = factor(zoop_data_full_2$Season, levels = c("Spring","Summer","Fall","Winter"), ordered = T)

fname = "H:/dm1679/Data/Glider Data/RMI_Zoop_FTLE_Correlation_Data_Full_2.rda"
save(zoop_data_full_2, file = fname)

#####

load("H:/dm1679/Data/Glider Data/RMI_Zoop_FTLE_Correlation_Data_Full.rda")

load("H:/dm1679/Data/Glider Data/RMI_Zoop_FTLE_Correlation_Data_Full_2.rda")

## Examine distributions of abundance for each potential explanatory variable

sal_plot = ggplot() +
  geom_point(data = zoop_data_full, aes(x = salinity, y = Abundance, group=Species, color=Species)) +
  scale_color_viridis_d(begin = 0.2) +
  labs(x = "Salinity (PSU)", y = "Concentration (ind/m^3)")
ggsave(sal_plot, filename = paste0(figure_dir, "Concentration_Salinity_Plot.png"))

pH_plot = ggplot() +
  geom_point(data = zoop_data_full, aes(x = pH, y = Abundance, group=Species, color=Species)) +
  scale_color_viridis_d(begin = 0.2) +
  labs(x = "pH", y = "Concentration (ind/m^3)")
ggsave(pH_plot, filename = paste0(figure_dir, "Concentration_pH_Plot.png"))

chlor_plot = ggplot() +
  geom_point(data = zoop_data_full, aes(x = chlorophyll_a, y = Abundance, group=Species, color=Species)) +
  scale_color_viridis_d(begin = 0.2) +
  labs(x = "Chlorophyll a (ug/L)", y = "Concentration (ind/m^3)")
ggsave(chlor_plot, filename = paste0(figure_dir, "Concentration_Chlorophyll_Plot.png"))

FTLE_plot = ggplot() +
  geom_point(data = zoop_data_full, aes(x = FTLE_value, y = Abundance, group=Species, color=Species)) +
  scale_color_viridis_d(begin = 0.2) +
  labs(x = "Calculated FTLE", y = "Concentration (ind/m^3)")
ggsave(FTLE_plot, filename = paste0(figure_dir, "Concentration_FTLE_Plot.png"))

plot(zoop_data_full$temperature, zoop_data_full$Abundance) # multimodal, seasonal?
plot(zoop_data_full$Depth, zoop_data_full$Abundance) # multimodal, seasonal?

temp_plot = ggplot() + 
  geom_point(data = zoop_data_full, aes(x = temperature, y = Abundance, group=Species, color=Species)) +
  scale_color_viridis_d(begin = 0.2) +
  labs(x = "Temperature (degrees C)", y = "Concentration (ind/m^3)")
ggsave(temp_plot, filename = paste0(figure_dir, "Concentration_Temperature_Plot.png"))

temp_plot_seasonal = temp_plot + facet_grid(~Season)
ggsave(temp_plot_seasonal, filename = paste0(figure_dir, "Concentration_Temperature_Plot_Seasonal.png"))
# fall and winter unimodal, spring and summer multimodal

depth_plot = ggplot() + geom_point(data = zoop_data_full, aes(x = Seafloor_Depth, y = Abundance, group=Species, color=Species)) +
  scale_color_viridis_d(begin = 0.2) +
  labs(x = "Seafloor Depth (m)", y = "Concentration (ind/m^3)")
ggsave(depth_plot, filename = paste0(figure_dir, "Concentration_Depth_Plot.png"))

depth_plot_seasonal = depth_plot + facet_grid(~Season)
ggsave(depth_plot_seasonal, filename = paste0(figure_dir, "Concentration_Depth_Plot_Seasonal.png"))
# seasons look more unimodal

## Split datasets by season, model each one?

#####

zoop_data_fall_2023[is.na(zoop_data_fall_2023)] = NA

glm_operators = c("+","-","*",":")
final_BIC = 999999

for(i in 1:2) {
  for (j in 1:length(glm_operators)) {
    for (k in 1:length(glm_operators)) {
      for (m in 1:length(glm_operators)) {
        for (n in 1:length(glm_operators)) {
          for (o in 1:length(glm_operators)) {
            try(
              {
            test_model = eval(parse(
              text = c(
                "glm(Abundance ~ ",
                glm_operators[i],
                "temperature",
                glm_operators[j],
                "salinity",
                glm_operators[k],
                "pH",
                glm_operators[m],
                "chlorophyll_a",
                glm_operators[n],
                "FTLE_value",
                glm_operators[o],
                "Seafloor_Depth,",
                "data = zoop_data_fall_2023,
    family = Gamma(link = \"log\"),
     control=list(maxit = 100))"
              )
            ))
            t_BIC = BIC(test_model)
            if (t_BIC < final_BIC) {
              final_BIC = t_BIC
              final_model = test_model
            } else {
              next
            }
          }
            )
          }
        }
      }
    }
  }
}






model = glm(Abundance ~ temperature * salinity * pH * chlorophyll_a * FTLE_value * Seafloor_Depth, 
            data = zoop_data_fall_2023, 
            family = Gamma(link = "log"),
            control=list(maxit = 100))
zoop_step = stepAIC(model, trace = F)
zoop_step$anova

model2 = glm(Abundance ~ Species + temperature + salinity + pH + chlorophyll_a + FTLE_value:Depth + Seafloor_Depth, data = na.omit(zoop_data_fall_2023), family = Gamma(link = "log"))
zoop_step2 = stepAIC(model2, trace = F)
zoop_step2$anova


zoop_data_winter_2024 = zoop_data_winter_2024 %>% filter(Abundance > 0)

model = glm(Abundance ~ Species * temperature * salinity * pH * chlorophyll_a * FTLE_value * Depth, data = zoop_data_winter_2024, family = Gamma(link = "log"), control = list(maxit = 100))
# does not converge?
zoop_step = stepAIC(model, trace = F)
zoop_step$anova

model2 = glm(Abundance ~ Species + temperature + salinity + pH + chlorophyll_a + FTLE_value + Depth, data = na.omit(zoop_data_winter_2024), family = Gamma(link = "log"))
zoop_step2 = stepAIC(model2, trace = F)
zoop_step2$anova

#####
## Initial GAM model creation loop

# Split data into training and testing datasets
set.seed(420)

zoop_split_su = initial_split(na.omit(zoop_data_summer_2024[zoop_data_summer_2024$Species != "Gelatinous Zooplankton",]), prop = 0.7)
zoop_train_su = training(zoop_split_su)
zoop_test_su = testing(zoop_split_su)

## Iterate through models to find the best one

t_algo = c("s","te","","-")
final_BIC = 999999

for(i in 1:length(t_algo)) {
  for (j in 1:length(t_algo)) {
    for (k in 1:length(t_algo)) {
      for (m in 1:length(t_algo)) {
        for (o in 1:length(t_algo)) {
          for (p in 1:length(t_algo)) {
            test_model = eval(parse(
              text = c(
                "gam(Abundance ~ Species +",
                t_algo[i],
                "(salinity) + ",
                t_algo[j],
                "(temperature) + ",
                t_algo[k],
                "(pH) + ",
                t_algo[m],
                "(chlorophyll_a) + ",
                t_algo[o],
                "(FTLE_value) + ",
                t_algo[p],
                "(Depth),",
                "data = zoop_train_su,
              select = TRUE,
              family = Gamma(link = \"log\"),
              method = \"QNCV\")"
              )
            ))
            t_BIC = BIC(test_model)  # smaller BIC is better
            if (t_BIC < final_BIC) {
              final_BIC = t_BIC
              final_model = test_model
            } else {
              next
            }
          }
        }
      }
    }
  }
}

final_model
BIC(final_model)
summary(final_model)
gam.check(final_model)


zoop_data_spring_all = rbind(zoop_data_spring_2023, zoop_data_spring_2024)

zoop_split_sp = initial_split(na.omit(zoop_data_spring_all[zoop_data_spring_all$Species != "Gelatinous Zooplankton",]), prop = 0.7)
zoop_train_sp = training(zoop_split_sp)
zoop_test_sp = testing(zoop_split_sp)

final_BIC = 999999

for(i in 1:length(t_algo)) {
  for (j in 1:length(t_algo)) {
    for (k in 1:length(t_algo)) {
      for (m in 1:length(t_algo)) {
        for (o in length(t_algo)) {
          for (p in 1:length(t_algo)) {
            test_model = eval(parse(
              text = c(
                "gam(Abundance ~ Species +",
                t_algo[i],
                "(salinity) + ",
                t_algo[j],
                "(temperature) + ",
                t_algo[k],
                "(pH) + ",
                t_algo[m],
                "(chlorophyll_a) + ",
                t_algo[o],
                "(FTLE_value) + ",
                t_algo[p],
                "(Depth),",
                "data = zoop_train_sp,
              select = TRUE,
              family = Gamma(link = \"log\"),
              method = \"QNCV\")"
              )
            ))
            t_BIC = BIC(test_model)  # smaller BIC is better
            if (t_BIC < final_BIC) {
              final_BIC = t_BIC
              final_model = test_model
            } else {
              next
            }
          }
        }
      }
    }
  }
}

final_model_2
BIC(final_model_2)
summary(final_model_2)
gam.check(final_model_2)

#####
##

final_model = gam(log10(Abundance) ~ Season + Species + s(salinity) + te(temperature) + te(pH) + te(chlorophyll_a),
                  data=zoop_train, select=T, method = "QNCV")
BIC(final_model)

getViz(final_model) %>%
  plot(allTerms=T) %>%
  print(pages=1,shade=TRUE)

concurvity(final_model,full = F)

pred_model = predict(final_model, newdata=zoop_test, type = "response", se.fit = TRUE)

actual_response = na.omit(log10(zoop_test$Abundance))
predicted_response = na.omit(pred_model$fit)
Metrics::rmse(actual_response, predicted_response)

zoop_variables = zoop_data_full %>%
  st_drop_geometry() %>%
  ungroup() %>%
  dplyr::select(pH:temperature,
                FTLE_value) %>%
  tidyr::drop_na()
vif_values = spatialRF::vif(zoop_variables)
format(vif_values, scientific = F, digits = 3)
# VIF equal to 1 = variables are not correlated
# VIF between 1 and 5 = variables are moderately correlated 
# VIF greater than 5 = variables are highly correlated

corrplot::corrplot(cor(zoop_variables, method = 'spearman'),
                   method = 'number', 
                   number.cex = 0.7, 
                   type = 'upper')

#####
## Deviance explained by and relative contribution of each model term

final_model_dev_exp = summary(final_model)$dev.expl
final_model_contrib = final_model_dev_exp * 100

model_1 = gam(log10(Abundance) ~ 
                #Season + 
                Species +
                s(salinity) + 
                te(temperature) + 
                te(pH) + 
                te(chlorophyll_a),
              data=zoop_train, select=T, method = "QNCV")
m1_dev_exp = summary(model_1)$dev.expl
m1_contrib = (final_model_dev_exp - m1_dev_exp) * 100
m1_AIC = AIC(model_1)

model_2 = gam(log10(Abundance) ~ 
                Season + 
                Species +
                #s(salinity) + 
                te(temperature) + 
                te(pH) + 
                te(chlorophyll_a),
              data=zoop_train, select=T, method = "QNCV")
m2_dev_exp = summary(model_2)$dev.expl
m2_contrib = (final_model_dev_exp - m2_dev_exp) * 100
m2_AIC = AIC(model_2)

model_3 = gam(log10(Abundance) ~ 
                Season + 
                Species +
                s(salinity) + 
                #te(temperature) + 
                te(pH) + 
                te(chlorophyll_a),
              data=zoop_train, select=T, method = "QNCV")
m3_dev_exp = summary(model_3)$dev.expl
m3_contrib = (final_model_dev_exp - m3_dev_exp) * 100
m3_AIC = AIC(model_3)

model_4 = gam(log10(Abundance) ~ 
                Season + 
                Species +
                s(salinity) + 
                te(temperature) + 
                #te(pH) + 
                te(chlorophyll_a),
              data=zoop_train, select=T, method = "QNCV")
m4_dev_exp = summary(model_4)$dev.expl
m4_contrib = (final_model_dev_exp - m4_dev_exp) * 100
m4_AIC = AIC(model_4)

model_5 = gam(log10(Abundance) ~ 
                Season + 
                Species +
                s(salinity) + 
                te(temperature) + 
                te(pH),
                #te(chlorophyll_a),
              data=zoop_train, select=T, method = "QNCV")
m5_dev_exp = summary(model_5)$dev.expl
m5_contrib = (final_model_dev_exp - m5_dev_exp) * 100
m5_AIC = AIC(model_5)

model_6 = gam(log10(Abundance) ~ 
                Season + 
                # Species +
                s(salinity) + 
                te(temperature) + 
                te(pH) +
                te(chlorophyll_a),
              data=zoop_train, select=T, method = "QNCV")
m6_dev_exp = summary(model_6)$dev.expl
m6_contrib = (final_model_dev_exp - m6_dev_exp) * 100
m6_AIC = AIC(model_6)

contributions = data.frame(
  Term = c("Full Model",
           "Season", 
           "s(salinity)", 
           "te(temperature)", 
           "te(pH)",
           "te(chlorophyll_a)",
           "Species"),
  Deviance_Explained = c(final_model_contrib,
                         m1_contrib,
                         m2_contrib,
                         m3_contrib,
                         m4_contrib,
                         m5_contrib,
                         m6_contrib),
  AIC = c(AIC(final_model),
          m1_AIC,
          m2_AIC,
          m3_AIC,
          m4_AIC,
          m5_AIC,
          m6_AIC))
png(filename = "H:/dm1679/Data/Glider Data/zoop_contrib_table.png", 
    width = 800, height = 600)
grid.table(contributions)
dev.off()

#####
## Testing models with different contributions

final_model_2 = gam(log10(Abundance) ~ Season + Species + s(salinity) + te(pH),
                  data=zoop_train, select=T, method = "QNCV")
BIC(final_model_2)
summary(final_model_2)
gam.check(final_model_2)

pred_model_2 = predict(final_model_2, newdata=zoop_test, type = "response", se.fit = TRUE)
predicted_response_2 = na.omit(pred_model_2$fit)

plot(log10(zoop_test$Abundance), predicted_response_2)

results = data.frame(m1 = Metrics::rmse(actual_response, predicted_response), 
                     m2 = Metrics::rmse(actual_response, predicted_response_2))
results

#####
## Model validation via repeated training/testing

cross_val = mc_cv(zoop_data_full, prop = 0.7, times = 100)
cross_val

##----Monte Carlo Random----##
AIC_values_mc = numeric(length(cross_val$id))
AUC_values_mc = numeric(length(cross_val$id))
RMSE_values_mc = numeric(length(cross_val$id))

for (i in seq_along(cross_val$id)) {
  value = cross_val$id[i]
  resample = cross_val$splits[cross_val$id == value][[1]]
  training_set = analysis(resample)
  testing_set = assessment(resample)
  
  models = gam(log10(Abundance) ~ Season + 
                 Species + s(salinity) + 
                 te(temperature) + te(pH) + 
                 te(chlorophyll_a),
               data=training_set, select=T, 
               method = "QNCV")
  AIC_values_mc[i] = AIC(models)
  
  models_predict = predict(models, newdata=testing_set, type = "response")
  RMSE_values_mc[i] = Metrics::rmse(testing_set$Abundance[complete.cases(models_predict)],models_predict[complete.cases(models_predict)])
  # AUC_values_mc[i] = Metrics::auc(testing_set$Abundance[complete.cases(models_predict)],models_predict[complete.cases(models_predict)])
  # print(i)
}
