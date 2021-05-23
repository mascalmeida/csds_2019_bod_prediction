# Eficiencia ETE

## Clear all ----

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())

## Pacotes ----

if(!require("readxl")) install.packages("readxl") ; library(readxl)
if(!require("tidyverse")) install.packages("tidyverse") ; library(tidyverse)
if(!require("urca")) install.packages("urca") ; library(urca)
if(!require("GGally")) install.packages("GGally") ; library(GGally) 
if(!require("corrplot")) install.packages("corrplot") ; library(corrplot)
if(!require("DMwR")) install.packages("DMwR") ; library(DMwR)
if(!require("caret")) install.packages("caret") ; library(caret)
if(!require("cowplot")) install.packages("cowplot") ; library(cowplot)
if(!require("reshape2")) install.packages("reshape2") ; library(reshape2)
if(!require("e1071")) install.packages("e1071") ; library(e1071)
if(!require("xlsx")) install.packages("xlsx") ; library(xlsx)
if(!require("psych")) install.packages("psych") ; library(psych)

## Banco de dados ----

setwd("C:/Users/lukas/Desktop/compilado/CSDS_2019")
raw_data <- read_excel("data/Dados Brutos.xls", 
                       col_types = c("date", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "date", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric"))

data_in <- raw_data[-seq(nrow(raw_data)-1, nrow(raw_data)),1:15]
data_out <- raw_data[-seq(1,2),17:24]

## Analise exploratoria ----

# NA's
resumo <- raw_data[, -16] %>% 
  select(-Datein, -Dateout) %>% 
  summary()

resumo.in <- data_in %>% 
  select(-Datein) %>% 
  summary()

resumo.out <- data_out %>% 
  select(-Dateout) %>% 
  summary()

write.xlsx(x = resumo, file = "resumo_total.xlsx")
write.xlsx(x = resumo.in, file = "resumo_entradas.xlsx")
write.xlsx(x = resumo.out, file = "resumo_saidas.xlsx")

# Descritiva total
descritiva <- raw_data[, -16] %>% 
  select(-Datein, -Dateout) %>% 
  describe()

descritiva.in <- data_in %>% 
  select(-Datein) %>% 
  describe()

descritiva.out <- data_out %>% 
  select(-Dateout) %>% 
  describe()

write.xlsx(x = descritiva, file = "descritiva_total.xlsx")
write.xlsx(x = descritiva.in, file = "descritiva_entradas.xlsx")
write.xlsx(x = descritiva.out, file = "descritiva_saidas.xlsx")

## Retirando colunas com NA's----

na_s <- raw_data %>% 
  summarise_all(funs(sum(is.na(.))))*(100/nrow(raw_data))

# Exportando tabela com NA(%)
write.xlsx(x = na_s, file = "nas_porcento.xlsx")

na_s <- data_in %>% 
  summarise_all(funs(sum(is.na(.))))*(100/nrow(raw_data))

sel_na <- ifelse(na_s[1,] < 35, 1, NA) %>% 
  as.data.frame()

sel_na <- sel_na[which(colMeans(is.na(sel_na))<=.1)]

sel_na <- colnames(sel_na)

## Modelagem DBOin ----

# Metodos de predicao
# Validacao do modelo

### Metodos de predicao ----

# Florestas aleatorias

### Validacao de modelos ----

# Analise do R² e RMSE
# Metodo grafico

### Configuracoes (ntree, split) ----

N = 128
split = 0.75

### Dados para DBOin----

reduce_in <- data_in %>% 
  select(sel_na) %>% 
  group_by(`BODin (ppm)`) %>% 
  filter(is.na(`BODin (ppm)`) != T) %>% 
  select(-Datein)

#### Modelos - RF1 ----
  
  # Random forest
  
  {
    set.seed(1)
    
    intrain  <- createDataPartition(y = na.omit(reduce_in)$`BODin (ppm)`, p = split, list = FALSE)
    training <- na.omit(reduce_in)[intrain,]
    testing  <- na.omit(reduce_in)[-intrain,]
    
    control  <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 10,
                             savePredictions = T)
    
    rf.1 <- train(`BODin (ppm)` ~ ., 
                  data   = training, 
                  method = "rf",  
                  preProcess = c("center","scale"),
                  ntree = N, 
                  importance = TRUE, 
                  verbose = TRUE,
                  metric  = "RMSE",
                  trControl  = control)
    
    print(rf.1)
  }

### Validacao do modelo----

# Exportanto indices de qualidade do modelo
t.compare <- rf.1$results %>% 
  mutate(Variable = c("DBOin", "DBOin", "DBOin"))
write.xlsx(x = t.compare, file = "modelos_dboin.xlsx")

# Metodo grafico

dboin_pred <- as.numeric(predict(rf.1, newdata = testing))

dboin_test <- testing$`BODin (ppm)`

dboin_compare <- cbind(dboin_test, dboin_pred) %>% 
  as.data.frame()

g.1.compare_in <- dboin_compare %>% 
  ggplot() +
  geom_line(mapping = aes(x = dboin_test, y = dboin_test)) +
  geom_point(mapping = aes(x = dboin_test, y = dboin_pred),
             alpha = 0.6, size = 5) +
  geom_smooth(mapping = aes(x = dboin_test, y = dboin_pred),
              method = "lm") +
  labs(title = "Prediction - Inlet BOD",
       subtitle = "mtry = 8, RMSE = 31.09, R² = 0.514",
       x = "Observed data", y = "Predicted") +
  theme_linedraw(base_size = 26, base_family = "arial")
  
g.1.compare_in

ggsave(filename = "g.1.compare_in.png", 
       g.1.compare_in, width = 13, height = 7, dpi = 450)

### Exibicao do resultado----

# Grafico de importancia
g.importance.1 <- varImp(rf.1)$importance %>%
  as.data.frame() %>%
  arrange(1 - Overall) %>%
  mutate(
    importance = Overall,
    variable = c("COD (input)", "COL",
                 "FR", "COND", "pH",
                 "PULP", "RF", "PAP")
  ) %>%
  select(-Overall) %>%
  ggplot(aes(reorder(variable,-importance), importance)) +
  geom_col(aes(fill = reorder(variable,-importance)), width = 0.65) +
  scale_x_discrete(labels = as.character(sort(
    round(varImp(rf.1)$importance$Overall, 1),
    decreasing = T
  ))) +
  labs(title = "Parameters Importance - Inlet BOD Model",
       x = "",
       y = "Scores",
       fill = "Parameters") +
  theme_linedraw(base_size = 26, base_family = "arial") +
  theme(legend.position = "top")

g.importance.1

# Grafico de predicao (DQOin(obs) x DBOin(pred/obs))

dboin_imp = testing$`CODin (ppm)`

dboin_compare <- cbind(dboin_compare, dboin_imp)

g.pred.1 <- dboin_compare %>% 
  setNames(c("Observed data", "Predicted", "dboin_imp")) %>% 
  melt(id.vars = c("dboin_imp")) %>% 
  ggplot() +
  geom_point(aes(x = dboin_imp, y = value, col = variable), 
             alpha = 0.55, size = 5) +
  labs(title = "Inlet COD vs. Inlet BOD",
       x = "inlet COD (mg/L)", 
       y = "inlet BOD (mg/L)", 
       color = "Legend") +
  theme_linedraw(base_size = 26, base_family = "arial") +
  theme(legend.position = "top")

g.pred.1

ggsave(filename = "g.importance_in.png", 
       g.importance.1, width = 13, height = 7, dpi = 450)
ggsave(filename = "g.predicao_in.png", 
       g.pred.1, width = 13, height = 7, dpi = 450)
### Dados para DQOout----

reduce_out1 <- raw_data %>% 
  select(`CODout (ppm)`, sel_na) %>% 
  group_by(`CODout (ppm)`) %>% 
  filter(is.na(`CODout (ppm)`) != T) %>% 
  select(-Datein)

#### Modelos - RF2 ----

# Random forest

{
  set.seed(1)
  
  intrain  <- createDataPartition(y = na.omit(reduce_out1)$`CODout (ppm)`, p = split, list = FALSE)
  training <- na.omit(reduce_out1)[intrain,]
  testing  <- na.omit(reduce_out1)[-intrain,]
  
  control  <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           savePredictions = T)
  
  rf.2 <- train(`CODout (ppm)` ~ ., 
                data   = training, 
                method = "rf",  
                preProcess = c("center","scale"),
                ntree = N, 
                importance = TRUE, 
                verbose = TRUE,
                metric  = "RMSE",
                trControl  = control)
  
  print(rf.2)
}

### Validacao do modelo----

# Exportanto indices de qualidade do modelo
t.compare.2 <- rf.2$results %>% 
  mutate(Method = c("DQOout", "DQOout", "DQOout"))
write.xlsx(x = t.compare.2, file = "modelos_dqoout.xlsx")

# Metodo grafico

dqoout_pred <- as.numeric(predict(rf.2, newdata = testing))

dqoout_test <- testing$`CODout (ppm)`

dqoout_compare <- cbind(dqoout_test, dqoout_pred) %>% 
  as.data.frame()

g.2.compare_out1 <- dqoout_compare %>% 
  ggplot() +
  geom_line(mapping = aes(x = dqoout_test, y = dqoout_test)) +
  geom_point(mapping = aes(x = dqoout_test, y = dqoout_pred),
             alpha = 0.6, size = 5) +
  geom_smooth(mapping = aes(x = dqoout_test, y = dqoout_pred),
              method = "lm") +
  labs(title = "Prediction - Outlet COD",
       subtitle = "mtry = 2, RMSE = 54.08, R² = 0.406",
       x = "Observed data", y = "Predicted") +
  theme_linedraw(base_size = 26, base_family = "arial")

g.2.compare_out1

ggsave(filename = "g.2.compare_out1.png", 
       g.2.compare_out1, width = 13, height = 7, dpi = 450)

### Exibicao do resultado----

# Grafico de importancia
g.importance.2 <- varImp(rf.2)$importance %>%
  as.data.frame() %>%
  arrange(1 - Overall) %>%
  mutate(
    importance = Overall,
    variable = c(
      "COD (input)",
      "FR",
      "BOD (input)",
      "COL",
      "pH",
      "COND",
      "PULP",
      "PAP",
      "RF"
    )
  ) %>%
  select(-Overall) %>%
  ggplot(aes(reorder(variable,-importance), importance)) +
  geom_col(aes(fill = reorder(variable,-importance)), width = 0.65) +
  scale_x_discrete(labels = as.character(sort(
    round(varImp(rf.2)$importance$Overall, 1),
    decreasing = T
  ))) +
  labs(title = "Parameters Importance - Outlet COD Model",
       x = "",
       y = "Scores",
       fill = "Parameters") +
  theme_linedraw(base_size = 26, base_family = "arial") +
  theme(legend.position = "top")

g.importance.2

# Grafico de predicao (DQOin(obs) x DBOin(pred/obs))

dqoout_imp = testing$`CODin (ppm)`

dqoout_compare <- cbind(dqoout_compare, dqoout_imp)

g.pred.2 <- dqoout_compare %>% 
  setNames(c("Observed data", "Predicted", "dqoout_imp")) %>% 
  melt(id.vars = c("dqoout_imp")) %>% 
  ggplot() +
  geom_point(aes(x = dqoout_imp, y = value, col = variable), 
             alpha = 0.55, size = 5) +
  labs(title = "Inlet COD vs. Outlet COD",
       x = "inlet COD (mg/L)", 
       y = "outlet COD (mg/L)", 
       color = "Legend") +
  theme_linedraw(base_size = 26, base_family = "arial") +
  theme(legend.position = "top")

g.pred.2

ggsave(filename = "g.importance_out1.png", 
       g.importance.2, width = 13, height = 7, dpi = 450)
ggsave(filename = "g.predicao_out1.png", 
       g.pred.2, width = 13, height = 7, dpi = 450)
### Dados originais para DBOout----

reduce_out2 <- raw_data %>% 
  select(`BODout(ppm)`, sel_na) %>% 
  group_by(`BODout(ppm)`) %>% 
  filter(is.na(`BODout(ppm)`) != T) %>% 
  select(-Datein)

#### Modelos - RF3 ----

# Random forest

{
  set.seed(1)
  
  intrain  <- createDataPartition(y = na.omit(reduce_out2)$`BODout(ppm)`, p = split, list = FALSE)
  training <- na.omit(reduce_out2)[intrain,]
  testing  <- na.omit(reduce_out2)[-intrain,]
  
  control  <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           savePredictions = T)
  
  rf.3 <- train(`BODout(ppm)` ~ ., 
                data   = training, 
                method = "rf",  
                preProcess = c("center","scale"),
                ntree = N, 
                importance = TRUE, 
                verbose = TRUE,
                metric  = "RMSE",
                trControl  = control)
  
  print(rf.3)
}

# Exportanto indices de qualidade do modelo
t.compare.3 <- rf.3$results %>% 
  mutate(Method = c("DBOout1", "DBOout1", "DBOout1"))
write.xlsx(x = t.compare.3, file = "modelos_dboout1.xlsx")

### Dados 1 para DBOout----

reduce_out2 <- raw_data %>% 
  select(`BODout(ppm)`,`CODout (ppm)`, sel_na) %>% 
  group_by(`BODout(ppm)`) %>% 
  filter(is.na(`BODout(ppm)`) != T) %>% 
  select(-Datein)

#### Modelos - RF4 ----

# Random forest

{
  set.seed(1)
  
  intrain  <- createDataPartition(y = na.omit(reduce_out2)$`BODout(ppm)`, p = split, list = FALSE)
  training <- na.omit(reduce_out2)[intrain,]
  testing  <- na.omit(reduce_out2)[-intrain,]
  
  control  <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           savePredictions = T)
  
  rf.4 <- train(`BODout(ppm)` ~ ., 
                data   = training, 
                method = "rf",  
                preProcess = c("center","scale"),
                ntree = N, 
                importance = TRUE, 
                verbose = TRUE,
                metric  = "RMSE",
                trControl  = control)
  
  print(rf.4)
}

# Exportanto indices de qualidade do modelo
t.compare.4 <- rf.4$results %>% 
  mutate(Method = c("DBOout2", "DBOout2", "DBOout2"))
write.xlsx(x = t.compare.4, file = "modelos_dboout2.xlsx")

### Grafico 1 de importancia----

g.importance.3 <- varImp(rf.4)$importance %>%
  as.data.frame() %>%
  arrange(1 - Overall) %>%
  mutate(
    importance = Overall,
    variable = c(
      "COD (output)",
      "FR",
      "BOD (input)",
      "COD (input)",
      "COL",
      "COND",
      "PAP",
      "PULP",
      "RF",
      "pH"
    )
  ) %>%
  select(-Overall) %>%
  ggplot(aes(reorder(variable,-importance), importance)) +
  geom_col(aes(fill = reorder(variable,-importance)), width = 0.65) +
  scale_x_discrete(labels = as.character(sort(
    round(varImp(rf.4)$importance$Overall, 1),
    decreasing = T
  ))) +
  labs(title = "Parameters Importance 1 - Outlet BOD Model",
       x = "",
       y = "Scores",
       fill = "Parameters") +
  theme_linedraw(base_size = 26, base_family = "arial") +
  theme(legend.position = "top")

g.importance.3

ggsave(filename = "g1.importance_out2.png", 
       g.importance.3, width = 13, height = 7, dpi = 450)

### Dados 2 para DBOout----

reduce_out3 <- raw_data %>% 
  select(`BODout(ppm)`, sel_na) %>% 
  group_by(`BODout(ppm)`) %>% 
  filter(is.na(`BODout(ppm)`) != T) %>% 
  select(-Datein) %>% 
  na.omit()

dqoout_tot <- as.numeric(predict(rf.2, newdata = reduce_out3))

reduce_out4 <- data.frame(reduce_out3, dqoout_tot)

#### Modelos - RF5 ----

# Random forest

{
  set.seed(1)
  
  intrain  <- createDataPartition(y = na.omit(reduce_out4)$BODout.ppm., p = split, list = FALSE)
  training <- na.omit(reduce_out4)[intrain,]
  testing  <- na.omit(reduce_out4)[-intrain,]
  
  control  <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           savePredictions = T)
  
  rf.5 <- train(BODout.ppm. ~ ., 
                data   = training, 
                method = "rf",  
                preProcess = c("center","scale"),
                ntree = N, 
                importance = TRUE, 
                verbose = TRUE,
                metric  = "RMSE",
                trControl  = control)
  
  print(rf.5)
}

### Validacao do modelo----

# Exportanto indices de qualidade do modelo
t.compare.5 <- rf.5$results %>% 
  mutate(Method = c("DBOout3", "DBOout3", "DBOout3"))
write.xlsx(x = t.compare.5, file = "modelos_dboout3.xlsx")

# Metodo grafico

dboout_pred <- as.numeric(predict(rf.5, newdata = testing))

dboout_test <- testing$BODout.ppm.

dboout_compare <- cbind(dboout_test, dboout_pred) %>% 
  as.data.frame()

g.3.compare_out1 <- dboout_compare %>% 
  ggplot() +
  geom_line(mapping = aes(x = dboout_test, y = dboout_test)) +
  geom_point(mapping = aes(x = dboout_test, y = dboout_pred),
             alpha = 0.6, size = 5) +
  geom_smooth(mapping = aes(x = dboout_test, y = dboout_pred),
              method = "lm") +
  labs(title = "Prediction - Outlet BOD",
       subtitle = "mtry = 6, RMSE = 16.93, R² = 0.493",
       x = "Observed data", y = "Predicted") +
  theme_linedraw(base_size = 26, base_family = "arial")

g.3.compare_out1

ggsave(filename = "g.3.compare_out2.png", 
       g.3.compare_out1, width = 13, height = 7, dpi = 450)

### Exibicao do resultado----

# Grafico de importancia
g.importance.4 <- varImp(rf.5)$importance %>%
  as.data.frame() %>%
  arrange(1 - Overall) %>%
  mutate(
    importance = Overall,
    variable = c(
      "COD (output) predicted",
      "FR",
      "BOD (input)",
      "COD (input)",
      "COL",
      "COND",
      "PAP",
      "PULP",
      "RF",
      "pH"
    )
  ) %>%
  select(-Overall) %>%
  ggplot(aes(reorder(variable,-importance), importance)) +
  geom_col(aes(fill = reorder(variable,-importance)), width = 0.65) +
  scale_x_discrete(labels = as.character(sort(
    round(varImp(rf.5)$importance$Overall, 1),
    decreasing = T
  ))) +
  labs(title = "Parameters Importance 2 - Outlet BOD Model",
       x = "",
       y = "Scores",
       fill = "Parameters") +
  theme_linedraw(base_size = 26, base_family = "arial") +
  theme(legend.position = "top")

g.importance.4

# Grafico de predicao (DQOout(pred) x DBOout(pred/obs))

dboout_imp = testing$dqoout_tot

dboout_compare <- cbind(dboout_compare, dboout_imp)

g.pred.3 <- dboout_compare %>% 
  setNames(c("Observed data", "Predicted", "dboout_imp")) %>% 
  melt(id.vars = c("dboout_imp")) %>% 
  ggplot() +
  geom_point(aes(x = dboout_imp, y = value, col = variable), 
             alpha = 0.55, size = 5) +
  labs(title = "Outlet COD (predicted) vs. Outlet BOD",
       x = "outlet COD (predicted) (mg/L)", 
       y = "outlet BOD (mg/L)", 
       color = "Legend") +
  theme_linedraw(base_size = 26, base_family = "arial") +
  theme(legend.position = "top")

g.pred.3

ggsave(filename = "g2.importance_out2.png", 
       g.importance.4, width = 13, height = 7, dpi = 450)
ggsave(filename = "g.predicao_out2.png", 
       g.pred.3, width = 13, height = 7, dpi = 450)
## Eficiencia----

DBOin_ptot <- as.numeric(predict(rf.1, newdata = reduce_out3))
DBOout_ptot <- as.numeric(predict(rf.5, newdata = reduce_out4))
DBOin_obs <- reduce_out3$`BODin (ppm)`
DBOout_obs <- reduce_out4$BODout.ppm.

ef_data <- data.frame(DBOin_obs, DBOout_obs, DBOin_ptot, DBOout_ptot) %>% 
  mutate(ef_obs = ((DBOin_obs - DBOout_obs)*100)/DBOin_obs,
         ef_pred = ((DBOin_ptot - DBOout_ptot)*100)/DBOin_ptot) %>% 
  mutate(ef_clas_obs = ifelse(ef_obs > 60, "yes", "no"),
         ef_clas_pred = ifelse(ef_pred > 60, "yes", "no"))

descritiva.ef <- ef_data %>% 
  select(ef_pred, ef_obs) %>% 
  describe()

write.xlsx(x = descritiva.ef, file = "descritiva_eficiencia.xlsx")

u <- union(ef_data$ef_clas_obs, ef_data$ef_clas_pred)
t.1 <- table(factor(ef_data$ef_clas_obs, u), factor(ef_data$ef_clas_pred, u))
Accuracy <- confusionMatrix(t.1)$overall[1]
m.1 <- confusionMatrix(t.1)$byClass %>% 
  as.data.frame() %>% 
  t() %>% 
  cbind(Accuracy) %>% 
  t() %>% 
  as.data.frame() %>% 
  setNames("Value")

indexnames <- rownames(m.1)

m.1 <- m.1 %>% 
  mutate(Index = indexnames)

g.matrix <- m.1 %>% 
ggplot(aes(reorder(Index,-Value), Value)) + 
  geom_col(aes(fill = reorder(Index,-Value)), width = 0.65) +
  scale_x_discrete(labels = as.character(sort(
    round(m.1$Value, 2),
    decreasing = T
  ))) +
  labs(title = "Matrix Confusion Indexes",
       x = "",
       y = "Value",
       fill = " ") +
  theme_linedraw(base_size = 26, base_family = "arial")

g.matrix

m.2 <- m.1[c(1,2,5,12),]

g.matrix.2 <- m.2 %>% 
  ggplot(aes(reorder(Index,-Value), Value)) + 
  geom_col(aes(fill = reorder(Index,-Value)),
           width = 0.65) +
  scale_x_discrete(labels = as.character(sort(
    round(m.2$Value, 3),
    decreasing = T
  ))) +
  labs(title = "Matrix Confusion Indexes",
       x = "",
       y = "Value",
       fill = " ") +
  theme_linedraw(base_size = 26, base_family = "arial")

g.matrix.2

ggsave(filename = "g.matrix.png", 
       g.matrix, width = 13, height = 7, dpi = 450)
ggsave(filename = "g.matrix.2.png", 
       g.matrix.2, width = 13, height = 7, dpi = 450)


