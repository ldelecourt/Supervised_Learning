#####################################################################################
# neuralnet, RNN avec 1 couche cachee
#####################################################################################
library(neuralnet)

set.seed(1234)

#####################################################################################
# SPAM DATA
#####################################################################################
source("spam_data.R")


#####################################################################################
# Creation de la formula
#####################################################################################
nom_var_exp <- colnames(X_train)
var_cibl <- colnames(Y_train)
var_exp <- paste(nom_var_exp ,collapse = "+")
formule <- paste(var_cibl, "== '0' ~", var_exp)
formule <- as.formula(formule)
# On met sous forme de factor la variable a expliquer
df_train_rnn <- df_train
df_train_rnn$V58 <- as.factor(df_train_rnn$V58)


#####################################################################################
# Algorithm = "rprop+"
#####################################################################################
# Choix du nombre de decoupage k pour la CV
k <- 3
TE_rnn_cv <- rep(1, k)
# fonction d'activation
softplus <- function(x) log(1 + exp(x))
#relu <- function(x) {x * (x>=0)}
# CV
for (i in 1:k) {
  # Sample data
  index <- sample(1:nrow(df_train_rnn), (1-(1/k))*nrow(df_train_rnn))
  cv_train <- df_train_rnn[index, ]
  cv_test <- df_train_rnn[-index, ]
  
  # Fit
  rnn_fit <- neuralnet(formula=formule, data=cv_train, linear.output = FALSE,
                       hidden=c(2), act.fct=softplus, algorithm="rprop+")
  
  # Prediction
  rnn_pred <- predict(rnn_fit, cv_test)
  
  print(table(cv_test$V58 == "0", rnn_pred > 0.5))
  # Taux d'erreur
  TE_rnn_cv[i] <- 1 - sum(diag(table(cv_test$V58 == "0", rnn_pred > 0.5))) / nrow(cv_test)
}

# Taux d'erreur moyen pour le modèle d'entrainement
mean(TE_rnn_cv)

# Prediction sur les vraies données test
rnn_pred_test <- predict(rnn_fit, X_test)
# Taux d'erreur sur les données test
TE_rnn_test <- 1 - sum(diag(table(Y_test == 0, rnn_pred_test > 0.5))) / nrow(Y_test)
TE_rnn_test



#####################################################################################
# Algorithm = "backpropagation"
#####################################################################################
# Choix du nombre de decoupage k pour la CV
k <- 3
TE_rnn_cv <- rep(1, k)
# fonction d'activation
softplus <- function(x) log(1 + exp(x))
#relu <- function(x) {x * (x>=0)}
# CV
for (i in 1:k) {
  # Sample data
  index <- sample(1:nrow(df_train_rnn), (1-(1/k))*nrow(df_train_rnn))
  cv_train <- df_train_rnn[index, ]
  cv_test <- df_train_rnn[-index, ]
  
  # Fit
  rnn_fit <- neuralnet(formula=formule, data=cv_train, linear.output = FALSE, rep=3,
                       algorithm="backprop", act.fct="logistic", hidden=c(5), learningrate=0.1)
  
  # Prediction
  rnn_pred <- predict(rnn_fit, cv_test)
  
  print(table(cv_test$V58 == "0", rnn_pred > 0.5))
  # Taux d'erreur
  TE_rnn_cv[i] <- 1 - sum(diag(table(cv_test$V58 == "0", rnn_pred > 0.5))) / nrow(cv_test)
}

# Taux d'erreur moyen pour le modèle d'entrainement
mean(TE_rnn_cv)

# Prediction sur les vraies données test
rnn_pred_test <- predict(rnn_fit, X_test)
# Taux d'erreur sur les données test
TE_rnn_test <- 1 - sum(diag(table(Y_test == 0, rnn_pred_test > 0.5))) / nrow(Y_test)
TE_rnn_test


#####################################################################################
# Algorithm = sag
#####################################################################################
# Choix du nombre de decoupage k pour la CV
k <- 3
TE_rnn_cv <- rep(1, k)
# fonction d'activation
softplus <- function(x) log(1 + exp(x))
#relu <- function(x) {x * (x>=0)}
# CV
for (i in 1:k) {
  # Sample data
  index <- sample(1:nrow(df_train_rnn), (1-(1/k))*nrow(df_train_rnn))
  cv_train <- df_train_rnn[index, ]
  cv_test <- df_train_rnn[-index, ]
  
  # Fit
  rnn_fit <- neuralnet(formula=formule, data=cv_train, linear.output = FALSE, rep=1,
                       algorithm="sag", act.fct="logistic", hidden=c(1))
  
  # Prediction
  rnn_pred <- predict(rnn_fit, cv_test)
  
  print(table(cv_test$V58 == "0", rnn_pred > 0.5))
  # Taux d'erreur
  TE_rnn_cv[i] <- 1 - sum(diag(table(cv_test$V58 == "0", rnn_pred > 0.5))) / nrow(cv_test)
}

# Taux d'erreur moyen pour le modèle d'entrainement
mean(TE_rnn_cv)

# Prediction sur les vraies données test
rnn_pred_test <- predict(rnn_fit, X_test)
# Taux d'erreur sur les données test
TE_rnn_test <- 1 - sum(diag(table(Y_test == 0, rnn_pred_test > 0.5))) / nrow(Y_test)
TE_rnn_test



#####################################################################################
# Algorithm = slr
#####################################################################################
# Choix du nombre de decoupage k pour la CV
k <- 3
TE_rnn_cv <- rep(1, k)
# fonction d'activation
softplus <- function(x) log(1 + exp(x))
#relu <- function(x) {x * (x>=0)}
# CV
for (i in 1:k) {
  # Sample data
  index <- sample(1:nrow(df_train_rnn), (1-(1/k))*nrow(df_train_rnn))
  cv_train <- df_train_rnn[index, ]
  cv_test <- df_train_rnn[-index, ]
  
  # Fit
  rnn_fit <- neuralnet(formula=formule, data=cv_train, linear.output = FALSE, rep=2,
                       algorithm="slr", act.fct="tanh", hidden=c(2))
  
  # Prediction
  rnn_pred <- predict(rnn_fit, cv_test)
  # matrice de confusion
  print(table(cv_test$V58 == "0", rnn_pred > 0.5))
  # Taux d'erreur
  TE_rnn_cv[i] <- 1 - sum(diag(table(cv_test$V58 == "0", rnn_pred > 0.5))) / nrow(cv_test)
}

# Taux d'erreur moyen pour le modèle d'entrainement
mean(TE_rnn_cv)

# Prediction sur les vraies données test
rnn_pred_test <- predict(rnn_fit, X_test)
# Taux d'erreur sur les données test
TE_rnn_test <- 1 - sum(diag(table(Y_test == 0, rnn_pred_test > 0.5))) / nrow(Y_test)
TE_rnn_test


#####################################################################################
# Algorithm = rprop-
#####################################################################################
# Choix du nombre de decoupage k pour la CV
k <- 3
TE_rnn_cv <- rep(1, k)
# CV
for (i in 1:k) {
  # Sample data
  index <- sample(1:nrow(df_train_rnn), (1-(1/k))*nrow(df_train_rnn))
  cv_train <- df_train_rnn[index, ]
  cv_test <- df_train_rnn[-index, ]
  
  # Fit
  rnn_fit <- neuralnet(formula=formule, data=cv_train, linear.output = FALSE, rep=1,
                       algorithm="rprop-", act.fct="tanh", hidden=c(5))
  
  # Prediction
  rnn_pred <- predict(rnn_fit, cv_test)
  # matrice de confusion
  print(table(cv_test$V58 == "0", rnn_pred > 0.5))
  # Taux d'erreur
  TE_rnn_cv[i] <- 1 - sum(diag(table(cv_test$V58 == "0", rnn_pred > 0.5))) / nrow(cv_test)
}

# Taux d'erreur moyen pour le modèle d'entrainement
mean(TE_rnn_cv)

# Prediction sur les vraies données test
rnn_pred_test <- predict(rnn_fit, X_test)
# Taux d'erreur sur les données test
TE_rnn_test <- 1 - sum(diag(table(Y_test == 0, rnn_pred_test > 0.5))) / nrow(Y_test)
TE_rnn_test
