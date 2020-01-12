library(DataExplorer)
library(glmnet)
#library(InformationValue)
library(neuralnet)
library(e1071)

set.seed(1234)

# SPAM DATA
df <- read.csv("/Users/d/Cours/SISE_M2/App_stat/projet/non_supervise/spambase.data", header=FALSE, sep=",")
introduce(df)
# derniere colonne en factor
#df$V58 <- as.factor(df$V58)
# sample de df afin de melanger le df
subset <- sample(1:nrow(df), nrow(df), replace=FALSE)
df <- df[subset, ]

# Jeu de donnees train et test
df_train <- df[1:1500, ]
df_test <- df[1501:4601, ]
# Données explicatives et variable a prédire d'entrainement
X_train <- as.matrix(df_train[-ncol(df_train)])
Y_train <- as.matrix(df_train[ncol(df_train)])
# Donnees test
X_test <- as.matrix(df_test[-ncol(df_test)])
Y_test <- as.matrix(df_test[ncol(df_test)])


#####################################################################################
# GLMNET
#####################################################################################
# Regression lineraire penalisee (elasticnet)
# pour glmnet seulement. lasso: alpha=1, ridge= alpha=0
# 1) fit <- cv.glmnet
# 2) predict(fit, ...)
glm_fit <- cv.glmnet(X_train, Y_train, family="binomial", type.measure="class", nfolds=3, 
                     alpha=0.8, standardize=TRUE)
# plot fit
plot(glm_fit)
# Calcul de prediction avec lambda = lambda.min, valeurs de lambda qui minimise l'erreur en CV
glm_pred <- predict(glm_fit, newx=X_test, type="class", s=glm_fit$lambda.min)
# Vérification
table(glm_pred)
table(Y_test)
# Taux d'erreur
TE_glm <- sum(Y_test != glm_pred)/nrow(Y_test)
TE_glm



#####################################################################################
# NEURALNET
#####################################################################################
# RNN avec 1 couche cachée
# Creation de la formula
nom_var_exp <- colnames(X_train)
var_cibl <- colnames(Y_train)
var_exp <- paste(nom_var_exp ,collapse = "+")
formule <- paste(var_cibl, "== '0' ~", var_exp)
formule <- as.formula(formule)
# On met sous forme de factor la variable a expliquer
df_train_rnn <- df_train
df_train_rnn$V58 <- as.factor(df_train_rnn$V58)

# fitting RNN, act.fun pour fonction d'activation
rnn_fit <- neuralnet(formula=formule, data=df_train_rnn, hidden=c(4), linear.output = FALSE,
                     act.fct="logistic")
plot(rnn_fit, file="test_rnn1")
# Prediction
rnn_pred <- predict(rnn_fit, X_test)
# Matrice de confusion
table(Y_test == 0, rnn_pred[,1] > 0.5)
TE_rnn <- 1 - sum(diag(table(Y_test == 0, rnn_pred[,1] > 0.5))) / nrow(Y_test)

# CV à faire à la main


#####################################################################################
# SVM
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="linear", type="C-classification", cross=2)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


