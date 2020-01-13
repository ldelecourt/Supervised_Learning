#####################################################################################
# e1071, support vector machine avec differents prametres
#####################################################################################
library(e1071)

set.seed(1234)

#####################################################################################
# SPAM DATA
#####################################################################################
source("spam_data.R")


#####################################################################################
# PARAMS: kernel="linear", cost=1, epsilon=0.1, tolerance=0.001
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="linear", type="C-classification", cross=10,
               cost=1, epsilon=0.1, tolerance=0.001)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm



#####################################################################################
# PARAMS: kernel="linear", cost=100, epsilon=0.1, tolerance=0.001
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="linear", type="C-classification", cross=10,
               cost=100, epsilon=0.1, tolerance=0.001)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm



#####################################################################################
# PARAMS: kernel="linear", cost=10, epsilon=0.1, tolerance=0.001
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="linear", type="C-classification", cross=10,
               cost=10, epsilon=0.1, tolerance=0.001)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="linear", cost=10, epsilon=1, tolerance=0.001
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="linear", type="C-classification", cross=10,
               cost=10, epsilon=1, tolerance=0.001)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="linear", cost=10, epsilon=100, tolerance=0.001
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="linear", type="C-classification", cross=10,
               cost=10, epsilon=100, tolerance=0.001)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="linear", cost=10, epsilon=0.1, tolerance=0.1
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="linear", type="C-classification", cross=10,
               cost=10, epsilon=0.1, tolerance=0.1)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="polynomial", cost=1, epsilon=0.1, tolerance=0.001, degree=3, coef0=0
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="polynomial", type="C-classification", cross=10,
               cost=1, epsilon=0.1, tolerance=0.001, degree=3, coef0=0)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm



#####################################################################################
# PARAMS: kernel="polynomial", cost=10, epsilon=0.1, tolerance=0.100, degree=3, coef0=0
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="polynomial", type="C-classification", cross=10,
               cost=10, epsilon=0.1, tolerance=0.001, degree=3, coef0=0)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="polynomial", cost=100, epsilon=0.1, tolerance=0.001, degree=3, coef0=0
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="polynomial", type="C-classification", cross=10,
               cost=100, epsilon=0.1, tolerance=0.001, degree=3, coef0=0)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="polynomial", cost=1, epsilon=0.1, tolerance=0.001, degree=10, coef0=0
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="polynomial", type="C-classification", cross=10,
               cost=1, epsilon=0.1, tolerance=0.001, degree=10, coef0=0)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm



#####################################################################################
# PARAMS: kernel="polynomial", cost=100, epsilon=0.1, tolerance=0.001, degree=10, coef0=0
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="polynomial", type="C-classification", cross=10,
               cost=100, epsilon=0.1, tolerance=0.001, degree=10, coef0=0)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm



#####################################################################################
# PARAMS: kernel="polynomial", cost=100, epsilon=0.1, tolerance=0.001, degree=2, coef0=0
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="polynomial", type="C-classification", cross=10,
               cost=100, epsilon=0.1, tolerance=0.001, degree=2, coef0=0)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="polynomial", cost=100, epsilon=0.1, tolerance=0.001, degree=3, coef0=10
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="polynomial", type="C-classification", cross=10,
               cost=100, epsilon=0.1, tolerance=0.001, degree=3, coef0=10)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="polynomial", cost=100, epsilon=0.1, tolerance=0.001, degree=3, coef0=100
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="polynomial", type="C-classification", cross=10,
               cost=100, epsilon=0.1, tolerance=0.001, degree=3, coef0=100)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="polynomial", cost=100, epsilon=0.1, tolerance=0.001, degree=3, coef0=15
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="polynomial", type="C-classification", cross=10,
               cost=100, epsilon=0.1, tolerance=0.001, degree=3, coef0=15)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="polynomial", cost=100, epsilon=0.1, tolerance=0.001, degree=3, coef0=15, gamma=0.001
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="polynomial", type="C-classification", cross=10,
               cost=100, epsilon=0.1, tolerance=0.001, degree=3, coef0=15, gamma=0.001)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm



#####################################################################################
# PARAMS: kernel="polynomial", cost=100, epsilon=0.1, tolerance=0.001, degree=3, coef0=15, gamma=0.0001
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="polynomial", type="C-classification", cross=10,
               cost=100, epsilon=0.1, tolerance=0.001, degree=3, coef0=15, gamma=0.0001)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="polynomial", cost=100, epsilon=0.1, tolerance=0.0001, degree=3, coef0=15, gamma=0.001
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="polynomial", type="C-classification", cross=10,
               cost=100, epsilon=0.1, tolerance=0.0001, degree=3, coef0=15, gamma=0.001)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm



#####################################################################################
# PARAMS: kernel="polynomial", cost=100, epsilon=1, tolerance=0.0001, degree=3, coef0=15, gamma=0.001
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="polynomial", type="C-classification", cross=10,
               cost=100, epsilon=1, tolerance=0.0001, degree=3, coef0=15, gamma=0.001)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="polynomial", cost=100, epsilon=1, tolerance=0.0001, degree=2, coef0=15, gamma=0.001
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="polynomial", type="C-classification", cross=10,
               cost=100, epsilon=1, tolerance=0.0001, degree=2, coef0=15, gamma=0.001)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm




#####################################################################################
# PARAMS: kernel="sigmoid", cost=100, epsilon=1, tolerance=0.0001, coef0=15, gamma=0.001
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="sigmoid", type="C-classification", cross=10,
               cost=100, epsilon=1, tolerance=0.0001, coef0=15, gamma=0.001)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="sigmoid", cost=1, epsilon=0.1, tolerance=0.001, coef0=0, gamma=default
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="sigmoid", type="C-classification", cross=10,
               cost=1, epsilon=0.1, tolerance=0.001, coef0=0)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="sigmoid", cost=10, epsilon=0.1, tolerance=0.001, coef0=0, gamma=default
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="sigmoid", type="C-classification", cross=10,
               cost=10, epsilon=0.1, tolerance=0.001, coef0=0)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="sigmoid", cost=0.01, epsilon=0.1, tolerance=0.001, coef0=0, gamma=default
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="sigmoid", type="C-classification", cross=10,
               cost=0.01, epsilon=0.1, tolerance=0.001, coef0=0)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="sigmoid", cost=1, epsilon=0.1, tolerance=0.001, coef0=0.01, gamma=default
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="sigmoid", type="C-classification", cross=10,
               cost=1, epsilon=0.1, tolerance=0.001, coef0=0.01)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="sigmoid", cost=1, epsilon=0.1, tolerance=0.001, coef0=0, gamma=0.01
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="sigmoid", type="C-classification", cross=10,
               cost=1, epsilon=0.1, tolerance=0.001, coef0=0, gamma=0.01)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="sigmoid", cost=1, epsilon=0.1, tolerance=0.001, coef0=0, gamma=0.01
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="sigmoid", type="C-classification", cross=10,
               cost=1, epsilon=0.1, tolerance=0.001, coef0=0, gamma=0.01)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="radial", cost=1, epsilon=0.1, tolerance=0.001, gamma=default
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="radial", type="C-classification", cross=10,
               cost=1, epsilon=0.1, tolerance=0.001)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="radial", cost=1.2, epsilon=0.1, tolerance=0.0001, gamma=0.02
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="radial", type="C-classification", cross=10,
               cost=1.2, epsilon=0.1, tolerance=0.0001, gamma=0.02)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm


#####################################################################################
# PARAMS: kernel="radial", cost=1.2, epsilon=0.1, tolerance=0.0001, gamma=0.02
#####################################################################################
# Creation du modele, fit
svm_fit <- svm(V58~. , data=df_train, kernel="radial", type="C-classification", cross=10,
               cost=1.6, epsilon=0.1, tolerance=0.0001, gamma=0.02)
# Calcul des prediction
svm_pred <- predict(svm_fit, X_test)
# Taux d'erreur
TE_svm <- sum(Y_test != svm_pred)/nrow(Y_test)
TE_svm
