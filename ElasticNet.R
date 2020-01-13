#####################################################################################
# GLMNET, regression lineaire penalisee elasticnet
#####################################################################################
library(glmnet)

set.seed(1234)

#####################################################################################
# SPAM DATA
#####################################################################################
source("spam_data.R")


#####################################################################################
# PARAMS: alpha=0.8, weights=1
#####################################################################################
glm_fit <- cv.glmnet(X_train, Y_train, family="binomial", type.measure="class", nfolds=10, alpha=0.8)
# plot fit
plot(glm_fit)
glm_fit$lambda.min
coef(glm_fit, s="lambda.min")
# Calcul de prediction avec lambda = lambda.min, valeurs de lambda qui minimise l'erreur en CV
glm_pred <- predict(glm_fit, newx=X_test, type="class", s=c(glm_fit$lambda.min, glm_fit$lambda.1se))
# Taux d'erreur avec lambda.min
TE_glm_min <- sum(Y_test != glm_pred[ ,1])/nrow(Y_test)
TE_glm_min
# Taux d'erreur avec lambda.lse
TE_glm_lse <- sum(Y_test != glm_pred[ ,2])/nrow(Y_test)
TE_glm_lse


#####################################################################################
# PARAMS: alpha=0.8, weights=1000
#####################################################################################
glm_fit <- cv.glmnet(X_train, Y_train, family="binomial", type.measure="class", nfolds=10, 
                     weights=rep(1000, nrow(X_train)), alpha=0.8)
# plot fit
plot(glm_fit)
# Calcul de prediction avec lambda = lambda.min, valeurs de lambda qui minimise l'erreur en CV
glm_pred <- predict(glm_fit, newx=X_test, type="class", s=c(glm_fit$lambda.min, glm_fit$lambda.1se))
# Taux d'erreur avec lambda.min
TE_glm_min <- sum(Y_test != glm_pred[ ,1])/nrow(Y_test)
TE_glm_min
# Taux d'erreur avec lambda.lse
TE_glm_lse <- sum(Y_test != glm_pred[ ,2])/nrow(Y_test)
TE_glm_lse


#####################################################################################
# PARAMS: alpha=0.8, weights=10
#####################################################################################
glm_fit <- cv.glmnet(X_train, Y_train, family="binomial", type.measure="class", nfolds=10, 
                     weights=rep(10, nrow(X_train)), alpha=0.8)
# plot fit
plot(glm_fit)
# Calcul de prediction avec lambda = lambda.min, valeurs de lambda qui minimise l'erreur en CV
glm_pred <- predict(glm_fit, newx=X_test, type="class", s=c(glm_fit$lambda.min, glm_fit$lambda.1se))
# Taux d'erreur avec lambda.min
TE_glm_min <- sum(Y_test != glm_pred[ ,1])/nrow(Y_test)
TE_glm_min
# Taux d'erreur avec lambda.lse
TE_glm_lse <- sum(Y_test != glm_pred[ ,2])/nrow(Y_test)
TE_glm_lse



#####################################################################################
# PARAMS: alpha=0.1, weights=1
#####################################################################################
glm_fit <- cv.glmnet(X_train, Y_train, family="binomial", type.measure="class", nfolds=10, 
                     weights=rep(1, nrow(X_train)), alpha=0.1)
# plot fit
plot(glm_fit)
# Calcul de prediction avec lambda = lambda.min, valeurs de lambda qui minimise l'erreur en CV
glm_pred <- predict(glm_fit, newx=X_test, type="class", s=c(glm_fit$lambda.min, glm_fit$lambda.1se))
# Taux d'erreur avec lambda.min
TE_glm_min <- sum(Y_test != glm_pred[ ,1])/nrow(Y_test)
TE_glm_min
# Taux d'erreur avec lambda.lse
TE_glm_lse <- sum(Y_test != glm_pred[ ,2])/nrow(Y_test)
TE_glm_lse



#####################################################################################
# PARAMS: alpha=0.99, weights=1
#####################################################################################
glm_fit <- cv.glmnet(X_train, Y_train, family="binomial", type.measure="class", nfolds=10, 
                     weights=rep(1, nrow(X_train)), alpha=0.99)
# plot fit
plot(glm_fit)
# Calcul de prediction avec lambda = lambda.min, valeurs de lambda qui minimise l'erreur en CV
glm_pred <- predict(glm_fit, newx=X_test, type="class", s=c(glm_fit$lambda.min, glm_fit$lambda.1se))
# Taux d'erreur avec lambda.min
TE_glm_min <- sum(Y_test != glm_pred[ ,1])/nrow(Y_test)
TE_glm_min
# Taux d'erreur avec lambda.lse
TE_glm_lse <- sum(Y_test != glm_pred[ ,2])/nrow(Y_test)
TE_glm_lse
