library(DataExplorer)
set.seed(1234)

#####################################################################################
# SPAM DATA
#####################################################################################
df <- read.csv("spambase.data", header=FALSE, sep=",")
introduce(df)
# derniere colonne en factor
#df$V58 <- as.factor(df$V58)
# On centre-réduit les données car différentes unites
Y <- df[ncol(df)]
df <- as.data.frame(scale(df[-ncol(df)], center=TRUE, scale=TRUE))
df <- cbind(df, Y)
summary(df)
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