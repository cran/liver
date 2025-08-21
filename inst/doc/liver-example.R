## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = " ", fig.width = 7, fig.height = 7, fig.align = "center")

## ----include = FALSE----------------------------------------------------------
library(liver)
library(ggplot2)  
library(pROC)    

## -----------------------------------------------------------------------------
data(churn)       

str(churn)

## -----------------------------------------------------------------------------
set.seed(5)

data_sets = partition(data = churn, ratio = c(0.8, 0.2))

train_set = data_sets $ part1
test_set  = data_sets $ part2

actual_test  = test_set $ churn

## -----------------------------------------------------------------------------
formula = churn ~ account.length + voice.plan + voice.messages + intl.plan + intl.mins + 
                  day.mins + eve.mins + night.mins + customer.calls

predict_knn = kNN(formula, train = train_set, test = test_set, k = 8)

## ----fig.align = 'center', fig.height = 3, fig.width = 3----------------------
conf.mat(predict_knn, actual_test)

conf.mat.plot(predict_knn, actual_test)

## -----------------------------------------------------------------------------
mse(predict_knn, actual_test)

## -----------------------------------------------------------------------------
predict_knn_trans = kNN(formula, train = train_set, test = test_set, k = 8, scaler = "minmax")

## ----fig.show = "hold", fig.align = 'default', out.width = "46%"--------------
conf.mat.plot(predict_knn_trans, actual_test)

conf.mat.plot(predict_knn, actual_test)

## -----------------------------------------------------------------------------
prob_knn = kNN(formula, train = train_set, test = test_set, k = 8, type = "prob")[ , 1 ]

prob_knn_trans = kNN(formula, train = train_set, test = test_set, scaler = "minmax", k = 8, type = "prob")[ , 1 ]

## ----message = F, fig.align = "center"----------------------------------------
roc_knn = roc(actual_test, prob_knn)
roc_knn_trans = roc(actual_test, prob_knn_trans)

ggroc(list(roc_knn, roc_knn_trans), size = 0.8) + 
    theme_minimal() + ggtitle("ROC plots with AUC") +
  scale_color_manual(values = c("red", "blue"), 
    labels = c(paste("AUC=", round(auc(roc_knn), 3), "; Raw data; "),
                paste("AUC=", round(auc(roc_knn_trans), 3), "; Transformed data"))) +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.7, .3), text = element_text(size = 17)) + 
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color = "grey", linetype = "dashed")

## ----fig.align = "center"-----------------------------------------------------
kNN.plot(formula, train = train_set, test = test_set, scaler = "minmax", 
          k.max = 30, set.seed = 3)

