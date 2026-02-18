## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = " ", fig.width = 7, fig.height = 7, fig.align = "center")

## ----include = FALSE----------------------------------------------------------
library(liver)
library(ggplot2)  
library(pROC)    

## -----------------------------------------------------------------------------
data(churn_mlc)       

str(churn_mlc)

## -----------------------------------------------------------------------------
set.seed(42)

splits = partition(data = churn_mlc, ratio = c(0.8, 0.2))

train_set = splits$part1
test_set  = splits$part2

test_labels  = test_set$churn

## -----------------------------------------------------------------------------
formula = churn ~ account_length + voice_plan + voice_messages + intl_plan + intl_mins + 
                  day_mins + eve_mins + night_mins + customer_calls

predict_knn = kNN(formula, train = train_set, test = test_set, k = 6)

## ----fig.align = 'center', fig.height = 3, fig.width = 3----------------------
conf.mat(predict_knn, test_labels)

conf.mat.plot(predict_knn, test_labels)

## -----------------------------------------------------------------------------
mse(predict_knn, test_labels)

## -----------------------------------------------------------------------------
predict_knn_trans = kNN(formula, train = train_set, test = test_set, k = 6, scaler = "minmax")

## ----fig.show = "hold", fig.align = 'default', out.width = "46%"--------------
conf.mat.plot(predict_knn_trans, test_labels)

conf.mat.plot(predict_knn, test_labels)

## -----------------------------------------------------------------------------
prob_knn = kNN(formula, train = train_set, test = test_set, k = 6, type = "prob")[, 1]

prob_knn_trans = kNN(formula, train = train_set, test = test_set, scaler = "minmax", k = 6, type = "prob")[, 1]

## ----message = F, fig.align = "center"----------------------------------------
roc_knn = roc(test_labels, prob_knn)
roc_knn_trans = roc(test_labels, prob_knn_trans)

ggroc(list(roc_knn, roc_knn_trans), linewidth = 0.8) + 
    theme_minimal() + ggtitle("ROC plots with AUC") +
  scale_color_manual(values = c("red", "blue"), 
    labels = c(paste("AUC=", round(auc(roc_knn), 3), "; Raw data; "),
                paste("AUC=", round(auc(roc_knn_trans), 3), "; Transformed data"))) +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.7, .3), text = element_text(size = 17)) + 
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color = "grey", linetype = "dashed")

## ----fig.align = "center"-----------------------------------------------------
kNN.plot(formula, train = train_set, ratio = c(0.7, 0.3), scaler = "minmax", 
          k.max = 30, set.seed = 3)

