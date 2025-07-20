### 讀取資料 ###
library(readxl)
df <- read_excel("NCCU DA/專案/Airline Satisfaction 1105.xlsx")
View(df)
library(tidyverse)

### 分群 ###
df_split <- df %>%
  group_by(`Customer Type`, `Type of Travel`) %>%
  group_split()

names(df_split) <- map_chr(df_split, function(group) {
  group_name <- paste(unique(group$`Customer Type`), 
                      unique(group$`Type of Travel`), 
                      sep = "_")
  return(group_name)
})

list2env(df_split, envir = .GlobalEnv)


### 隨機森林 ###

library(MASS)
library(randomForest)
library(caret)
library(reshape2)
library(ggplot2)

# 定義函數
predict_satisfaction_rf <- function(df, train_size = 0.7, plot_error = TRUE) {
  # 清理變數名稱
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  
  # 分割訓練集和測試集
  trainI <- sample(1:nrow(df), size = round(train_size * nrow(df)))
  traind <- df[trainI, ]
  testd <- df[-trainI, ]
  
  # 選擇需要的變數並將滿意度變數轉為因子類型
  traind_selected <- traind[, c(8:21, which(colnames(df) == "satisfaction"))]
  testd_selected <- testd[, c(8:21, which(colnames(df) == "satisfaction"))]
  traind_selected$satisfaction <- as.factor(traind_selected$satisfaction)
  testd_selected$satisfaction <- as.factor(testd_selected$satisfaction)
  
  # 訓練隨機森林模型
  rf <- randomForest(satisfaction ~ ., data = traind_selected, importance = TRUE)
  
  # 繪製錯誤率圖
  if (plot_error) {
    plot(rf)
    legend("topright", colnames(rf$err.rate), col = 1:4, cex = 0.8, fill = 1:4)
  }
  
  # 預測與計算混淆矩陣
  pred <- predict(rf, newdata = testd_selected)
  conf_matrix <- table(Real = testd_selected$satisfaction, Predict = pred)
  
  # 計算準確率
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  return(list(model = rf, predictions = pred, accuracy = accuracy, conf_matrix = conf_matrix))
}

# 跑隨機森林
rf_results <- list()
for (group_name in names(df_split)) {
  group_data <- df_split[[group_name]]
  colnames(group_data) <- make.names(colnames(group_data), unique = TRUE)
  
  cat("Processing group:", group_name, "\n")
  result <- predict_satisfaction_rf(group_data, train_size = 0.7, plot_error = FALSE)
  rf_results[[group_name]] <- result
}

# 顯示各分群結果
for (group_name in names(rf_results)) {
  cat("=== Group:", group_name, "===\n")
  
  result <- rf_results[[group_name]]
  
  cat("Accuracy:", round(result$accuracy, 4), "\n")
  
  cat("Confusion Matrix:\n")
  print(result$conf_matrix)
  
  conf_matrix <- result$conf_matrix
  precision <- diag(conf_matrix) / colSums(conf_matrix)
  recall <- diag(conf_matrix) / rowSums(conf_matrix)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  metrics <- data.frame(
    Class = rownames(conf_matrix),
    Precision = round(precision, 4),
    Recall = round(recall, 4),
    F1_Score = round(f1_score, 4)
  )
  
  cat("Metrics per Class:\n")
  print(metrics)
  
  cat("Variable Importance:\n")
  importance_result <- importance(result$model)
  print(importance_result)
}

