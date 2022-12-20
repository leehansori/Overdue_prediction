#데이터 읽기
data <- read.csv(file = "/Users/ieunhui/Downloads/dataset2.csv")
df = data
head(df)
summary(df)

#탐색적 자료분석 
summary(df)
hist(df$x,breaks=2)
plot(df$y1)
plot(df$y2)
plot(df$y3)
hist(df$y4,breaks=8)
plot(df$y5)

#로지스틱 회귀모형 구축 - 분류예측모델 
model <- glm(x~ y1 + y2 + y3 + y4 + y5, data = df, family=binomial(logit))
summary(model)
exp(coef(model))

#로지스틱 회귀모형 정확도 확인-1
Log_odds = predict(model, newdata = df)
Probability_1 = predict(model, newdata = df, type = 'response')

PREDICTED_C_1 = ifelse(Probability_1 > 0.5 , 1 , 0)
f_predicted_1 = as.factor(PREDICTED_C_1)
f_actual = as.factor(df$x)

#install.packages(c("caret","e1071"))
library(caret)
confusionMatrix(f_actual,f_predicted_1)

#로지스틱 회귀모형 roc 곡선
#install.packages("pROC")
library(pROC)

ROC = roc(df$x,Probability_1)

auc(df$x,Probability_1)

plot.roc(ROC,   
         col="royalblue",  
         print.auc=TRUE, 
         max.auc.polygon=TRUE,   
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")

#성능평가 
#install.packages("MLmetrics")
pred$prediction <- predict(model, newdata=df, type='response')

library(MLmetrics)
Recall(f_predicted_1, df$x, positive='1')*100
Accuracy(f_predicted_1, df$x)*100
precision(f_actual,f_predicted_1, positive='1')*100
F1_Score(f_actual,f_predicted_1, positive='1')*100
