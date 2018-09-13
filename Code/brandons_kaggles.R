trn <- read.csv(file.choose())
tst <- read.csv(file.choose())

all <- rbind(trn, tst)

ohe <- caret::dummyVars(Sale_Price ~ ., data = all)

trn_ohe <- predict(ohe, newdata = trn) %>% as.data.frame()
tst_ohe <- predict(ohe, newdata = tst) %>% as.data.frame()

trn_ohe$Sale_Price <- trn$Sale_Price

fit <- lm(Sale_Price ~ ., data = trn_ohe)
pred <- predict(fit, newdata = tst_ohe)

kaggle <- data.frame(
  "id" = 1:879,
  "Sale_Price" = pred
)

library(glmnet)
fit2 <- glmnet(
  x = as.matrix(subset(trn_ohe, select = -Sale_Price)),
  y = trn_ohe$Sale_Price
)

pred2 <- predict(fit2, newx = as.matrix(tst_ohe), s = 85)

kaggle2 <- data.frame(
  "id" = 1:879,
  "Sale_Price" = as.numeric(pred2)
)

library(caret)
fit3 <- train(
  x = as.matrix(subset(trn_ohe, select = -Sale_Price)),
  y = trn_ohe$Sale_Price,
  method = "lm"
)

pred3 <- predict(fit3, newdata = tst_ohe)

kaggle3 <- data.frame(
  "id" = 1:879,
  "Sale_Price" = pred3
)

write.csv(kaggle3, file = "kaggle3.csv", row.names = FALSE)


library(ranger)
fit4 <- randomForest(
  x = as.matrix(subset(trn_ohe, select = -Sale_Price)),
  y = trn_ohe$Sale_Price,
  ntree = 1000
)
pred4 <- predict(fit4, newdata = tst_ohe
                 )
kaggle4 <- data.frame(
  "id" = 1:879,
  "Sale_Price" = pred4
)

write.csv(kaggle4, file = "kaggle4.csv", row.names = FALSE)

fit5 <- earth(
  x = as.matrix(subset(trn_ohe, select = -Sale_Price)),
  y = trn_ohe$Sale_Price,
  degree = 3
)
pred5 <- predict(fit5, newdata = tst_ohe)

kaggle5 <- data.frame(
  "id" = 1:879,
  "Sale_Price" = as.numeric(pred5)
)
write.csv(kaggle5, file = "kaggle5.csv", row.names = FALSE)



library(caret)
library(rpart)
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
set.seed(101)
tree <- train(
  x = as.matrix(subset(trn_ohe, select = -Sale_Price)),
  y = trn_ohe$Sale_Price,
  method = "rpart",
  tuneLength = 10,
  trControl = ctrl
)
pred <- predict(tree, newdata = tst_ohe)

kaggle_tree <- data.frame(
  "id" = 1:879,
  "Sale_Price" = pred
)
write.csv(kaggle_tree, file = "kaggle_tree.csv", row.names = FALSE)

