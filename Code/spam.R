# Load the spam data
data(spam, package = "kernlab") #<<

# Relabel class
spam$type <- ifelse(spam$type == "spam", "spam", "ham")

# Partition the data into train/test sets
set.seed(101)  # for reproducibility
trn_id <- createDataPartition(spam$type, p = 0.7, list = FALSE)
trn <- spam[trn_id, ]                # training data
tst <- spam[-trn_id, ]               # test data
xtrn <- subset(trn, select = -type)  # training data features
xtst <- subset(tst, select = -type)  # test data features
ytrn <- trn$type                     # training data response
ytst <- tst$type                     # test data response

# Sample submission file
spam_sample_submission <- data.frame(
  "id" = 1L:3L,
  "type" = c("spam", "spam", "ham")
)
path <- "/Users/bgreenwell/Dropbox/AnalyticsSummit18/"
write.csv(spam_sample_submission, 
          file = paste0(path, "spam_sample_submission.csv"), 
          row.names = FALSE)

# Solution file
spam_solution_file <- data.frame(
  "id" = seq_len(nrow(tst)),
  "type" = tst$type
)
write.csv(spam_sample_submission, 
          file = paste0(path, "spam_solution.csv"), 
          row.names = FALSE)