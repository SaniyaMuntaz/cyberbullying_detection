# Load the dataset
data <- read.csv(
  "C:/Users/M SANIYA MUNTAZ/Downloads/datascience/cyberbullying_tweets ds.csv",
  stringsAsFactors = FALSE
)

# Display the first 6 rows
head(data)
dim(data)
names(data)
str(data)
table(data$cyberbullying_type)
colSums(is.na(data))
sum(duplicated(data$tweet_text))
# Keep the original dataset unchanged
clean_data <- data

# Remove rows with duplicate tweet text
clean_data <- clean_data[
  !duplicated(clean_data$tweet_text),
]

# Check the new dimensions
dim(clean_data)

# Verify that duplicates are removed
sum(duplicated(clean_data$tweet_text))
# Count empty or whitespace-only tweets
sum(trimws(clean_data$tweet_text) == "")
table(clean_data$cyberbullying_type)

# Create six separate category columns
clean_data$age <- as.integer(
  clean_data$cyberbullying_type == "age"
)

clean_data$ethnicity <- as.integer(
  clean_data$cyberbullying_type == "ethnicity"
)

clean_data$gender <- as.integer(
  clean_data$cyberbullying_type == "gender"
)

clean_data$not_cyberbullying <- as.integer(
  clean_data$cyberbullying_type == "not_cyberbullying"
)

clean_data$other_cyberbullying <- as.integer(
  clean_data$cyberbullying_type == "other_cyberbullying"
)

clean_data$religion <- as.integer(
  clean_data$cyberbullying_type == "religion"
)

# Check the new dataset
dim(clean_data)
names(clean_data)

# View the first few rows
head(clean_data)

# Shuffle the rows randomly
set.seed(123)

clean_data <- clean_data[
  sample(nrow(clean_data)),
]

# Check the first 10 rows
head(clean_data, 10)

# Check whether all categories are still present
table(clean_data$cyberbullying_type)

# Check dataset dimensions
dim(clean_data)

# Check column names
names(clean_data)

# Check the first 6 rows
head(clean_data)

# Check the number of tweets in each category
table(clean_data$cyberbullying_type)

# Check missing values
colSums(is.na(clean_data))

# Reset row indexes after shuffling
rownames(clean_data) <- NULL

# Check the first 10 rows and their indexes
head(clean_data, 10)
head(rownames(clean_data), 10)

# Install these packages only if you haven't installed them before
install.packages("stringr")
install.packages("dplyr")

# Load packages
library(stringr)
library(dplyr)

# Create a cleaned text column
clean_data <- clean_data %>%
  mutate(
    tweet_clean = tweet_text,
    tweet_clean = str_to_lower(tweet_clean),
    tweet_clean = str_remove_all(tweet_clean, "https?://\\S+|www\\.\\S+"),
    tweet_clean = str_remove_all(tweet_clean, "@\\w+"),
    tweet_clean = str_replace_all(tweet_clean, "[^a-z\\s]", " "),
    tweet_clean = str_squish(tweet_clean)
  )

# View original and cleaned tweets
head(clean_data[, c("tweet_text", "tweet_clean")], 10)

# Check for missing values in the new column
sum(is.na(clean_data$tweet_clean))

# Display some cleaned tweets
head(clean_data$tweet_clean, 10)

# Find tweets that became empty after cleaning
sum(clean_data$tweet_clean == "")

# Remove tweets that became empty after cleaning
clean_data <- clean_data[
  clean_data$tweet_clean != "",
]

# Reset row names
rownames(clean_data) <- NULL

# Check the updated dataset dimensions
dim(clean_data)

# Confirm that no empty tweets remain
sum(clean_data$tweet_clean == "")

install.packages("tidytext")  # Run only if not installed
library(tidytext)
library(dplyr)
library(ggplot2)

# Split each cleaned tweet into individual words
tokens <- clean_data %>%
  select(cyberbullying_type, tweet_clean) %>%
  unnest_tokens(word, tweet_clean)

# View the first 20 words
head(tokens, 20)

# Count the total number of word tokens
nrow(tokens)

# Load the built-in stop-word list
data("stop_words", package = "tidytext")

# Remove common stop words
tokens_clean <- tokens %>%
  anti_join(stop_words, by = "word")

# Check the result
head(tokens_clean, 20)
nrow(tokens_clean)

word_frequency <- tokens_clean %>%
  count(word, sort = TRUE)

# Display the 20 most frequent words
head(word_frequency, 20)

top_words <- word_frequency %>%
  slice_max(order_by = n, n = 20, with_ties = FALSE)

ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequent Words in Cyberbullying Tweets",
    x = "Word",
    y = "Frequency"
  ) +
  theme_minimal()

category_word_frequency <- tokens_clean %>%
  count(cyberbullying_type, word, sort = TRUE)

# Display the most frequent words by category
category_word_frequency %>%
  group_by(cyberbullying_type) %>%
  slice_max(order_by = n, n = 10, with_ties = FALSE) %>%
  arrange(cyberbullying_type, desc(n))

library(ggplot2)
library(dplyr)

top_category_words <- category_word_frequency %>%
  group_by(cyberbullying_type) %>%
  slice_max(order_by = n, n = 10, with_ties = FALSE) %>%
  ungroup()

ggplot(
  top_category_words,
  aes(x = reorder_within(word, n, cyberbullying_type),
      y = n,
      fill = cyberbullying_type)
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ cyberbullying_type, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "Top 10 Words in Each Cyberbullying Category",
    x = "Word",
    y = "Frequency"
  ) +
  theme_minimal()

library(dplyr)
library(tidytext)

# Assign a unique ID to each tweet
clean_data <- clean_data %>%
  mutate(tweet_id = row_number())

# Create tokens with tweet IDs
tokens <- clean_data %>%
  select(tweet_id, cyberbullying_type, tweet_clean) %>%
  unnest_tokens(word, tweet_clean)

# Remove common stop words
tokens_clean <- tokens %>%
  anti_join(stop_words, by = "word")

# Check the result
head(tokens_clean)

# Calculate TF-IDF for each word in each tweet
tfidf_data <- tokens_clean %>%
  count(tweet_id, cyberbullying_type, word, name = "n") %>%
  bind_tf_idf(
    term = word,
    document = tweet_id,
    n = n
  )

# View the results
head(tfidf_data)

# Display the highest TF-IDF scores
tfidf_data %>%
  arrange(desc(tf_idf)) %>%
  select(tweet_id, cyberbullying_type, word, tf_idf) %>%
  head(10)

top_tfidf_words <- tfidf_data %>%
  group_by(word) %>%
  summarise(
    average_tfidf = mean(tf_idf),
    .groups = "drop"
  ) %>%
  slice_max(
    order_by = average_tfidf,
    n = 20,
    with_ties = FALSE
  )

ggplot(
  top_tfidf_words,
  aes(x = reorder(word, average_tfidf),
      y = average_tfidf)
) +
  geom_col(fill = "darkcyan") +
  coord_flip() +
  labs(
    title = "Top 20 Words by Average TF-IDF Score",
    x = "Word",
    y = "Average TF-IDF"
  ) +
  theme_minimal()

summary(tfidf_data$tf)
summary(tfidf_data$idf)
summary(tfidf_data$tf_idf)

top_tfidf_words <- tfidf_data %>%
  group_by(word) %>%
  summarise(
    max_tfidf = max(tf_idf, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  slice_max(max_tfidf, n = 20, with_ties = FALSE)

ggplot(top_tfidf_words,
       aes(x = reorder(word, max_tfidf), y = max_tfidf)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 20 Words by Maximum TF-IDF Score",
    x = "Word",
    y = "Maximum TF-IDF Score"
  ) +
  theme_minimal()

# Check the range of the calculated scores
summary(tfidf_data$tf)
summary(tfidf_data$idf)
summary(tfidf_data$tf_idf)

# Inspect the 10 largest TF-IDF scores
tfidf_data %>%
  arrange(desc(tf_idf)) %>%
  select(tweet_id, word, n, tf, idf, tf_idf) %>%
  head(10)

# Calculate how many different tweets contain each word
word_document_frequency <- tfidf_data %>%
  group_by(word) %>%
  summarise(
    document_count = n_distinct(tweet_id),
    average_tfidf = mean(tf_idf),
    .groups = "drop"
  )

# Keep words appearing in at least 50 tweets
common_tfidf_words <- word_document_frequency %>%
  filter(document_count >= 50) %>%
  slice_max(
    order_by = average_tfidf,
    n = 20,
    with_ties = FALSE
  )

# Plot the results
ggplot(
  common_tfidf_words,
  aes(x = reorder(word, average_tfidf),
      y = average_tfidf)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "TF-IDF Scores of Frequently Occurring Words",
    subtitle = "Words appearing in at least 50 different tweets",
    x = "Word",
    y = "Average TF-IDF Score"
  ) +
  theme_minimal()

library(dplyr)
library(tidytext)
library(ggplot2)

# Count each word within each category
category_tfidf <- tokens_clean %>%
  count(cyberbullying_type, word, name = "n") %>%
  bind_tf_idf(
    term = word,
    document = cyberbullying_type,
    n = n
  )

# Inspect the results
head(category_tfidf)

# Count how many distinct tweets contain each word
word_tweet_counts <- tokens_clean %>%
  distinct(tweet_id, word) %>%
  count(word, name = "tweet_count")

# Keep words appearing in at least 20 tweets
category_tfidf_filtered <- category_tfidf %>%
  inner_join(word_tweet_counts, by = "word") %>%
  filter(tweet_count >= 20)

# Select the top 10 TF-IDF words in each category
top_category_tfidf <- category_tfidf_filtered %>%
  group_by(cyberbullying_type) %>%
  slice_max(
    order_by = tf_idf,
    n = 10,
    with_ties = FALSE
  ) %>%
  ungroup()

# View the selected words
top_category_tfidf %>%
  select(cyberbullying_type, word, tf_idf, tweet_count) %>%
  arrange(cyberbullying_type, desc(tf_idf))

ggplot(
  top_category_tfidf,
  aes(
    x = reorder_within(word, tf_idf, cyberbullying_type),
    y = tf_idf,
    fill = cyberbullying_type
  )
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ cyberbullying_type, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "Distinctive Words Across Cyberbullying Categories",
    subtitle = "Category-level TF-IDF; words must occur in at least 20 tweets",
    x = "Word",
    y = "TF-IDF Score"
  ) +
  theme_minimal()

# Remove very short words and a few obvious text artifacts
tokens_analysis <- tokens_clean %>%
  filter(
    nchar(word) >= 3,
    !grepl("^https?$", word)
  )

# Count words separately for each category
category_word_counts <- tokens_analysis %>%
  count(cyberbullying_type, word, sort = TRUE)

# Select the top 10 words in each category
top_words_by_category <- category_word_counts %>%
  group_by(cyberbullying_type) %>%
  slice_max(
    order_by = n,
    n = 10,
    with_ties = FALSE
  ) %>%
  ungroup()

# Create the graph
ggplot(
  top_words_by_category,
  aes(
    x = reorder_within(word, n, cyberbullying_type),
    y = n,
    fill = cyberbullying_type
  )
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ cyberbullying_type, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "Most Frequent Words Across Cyberbullying Categories",
    x = "Word",
    y = "Frequency"
  ) +
  theme_minimal()

install.packages("caret")       # Run once if needed
install.packages("Matrix")      # Run once if needed

library(caret)
library(Matrix)
library(dplyr)


# Select the columns needed for classification
model_data <- clean_data %>%
  select(tweet_clean, cyberbullying_type) %>%
  filter(
    !is.na(tweet_clean),
    tweet_clean != "",
    !is.na(cyberbullying_type)
  )

# Split the data: 80% training, 20% testing
set.seed(123)

train_index <- createDataPartition(
  model_data$cyberbullying_type,
  p = 0.80,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Check dataset sizes
nrow(train_data)
nrow(test_data)

# Check the distribution of categories
table(train_data$cyberbullying_type)
table(test_data$cyberbullying_type)

install.packages("tm")  # Run only if not installed
library(tm)
library(Matrix)

# Build vocabulary from training tweets only
corpus_train <- VCorpus(
  VectorSource(train_data$tweet_clean)
)

dtm_train <- DocumentTermMatrix(
  corpus_train,
  control = list(
    wordLengths = c(3, Inf),
    weighting = weightTfIdf
  )
)

# Remove terms that occur in very few training tweets
dtm_train <- removeSparseTerms(dtm_train, 0.99)

# Build test matrix using the training vocabulary
corpus_test <- VCorpus(
  VectorSource(test_data$tweet_clean)
)

dtm_test <- DocumentTermMatrix(
  corpus_test,
  control = list(
    dictionary = Terms(dtm_train),
    wordLengths = c(3, Inf),
    weighting = weightTfIdf
  )
)

# Convert to sparse matrices
x_train <- as(dtm_train, "sparseMatrix")
x_test <- as(dtm_test, "sparseMatrix")

# Check dimensions
dim(x_train)
dim(x_test)

dim(dtm_train)
dim(dtm_test)

# Check for empty rows in the training matrix
sum(Matrix::rowSums(as(dtm_train, "sparseMatrix")) == 0)

# Check for empty rows in the testing matrix
sum(Matrix::rowSums(as(dtm_test, "sparseMatrix")) == 0)

# Convert the matrices to sparse matrices
x_train <- as(dtm_train, "sparseMatrix")
x_test <- as(dtm_test, "sparseMatrix")

# Identify rows that contain words
keep_train <- Matrix::rowSums(x_train) > 0
keep_test <- Matrix::rowSums(x_test) > 0

# Remove empty rows
x_train <- x_train[keep_train, , drop = FALSE]
x_test <- x_test[keep_test, , drop = FALSE]

# Keep the correct labels for the remaining tweets
y_train <- factor(train_data$cyberbullying_type[keep_train])
y_test <- factor(
  test_data$cyberbullying_type[keep_test],
  levels = levels(y_train)
)

# Check the new dimensions
dim(x_train)
dim(x_test)
length(y_train)
length(y_test)

install.packages("glmnet")  # Run only if not already installed
library(glmnet)

set.seed(123)

logistic_model <- glmnet(
  x = x_train,
  y = y_train,
  family = "multinomial",
  alpha = 0,
  lambda = 0.01
)

print(logistic_model)

# Predict categories for the test tweets
predictions <- predict(
  logistic_model,
  newx = x_test,
  type = "class",
  s = 0.01
)

# Convert predictions into a factor
predictions <- factor(
  as.vector(predictions),
  levels = levels(y_test)
)

# Display the first 10 predictions
head(predictions, 10)

# Compare predicted categories with actual categories
library(caret)

confusionMatrix(
  data = predictions,
  reference = y_test
)

# Extract overall accuracy
accuracy <- confusionMatrix(
  data = predictions,
  reference = y_test
)$overall["Accuracy"]

# Extract class-wise statistics
evaluation <- confusionMatrix(
  data = predictions,
  reference = y_test
)$byClass

# Display accuracy
print(paste("Test Accuracy:", round(accuracy * 100, 2), "%"))

# Display class-wise precision and recall
print(
  round(
    evaluation[, c("Pos Pred Value", "Sensitivity", "Specificity")],
    3
  )
)

library(ggplot2)

cm <- as.data.frame(
  table(
    Actual = y_test,
    Predicted = predictions
  )
)

ggplot(cm, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Confusion Matrix - Cyberbullying Classification",
    x = "Actual Category",
    y = "Predicted Category",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

library(glmnet)

set.seed(123)

cv_model <- cv.glmnet(
  x = x_train,
  y = y_train,
  family = "multinomial",
  alpha = 0,
  type.measure = "class",
  nfolds = 5
)

# Best lambda selected through cross-validation
print(cv_model$lambda.min)

# Lambda giving a simpler model within one standard error
print(cv_model$lambda.1se)

# Plot cross-validation results
plot(cv_model)

# Predict using the best lambda
tuned_predictions <- predict(
  cv_model,
  newx = x_test,
  s = "lambda.min",
  type = "class"
)

tuned_predictions <- factor(
  as.vector(tuned_predictions),
  levels = levels(y_test)
)

# Evaluate predictions
library(caret)

tuned_results <- confusionMatrix(
  data = tuned_predictions,
  reference = y_test
)

# Overall performance
print(tuned_results$overall)

# Class-wise performance
print(tuned_results$byClass)

# Compare the original and tuned models
model_comparison <- data.frame(
  Model = c("Original Logistic Regression",
            "Tuned Logistic Regression"),
  Accuracy = c(0.7816, as.numeric(tuned_results$overall["Accuracy"])),
  Kappa = c(0.7371, as.numeric(tuned_results$overall["Kappa"]))
)

library(ggplot2)

ggplot(model_comparison, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = sprintf("%.2f%%", Accuracy * 100)),
    vjust = -0.5
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Accuracy Comparison of Logistic Regression Models",
    x = NULL,
    y = "Test Accuracy"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

# Extract F1 scores from both models
original_cm <- confusionMatrix(
  data = predictions,
  reference = y_test
)

original_f1 <- original_cm$byClass[, "F1"]
tuned_f1 <- tuned_results$byClass[, "F1"]

f1_comparison <- data.frame(
  Category = sub("^Class: ", "", names(original_f1)),
  Original = as.numeric(original_f1),
  Tuned = as.numeric(tuned_f1)
)

f1_long <- tidyr::pivot_longer(
  f1_comparison,
  cols = c(Original, Tuned),
  names_to = "Model",
  values_to = "F1"
)

ggplot(f1_long, aes(x = Category, y = F1, fill = Model)) +
  geom_col(position = "dodge") +
  labs(
    title = "Class-wise F1-score Comparison",
    x = "Cyberbullying Category",
    y = "F1-score"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

install.packages("text2vec")  # Run only if not installed
library(text2vec)
library(Matrix)

# Tokenize the training tweets
it_train <- itoken(
  train_data$tweet_clean,
  tokenizer = word_tokenizer,
  progressbar = FALSE
)

# Create a vocabulary of single words and two-word phrases
vocab <- create_vocabulary(
  it_train,
  ngram = c(1L, 2L)
)

# Remove extremely rare and excessively common terms
vocab <- prune_vocabulary(
  vocab,
  term_count_min = 5,
  doc_proportion_max = 0.5
)

# Create the feature matrices using the training vocabulary
vectorizer <- vocab_vectorizer(vocab)

dtm_train2 <- create_dtm(it_train, vectorizer)

it_test <- itoken(
  test_data$tweet_clean,
  tokenizer = word_tokenizer,
  progressbar = FALSE
)

dtm_test2 <- create_dtm(it_test, vectorizer)

# Convert counts to TF-IDF using training-set weights
tfidf <- TfIdf$new()
x_train2 <- fit_transform(dtm_train2, tfidf)
x_test2 <- transform(dtm_test2, tfidf)

# Remove empty rows while preserving labels
keep_train2 <- Matrix::rowSums(x_train2) > 0
keep_test2 <- Matrix::rowSums(x_test2) > 0

x_train2 <- x_train2[keep_train2, , drop = FALSE]
x_test2 <- x_test2[keep_test2, , drop = FALSE]

y_train2 <- factor(
  train_data$cyberbullying_type[keep_train2]
)

y_test2 <- factor(
  test_data$cyberbullying_type[keep_test2],
  levels = levels(y_train2)
)

# Check the new feature dimensions
dim(x_train2)
dim(x_test2)
length(y_train2)
length(y_test2)

install.packages("LiblineaR")  # Run only if not already installed
library(LiblineaR)

set.seed(123)

svm_model <- LiblineaR(
  data = x_train2,
  target = y_train2,
  type = 1,       # L2-regularized L2-loss SVC
  cost = 1,
  bias = TRUE,
  verbose = TRUE
)

print("SVM training completed!")

svm_predictions <- predict(
  svm_model,
  x_test2
)$predictions

svm_predictions <- factor(
  svm_predictions,
  levels = levels(y_test2)
)

svm_results <- caret::confusionMatrix(
  data = svm_predictions,
  reference = y_test2
)

# Overall results
print(svm_results$overall)

# Class-wise precision, recall and F1-score
print(svm_results$byClass)

library(LiblineaR)

set.seed(123)

# Compare different regularization strengths
cost_values <- c(0.01, 0.1, 1, 10)

svm_tuning <- lapply(cost_values, function(c_value) {
  model <- LiblineaR(
    data = x_train2,
    target = y_train2,
    type = 1,
    cost = c_value,
    bias = TRUE,
    verbose = FALSE
  )
  
  # Training accuracy is only a diagnostic, not the selection metric
  pred <- predict(model, x_train2)$predictions
  
  data.frame(
    Cost = c_value,
    TrainingAccuracy = mean(pred == y_train2)
  )
})

svm_tuning_results <- do.call(rbind, svm_tuning)
print(svm_tuning_results)


class(x_train2)
class(x_test2)



library(caret)
library(Matrix)

set.seed(123)

# Create training and validation indices
sub_index <- as.vector(
  createDataPartition(y_train2, p = 0.85, list = FALSE)
)

# Convert indices to integer
sub_index <- as.integer(sub_index)

# Split the sparse matrix
x_subtrain <- x_train2[sub_index, , drop = FALSE]
x_valid <- x_train2[-sub_index, , drop = FALSE]

# Split labels
y_subtrain <- y_train2[sub_index]
y_valid <- y_train2[-sub_index]

# Check the results
print(dim(x_subtrain))
print(dim(x_valid))
print(length(y_subtrain))
print(length(y_valid))

library(LiblineaR)
library(caret)

cost_values <- c(0.01, 0.1, 1, 10)

validation_results <- lapply(cost_values, function(c_value) {
  
  cat("Testing cost:", c_value, "\n")
  
  model <- LiblineaR(
    data = x_subtrain,
    target = y_subtrain,
    type = 1,
    cost = c_value,
    bias = TRUE,
    verbose = FALSE
  )
  
  predictions <- predict(model, x_valid)$predictions
  
  cm <- confusionMatrix(
    data = factor(predictions, levels = levels(y_valid)),
    reference = y_valid
  )
  
  data.frame(
    Cost = c_value,
    ValidationAccuracy = unname(cm$overall["Accuracy"]),
    Kappa = unname(cm$overall["Kappa"])
  )
})

validation_results <- do.call(rbind, validation_results)

print(validation_results)

library(LiblineaR)
library(caret)

# Train the final model on the full training set
set.seed(123)

final_svm_model <- LiblineaR(
  data = x_train2,
  target = y_train2,
  type = 1,
  cost = 0.1,
  bias = TRUE,
  verbose = FALSE
)

# Predict on the untouched test set
final_predictions <- predict(
  final_svm_model,
  x_test2
)$predictions

final_predictions <- factor(
  final_predictions,
  levels = levels(y_test2)
)

# Evaluate the final model
final_results <- confusionMatrix(
  data = final_predictions,
  reference = y_test2
)

print(final_results)

library(ggplot2)
library(dplyr)
library(tidyr)

# Extract class-wise results from the final SVM
class_metrics <- as.data.frame(
  final_results$byClass
)

class_metrics$Class <- rownames(class_metrics)

# Keep precision, recall and F1-score
plot_metrics <- class_metrics %>%
  transmute(
    Class = sub("Class: ", "", Class),
    Precision = `Pos Pred Value`,
    Recall = Sensitivity,
    F1_Score = 2 * Precision * Recall /
      (Precision + Recall)
  ) %>%
  pivot_longer(
    cols = c(Precision, Recall, F1_Score),
    names_to = "Metric",
    values_to = "Score"
  )

ggplot(
  plot_metrics,
  aes(x = Class, y = Score * 100, fill = Metric)
) +
  geom_col(
    position = "dodge",
    width = 0.75
  ) +
  labs(
    title = "Class-wise Performance of SVM",
    x = "Cyberbullying Category",
    y = "Score (%)",
    fill = "Metric"
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    ),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

library(ggplot2)

# Compare the test accuracies
model_comparison <- data.frame(
  Model = c(
    "Logistic Regression",
    "SVM - Previous",
    "SVM - Final"
  ),
  Accuracy = c(
    78.16,
    82.66,
    82.86
  )
)

ggplot(
  model_comparison,
  aes(x = Model, y = Accuracy, fill = Model)
) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(
    aes(label = paste0(Accuracy, "%")),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "Accuracy Comparison of Classification Models",
    x = "Model",
    y = "Accuracy (%)"
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 15,
      hjust = 1
    ),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

# Create a folder for the project results
output_dir <- "Cyberbullying_Results"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 1. Save the trained SVM model
saveRDS(
  final_svm_model,
  file.path(output_dir, "final_svm_model.rds")
)

# 2. Save the test predictions and actual labels
prediction_results <- data.frame(
  Actual = y_test2,
  Predicted = final_predictions
)

write.csv(
  prediction_results,
  file.path(output_dir, "svm_test_predictions.csv"),
  row.names = FALSE
)

# 3. Save the confusion matrix as a CSV
write.csv(
  as.matrix(final_results$table),
  file.path(output_dir, "svm_confusion_matrix.csv")
)

# 4. Save overall performance metrics
overall_metrics <- data.frame(
  Metric = names(final_results$overall),
  Value = as.numeric(final_results$overall)
)

write.csv(
  overall_metrics,
  file.path(output_dir, "svm_overall_metrics.csv"),
  row.names = FALSE
)

# 5. Save class-wise performance metrics
class_metrics <- as.data.frame(final_results$byClass)
class_metrics$Class <- rownames(class_metrics)

write.csv(
  class_metrics,
  file.path(output_dir, "svm_class_metrics.csv"),
  row.names = FALSE
)

# Confirm saved files
list.files(output_dir)
getwd()
list.files(pattern = "\\.html$",recursive = TRUE)