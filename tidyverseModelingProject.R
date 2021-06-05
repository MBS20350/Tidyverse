## Tidyverse modeling project
## Mark Sucato

library(tidyverse)
library(tidymodels)
library(skimr)
library(tidytext)
library(corpus)
library(tm)
library(corrplot)
library(rpart)
set.seed(1234)

##### Load and check training set

train <- read_csv("data_complaints_train.csv")
names(train) <- make.names(names(train), unique = TRUE) 
skim(train)   # No NAs or missing variables, Submitted.via all web

##### Parse/tokenize/stem training set complaints

train <- train %>%
	select(-Submitted.via) %>%
	mutate(across(c(Product, Company, State, ZIP.code), as.factor)) %>%
	mutate(temp_id = c(1:length(Product)))
parsedPreProc <- train %>%
  select(temp_id, Product, Consumer.complaint.narrative) %>%
  unnest_tokens(word, Consumer.complaint.narrative) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "xx")) %>%
  filter(!str_detect(word, "\\d")) %>%
  filter(!str_detect(word, "\\s")) %>%
  mutate(word = text_tokens(word, stemmer = "en")) 
parsedCard <- parsedPreProc %>%
  filter(Product == "Credit card or prepaid card") %>%
  select(-Product) %>%
  count(temp_id, word) %>%
  group_by(word) %>%
  summarise (total = sum(n)) %>%
  arrange(desc(total)) %>%
  slice(1:15)
parsedVeh <- parsedPreProc %>%
  filter(Product == "Vehicle loan or lease") %>%
  select(-Product) %>%
  count(temp_id, word) %>%
  group_by(word) %>%
  summarise (total = sum(n)) %>%
  arrange(desc(total)) %>%
  slice(1:15)
parsedMortg <- parsedPreProc %>%
  filter(Product == "Mortgage") %>%
  select(-Product) %>%
  count(temp_id, word) %>%
  group_by(word) %>%
  summarise (total = sum(n)) %>%
  arrange(desc(total)) %>%
  slice(1:15)
parsedStu <- parsedPreProc %>%
  filter(Product == "Student loan") %>%
  select(-Product) %>%
  count(temp_id, word) %>%
  group_by(word) %>%
  summarise (total = sum(n)) %>%
  arrange(desc(total)) %>%
  slice(1:15)
keywords1 <- full_join(parsedCard, parsedMortg)
keywords2 <- full_join(parsedStu, parsedVeh)
keywords <- full_join(keywords1, keywords2) %>%
  select(word) %>%
  distinct(word)
parsedTrain <- train %>%
	select(temp_id, Consumer.complaint.narrative) %>%
	unnest_tokens(word, Consumer.complaint.narrative) %>%
	mutate(word = text_tokens(word, stemmer = "en")) %>%
  semi_join(keywords) %>%
	count(temp_id, word)  
trainDTM <- parsedTrain %>%
	cast_dtm(temp_id, word, n)                 # 42,173 terms / 28 terms new idea
tidyTrain <- trainDTM %>%
	tidy() %>%
	rename(temp_id = document) %>%
	pivot_wider(names_from = term, 
			values_from = count, 
			names_repair = "minimal", 
			values_fill = 0) %>%
	mutate(temp_id = as.numeric(temp_id)) %>%
	arrange(temp_id) %>%
	left_join(train, by = "temp_id") %>%
	select(-Consumer.complaint.narrative)
Train <- tidyTrain %>%
	select(-temp_id)

# Check Train correlation

trainCor <- cor(Train %>% select_if(is.numeric))
corrplot(trainCor, tl.cex = 0.5)

# Not much correlation

##### Fit Decision Tree (no cross-validation)

allRecipe <- Train %>%
	recipe(Product ~ .) %>%
	step_dummy(Company, State, ZIP.code, one_hot = TRUE) %>%
	step_nzv(all_predictors())
tree <- decision_tree() %>%
	set_mode("classification") %>%
	set_engine("rpart")
allTreeWork <- workflow() %>%
	add_recipe(allRecipe) %>%
	add_model(tree)
allTreeFit <- fit(allTreeWork, data = Train)

##### Assess Decision Tree (no cross-validation)

allResults <- allTreeFit %>%
	pull_workflow_fit()
allResults$fit$variable.importance  # Shows influence of specific variables
allTrngPred <- predict(allTreeFit, new_data = Train)
allTrngAcc <- accuracy(Train, truth = Product, estimate = allTrngPred$.pred_class) 
allTrngAcc  # 77% accurate w/ DTM75, 80.1% with DTM85
count(Train, Product)
count(allTrngPred, .pred_class)

##### Fit Decision Tree (with cross-validation)

vfTrain <- vfold_cv(data = Train, v = 10)
vfTree <- decision_tree(
		cost_complexity = tune(),
		tree_depth = tune()) %>%
	set_mode("classification") %>%
	set_engine("rpart")
vfTreeGrid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)
vfTreeWork <- workflow() %>%
	add_recipe(allRecipe) %>%
	add_model(vfTree)
vfTreeWorkGrid <- vfTreeWork %>%
	tune_grid(
	resamples = vfTrain,
	grid = vfTreeGrid)
vfResults <- collect_metrics(vfTreeWorkGrid)
vfBest <- show_best(vfTreeWorkGrid, metric = "accuracy") 

## https://www.tidymodels.org/start/tuning/#data


##### Load and check test set
	
test <- read_csv("data_complaints_test.csv")
names(test) <- make.names(names(test), unique = TRUE)
skim(test) 

# No NAs or missing variables; test set has a problem id variable in place of product

##### Parse/tokenize/stem test set complaints

test <- test %>%
	select(-Submitted.via) %>%
	mutate(across(c(Company, State, ZIP.code), as.factor))
parsedTest <- test %>%
 	select(problem_id, Consumer.complaint.narrative) %>%
	unnest_tokens(word, Consumer.complaint.narrative) %>%
	anti_join(stop_words) %>%
	filter(!str_detect(word, "xx")) %>%
	filter(!str_detect(word, "\\d")) %>%
	filter(!str_detect(word, "\\s")) %>%
	mutate(word = text_tokens(word, stemmer = "en")) %>%
	count(problem_id, word)
testDTM <- parsedTest %>%
	cast_dtm(problem_id, word, n)              # 734 terms   
tempTrain <- tidyTrain %>%
	mutate(problem_id = temp_id) %>%
	select(-Product, -temp_id)
tidyTest <- testDTM %>%
	tidy() %>%
	rename(problem_id = document) %>%
	pivot_wider(names_from = term, 
			values_from = count, 
			names_repair = "minimal", 
			values_fill = 0) %>%
	mutate(problem_id = as.numeric(problem_id)) %>%
	arrange(problem_id) %>%
	left_join(test, by = "problem_id") %>%
	select(-Consumer.complaint.narrative) %>%
	select(names(tempTrain))


