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
library(randomForest)
library(vip)
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

##### Fit & assess Decision Tree (no cross-validation)

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

allResults <- allTreeFit %>%
	pull_workflow_fit() %>%
  vip(num_features = 30)
allResults  # Zip codes & states not important
allTrngPred <- predict(allTreeFit, new_data = Train)
allTrngAcc <- accuracy(Train, truth = Product, estimate = allTrngPred$.pred_class) 
allTrngAcc  # 79.7% accurate
count(Train, Product)
count(allTrngPred, .pred_class)

##### Decision Tree with cross-validation and tuning

vfTrain <- vfold_cv(data = Train, v = 10)
vfTree <- decision_tree(cost_complexity = tune(),tree_depth = tune()) %>%
	set_mode("classification") %>%
	set_engine("rpart")
vfTreeGrid <- grid_regular(cost_complexity(), tree_depth(), levels = 5)
vfTreeWork <- workflow() %>%
	add_recipe(allRecipe) %>%
	add_model(vfTree)
vfTreeWorkGrid <- vfTreeWork %>%
	tune_grid(resamples = vfTrain, grid = vfTreeGrid)
vfResults <- collect_metrics(vfTreeWorkGrid)
vfBest <- show_best(vfTreeWorkGrid, metric = "accuracy") 

## https://www.tidymodels.org/start/tuning/#data

##### Random Forest run

RFmodel <- rand_forest(mtry=10, min_n = 4) %>%
  set_engine("randomForest") %>%
  set_mode("classification")
RFwork <- workflow() %>%
  add_recipe(allRecipe) %>%
  add_model(RFmodel)
RFfit <- fit(RFwork, data = Train)

RFresults <- RFfit %>%
  pull_workflow_fit() %>%
  vip(num_features = 30)
RFresults  # Zip codes & states not important
RFTrngPred <- predict(RFfit, new_data = Train)
RFTrngAcc <- accuracy(Train, truth = Product, estimate = RFTrngPred$.pred_class) 
RFTrngAcc  # 97.3% accurate
count(Train, Product)
count(RFTrngPred, .pred_class)

Train2 <- Train %>%
  select(-State, -ZIP.code)
modRecipe <- Train2 %>%
  recipe(Product ~ .) %>%
  step_dummy(Company, one_hot = TRUE) %>%
  step_nzv(all_predictors())
RFwork2 <- workflow() %>%
  add_recipe(modRecipe) %>%
  add_model(RFmodel)
RFfit2 <- fit(RFwork2, data = Train2)

RFresults2 <- RFfit2 %>%
  pull_workflow_fit() %>%
  vip(num_features = 30)
RFresults2  # Zip codes & states not important
RFTrngPred2 <- predict(RFfit2, new_data = Train2)
RFTrngAcc2 <- accuracy(Train2, truth = Product, estimate = RFTrngPred2$.pred_class) 
RFTrngAcc2  # 96.7% accurate
count(Train2, Product)
count(RFTrngPred2, .pred_class)

##### Random Forest with cross-validation and tuning

vfRF <- vfold_cv(data = Train2, v = 10)
vfRFmodel <- rand_forest(mtry = tune(), min_n = 4) %>%
  set_engine("randomForest") %>%
  set_mode("classification")
vfRFwork <- workflow() %>%
  add_recipe(modRecipe) %>% 
  add_model(vfRFmodel)
vfRFworkGrid <- vfRFwork %>%
  tune_grid(resamples = vfRF, grid = 3)

vfResults <- collect_metrics(vfRFworkGrid)
vfBest <- show_best(vfRFworkGrid, metric = "accuracy") 
vfBest
vfFinal <- finalize_workflow(vfRFwork, select_best(vfRFworkGrid)) %>%
  fit(Train2)
vfFinalPred <- predict(vfFinal, new_data = Train2)
vfFinalAcc <- accuracy(Train2, truth = Product, estimate = vfFinalPred$.pred_class) 
vfFinal  # gives OOB error and class error 
vfFinalAcc #97.1 accurate
count(Train2, Product)
count(vfFinalPred, .pred_class)

##### Load and check test set
	
test <- read_csv("data_complaints_test.csv")
names(test) <- make.names(names(test), unique = TRUE)
skim(test) 

# No NAs or missing variables; test set has a problem id variable in place of product

##### Parse/tokenize/stem test set complaints

test <- test %>%
	select(-Submitted.via, -State, -ZIP.code) %>%
	mutate(across(c(Company), as.factor))
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
	select(-Product, -temp_id, -State, -ZIP.code)
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
  mutate(navient = 0) %>%
	select(names(tempTrain))

##### Fit test data to final model

vfFinalTestPred <- predict(vfFinal, new_data = tidyTest)
vfFinalTestPred