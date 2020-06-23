# Installing the required libraries

if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

seasons_stats <- read_csv("data/seasons_stats.csv", col_types = cols())
all_nba <- read_csv("data/all_nba_teams.csv", col_types = cols())

# Cleaning all_nba table to join with seasons_stats
all_nba <- all_nba %>% mutate(all_nba=1)
all_nba$Player = gsub(" \\(tie\\)", "", all_nba$Player) # Remove tie text
# Changing name differences between data sets
all_nba$Player = gsub("Neil Johnson", "Neil Johnston", all_nba$Player) # Change Neil Johnston name
all_nba$Player = gsub("KarlMalone", "Karl Malone", all_nba$Player) # Change Karl Malone name
all_nba$Player = gsub("Benard King", "Bernard King", all_nba$Player) # Change Bernard King name
all_nba$Player = gsub("World B. Free", "World B.", all_nba$Player) # Change World B. Free name
all_nba$Player = gsub("Jo Jo White", "Jo Jo", all_nba$Player) # Change Jo Jo White name
all_nba$Player = gsub("Norm Van Lier", "Norm Van", all_nba$Player) # Change Norm Van Lier name
all_nba$Player = gsub("Gus Johnsson", "Gus Johnson", all_nba$Player) # Change Gus Johnson name
all_nba$Player = gsub("Elgin baylor", "Elgin Baylor", all_nba$Player) # Change Elgin Baylor name
all_nba$Player = gsub("Richie Griffin", "Richie Guerin", all_nba$Player) # Change Richie Guerin name


# Cleaning seasons_stats data
# 1. Removing * from players name in seasons_stats
seasons_stats$Player = gsub("\\*", "", seasons_stats$Player)

# 2. Removing Player with multiple seasons, we just keep the TOT register which has the stats
#    for the complete season.
changed_players <- seasons_stats %>%
  filter(Tm=='TOT') %>% 
  select(Year, Player) %>%
  inner_join(seasons_stats, by = c('Year', 'Player'))

stats_to_remove <- changed_players %>% filter(Tm!='TOT') %>% select(Year, Player, Tm)
seasons_stats <- seasons_stats %>% anti_join(stats_to_remove, by = c('Year', 'Player', 'Tm'))

# 3. Removing NA columns
seasons_stats <- seasons_stats %>%
  select(
    -'X1', -'3PAr', -'ORB%', -'DRB%', -'TRB%', -'AST%', -'STL%', -'BLK%', -'TOV%', -'USG%', -'blanl',
    -'blank2', -'OBPM', -'DBPM', -'BPM', -'VORP', -'3P', -'3PA', -'3P%', -'ORB', -'DRB', -'STL', -'BLK'
  )

# Merge the two datasets
full_set <- seasons_stats %>% left_join(all_nba, by = c('Year', 'Player')) %>% filter(Year>=1952)
full_set[is.na(full_set)] <- 0 # replace NAs with 0
full_set$all_nba <- as.factor(full_set$all_nba) # Make all_nba column a factor

# Removing temporal objects
rm('changed_players', 'stats_to_remove')

# Given the nature of the dataset we are not going to create a random
# validation set but we are going to take the last three seasons available
# 2015, 2016 and 2017, we are going to train our models using all the seasons
# before that one and then we are going to validate the model against
# this two seasons.
train_set <- full_set %>% filter(Year<2015)
test_set <- full_set %>% filter(Year>=2015)


# Finding Principal Components using PCA (Principal Components Analysis)
train_matrix <- train_set %>% select(-Year, -Player, -Age, -Pos, -Tm, -all_nba) %>% as.matrix()
train_matrix <- sweep(train_matrix, 1, rowMeans(train_matrix))
pca <- prcomp(train_matrix)

# Looking how may variables explain the data
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

# Looking for information clusters
pcs <- data.frame(pca$rotation, name = colnames(train_matrix))
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = pcs)

# select only the columns we will use for the analysis
full_set <- full_set %>% select(Year, Player, MP, PTS, OWS, DWS, all_nba)

# Generating a basic logistic regression model
fit <- glm(all_nba ~ MP + PTS + OWS + DWS, data = train_set, family = binomial)
y_hat <- ifelse(predict(fit, train_set) > 0, 1, 0) %>% factor()
# F1 Score
F_meas(y_hat, factor(train_set$all_nba))
confusionMatrix(y_hat, train_set$all_nba %>% factor())


# Generating a basic Knn model
train_knn <- train(all_nba ~ MP + PTS + OWS + DWS, method = "knn", data = train_set)
knn_y_hat <- predict(train_knn, train_set)
# F1 Score
F_meas(knn_y_hat, factor(train_set$all_nba))
confusionMatrix(knn_y_hat, train_set$all_nba)


# Now we use rpart to create a decision tree
train_rpart <- train(all_nba ~ MP + PTS + OWS + DWS, method = "rpart", data = train_set)
rpart_y_hat <- predict(train_rpart, train_set)
# F1 score
F_meas(rpart_y_hat, factor(train_set$all_nba))
confusionMatrix(rpart_y_hat, train_set$all_nba %>% factor())
# Plotting the tree
plot(train_rpart$finalModel)
text(train_rpart$finalModel)

# Calculating the results of running the algorithms in the training set
lg_results <- ifelse(predict(fit, test_set) > 0, 1, 0) %>% factor()
knn_results <- predict(train_knn, test_set)
dt_results <- predict(train_rpart, test_set)

results <- tibble(method=character(), score=double())
results <- bind_rows(results,
  tibble(method='Logistic Regression', score=F_meas(lg_results, factor(test_set$all_nba))),
  tibble(method='Knn', score=F_meas(knn_results, factor(test_set$all_nba))),
  tibble(method='Decision Tree', score=F_meas(dt_results, factor(test_set$all_nba)))
)

results

# Plotting the confusion matrix
fourfoldplot(confusionMatrix(lg_results, test_set$all_nba)$table, color = c("#FF6666", "#6666FF"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

false_positives <- test_set %>%
  filter(all_nba==0 & all_nba!=lg_results) %>%
  select(Year, Player, Pos, MP, PTS, OWS, DWS)
false_negatives <- test_set %>%
  filter(all_nba==1 & all_nba!=lg_results) %>%
  select(Year, Player, Pos, MP, PTS, OWS, DWS)
