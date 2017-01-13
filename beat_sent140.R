# Ebert
# 12 January 2016

# Build a model that DOMINATES sentiment140

# Max_imbalance Lexicon:
load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/max_imbalance_lexicon.RData")

# Data
sent140 = get_sent140()

# Make term_freq matrix
sent140_tf_max_imbalance = make_term_freq(sent140, max_imbalance_lexicon)
save(sent140_tf_max_imbalance, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/sent140_tf_max_imbalance.RData")
load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/sent140_tf_max_imbalance.RData")


# Indices 
set.seed(1234)
sent140_indices = sample(359,251)
save(sent140_indices, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/sent140_indices.RData")
load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/sent140_indices.RData")


# Try even making a sent140-specific lexicon: <- NOPE. This is cheating. Model is biased unfairly against words like "Warner" 
ndsi_lexicon_df_sent140 = make_ndsi_lexicon(sent140[sent140_indices,], max_words = 10000)
ndsi_lexicon_df_sent140



# THIS IS THE BESTEST MODEL HERE:
# bestest_sent140_model = randomForest(polarity~.,data = sent140_tf_max_imbalance[sample(sent140_indices),c(1:4654)])
save(bestest_sent140_model, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/bestest_sent140_model.RData")
load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/bestest_sent140_model.RData")

test_phat = predict(bestest_sent140_model, newdata = sent140_tf_max_imbalance[-sent140_indices,c(1:4654)], type = "prob")
auc = roc(sent140_term_freq$polarity[-sent140_indices],test_phat[,2])
pred = prediction(test_phat[,2], sent140_term_freq$polarity[-sent140_indices])
acc_list = performance(pred,"acc")
max_acc = max(unlist(acc_list@y.values))
print(paste("     Sent140 AUC:", as.numeric(auc$auc)))
print(paste("Sent140 accuracy:", max_acc))



# Data for Parker's graph:
data_for_parker_sent140 = rbind(sent140_term_freq$polarity[-sent140_indices], test_phat[,2])
save(data_for_parker_sent140, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/data_for_parker_sent140.RData")
load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/data_for_parker_sent140.RData")
# ACC: 0.851851851851852
# AUC: 0.9259




