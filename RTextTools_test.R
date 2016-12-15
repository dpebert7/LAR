#RTextTools trial

library(RTextTools)


library(RTextTools)
data(NYTimes)
data <- NYTimes[sample(1:3100,size=100,replace=FALSE),]
matrix <- create_matrix(cbind(data["Title"],data["Subject"]), language="english",
                        removeNumbers=TRUE, stemWords=FALSE, weighting=tm::weightTfIdf)
container <- create_container(matrix,data$Topic.Code,trainSize=1:75, testSize=76:100,
                              virgin=FALSE)
models <- train_models(container, algorithms=c("MAXENT","SVM"))
results <- classify_models(container, models)
analytics <- create_analytics(container, results)


analytics@label_summary
analytics@document_summary