#Classification model Project
setwd("C:/Users/asare kwasi/Desktop/EntryLevel/R/PDSwR2-main/PDSwR2-main/Project/Spam")
SPambase <- readr::read_tsv("spamD.tsv")
View(SPambase)

#Building and applying logistic spam model
#Split the data into test and train dataset
spamTrain <- subset(SPambase, SPambase$rgroup >= 10)
spamTest <- subset(SPambase, SPambase$rgroup < 10)

#Create a formula that describe the model
spamVars <- setdiff(colnames(SPambase), list('rgroup','spam'))
spamFormula <- as.formula(paste('spam == "spam"',paste(spamVars, collapse = '+'),
                                sep = '~'))
#Fit the logistic regression model
spamModel <- glm(spamFormula, family = binomial(link = 'logit'),data = spamTrain)

#Make predictions on the training and test dataset
spamTrain$pred <- predict(spamModel, newdata = spamTrain, type = 'response')
spamTest$pred <- predict(spamModel, newdata = spamTest, type = 'response')

#Test the model
sample <- spamTest[c(7,35,224,327), c('spam','pred')]
print(sample)

#Spam confusion matrix
confmat_spam <- table(truth = spamTest$spam, prediction = ifelse(spamTest$pred > 0.5,
                          "spam", "non-spam"))
print(confmat_spam)
#for accuracy
accuracy_confmat_spam <- (confmat_spam[1,1] + confmat_spam[2,2])/sum(confmat_spam)
print(accuracy_confmat_spam)

#Probability error for confmat_spam
error_confmat_spam <- (1-accuracy_confmat_spam)
print(error_confmat_spam)

#calculating precision: Precision answers the question, "If the spam filter says this email is spam, what's the
#probability that it's really spam?"
(confmat_spam[2,2])/(confmat_spam[2,2]+ confmat_spam[1,2])

#calculating recall:Recall answers the question, "Of all the spam in the email set, what fraction did the spam filter detect?"
(confmat_spam[2,2])/(confmat_spam[2,2] + confmat_spam[2,1])

#Evaluating akismet confusion matrix by hand
confmat_akismet <- as.table(matrix(data = c(288-1,17,1,13882-17), nrow = 2, ncol = 2))
rownames(confmat_akismet) <- rownames(confmat_akismet)
colnames(confmat_akismet) <- colnames(confmat_akismet)
print(confmat_akismet)

#for accuracy in akismet data
accuracy_confmat_akismet <- (confmat_akismet[1,1]+ confmat_akismet[2,2])/sum(confmat_akismet)
print(accuracy_confmat_akismet)

# Probability error
error_confmat_akismat <- (1- accuracy_confmat_akismet)
print(error_confmat_akismat)

#Calculating precision: Precision answers the question, "If the spam filter says this email is spam, what's the
#probability that it's really spam?"
(confmat_akismet[2,2])/(confmat_akismet[2,2] +confmat_akismet[1,2])

#calculating recall:Recall answers the question, "Of all the spam in the email set, what fraction did the spam filter detect?"
(confmat_akismet[2,2])/(confmat_akismet[2,2]+ confmat_akismet[2,1])

##Suppose that you had multiple spam filters to choose from, each with different
#values of precision and recall. How do you pick which spam filter to use?
precision <- confmat_spam[2,2]/(confmat_spam[2,2] + confmat_spam [1,2])
recall <- confmat_spam[2,2]/(confmat_spam[2,2] + confmat_spam[2,1])
(F1 <- 2*precision*recall/(precision + recall))
