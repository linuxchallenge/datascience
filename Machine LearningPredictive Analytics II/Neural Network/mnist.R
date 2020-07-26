
# MNIST Data in csv form from Kaggle - https://www.kaggle.com/c/digit-recognizer/data?train.csv
mnistTrainFull <- read.csv("mnist-train-full.csv")
mnistTestFull <- read.csv("mnist-test-full.csv")

# Reduce training size to cut down on time - split the training data into training and validation
subsamplesize <- 2000
subsample <- sample(nrow(mnistTrainFull), subsamplesize, replace=FALSE)
validationsize <- subsamplesize*0.3
validationsample <- subsample[1:validationsize]
trainsample <- subsample[(validationsize+1):subsamplesize]

mnistValidation <- mnistTrainFull[validationsample,]
mnistTrain <- mnistTrainFull[trainsample,]
mnistTest <- mnistTestFull[sample(nrow(mnistTestFull), (subsamplesize*0.5)),]
write.csv(mnistTrain, file='mnist-train.csv', row.names=FALSE)
write.csv(mnistValidation, file='mnist-validation.csv', row.names=FALSE)
write.csv(mnistTest, file='mnist-test.csv', row.names=FALSE)

# Plot the digits in a grid
  previewNbyN <- 10
  nsamples <- previewNbyN*previewNbyN

  # Pick a sample from the training set
  mnistTrain = read.csv("mnist-train.csv")
  viewSample <- mnistTrain[sample(nrow(mnistTrain), nsamples),]

  # Arrange the digit images in a grid
  par( mfrow = c(previewNbyN, previewNbyN), mai = c(0,0,0,0))
  imageRows <- 28
  imageCols <- 28
  for(i in 1:nsamples) {
    digit <- as.matrix(viewSample[i, 2:ncol(viewSample)], byrow=TRUE)
    dim(digit) <- c(imageRows, imageCols)
    image(digit[,ncol(digit):1], axes=FALSE, col=gray(255:0 / 255))
    text( 0.2, 0.8, viewSample[i,1], cex=2, col=2)
  }
  
# Initialize the h2o environment
library(h2o)
h2o.init() #ip = "localhost", port = 54321, nthreads=-1, startH2O = TRUE, min_mem_size = "3g")

mnistTrain <- h2o.importFile("mnist-train.csv")
mnistValidation <- h2o.importFile("mnist-validation.csv")
mnistTest <- h2o.importFile("mnist-test.csv")
# Convert the first column to a factor
digitlabel <- "label"
pixels <- setdiff(names(mnistFull), digitlabel)
mnistTrain[,digitlabel] <- as.factor(mnistTrain[,digitlabel])
mnistValidation[,digitlabel] <- as.factor(mnistValidation[,digitlabel])

set.seed(1105)
# Perform 5-fold cross-validation on the training_frame
mnistNet <- h2o.deeplearning(x = pixels,
                             y = digitlabel,
                             training_frame = mnistTrain,
                             validation_frame = mnistValidation,
                             distribution = "multinomial",
                             activation = "RectifierWithDropout",
                             hidden = c(200,200,200),
                             hidden_dropout_ratio = c(0.1, 0.1, 0.1),
                             l1 = 1e-5,
                             epochs = 20)

# Test the model on the test data
digitPrediction <- h2o.predict(mnistNet, mnistTest)
ImageId <- as.numeric(seq(1,nrow(mnistTest)))
names(ImageId)[1] <- "ImageId"
predictions <- cbind(as.data.frame(ImageId),as.data.frame(digitPrediction[,1]))
names(predictions)[2] <- "Label"
write.table(as.matrix(predictions), file="DNN_pred.csv", row.names=FALSE, sep=",")

