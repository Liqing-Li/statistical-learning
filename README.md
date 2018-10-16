# statistical-learning

# Notes for Variable Selection :

-subset selection
-Lasso
-Ridge
-Example


# Notes for Regression Tree:

- fit reg tree
- pruning
- choosing cp (connection btw cp and alpha)
- handle categorical predictors



# Notes for Random forest and GBM regression

• Advantages of ensemble methods based on trees
– Less-processing is needed, e.g., NA can be handled
automatically, and no scaling/normalization is required – Can handle large number of predictors
• GBM vs randomForest
– randomForest has less number of tuning parameters, while GBM has more, but 

# RandomForest

- Fit a model
- two types of training error
- OOB samples
- variable importance

# GBM

- Fit a model
- add shrinkage effect
- test error
- variable importance


# Notes for Nonlinear regression

• Polynomial regression 
  - choice of d (forward/backward/ AIC BIC)
  
• Spline regression
  - Basis function
  - how to select df (number of knots)
  
• Smoothing splines
  - Demmler & Reinsch Basis
  - LOO-CV and GCV to pick lambda value (df)
  
• Local regression
  - kernel (local reg with only intercept) and Use CV to select bandwidth
  - local linear or poly reg (Loess)






#Notes for Clustering

Two types of inputs for clustering : Data matrix (X), distance matrix(D)
- X to D : easy with a given distance measure
- D to X :e.g. multi-dimensional scaling (MDS)

clustering methods:
- K-means
- K-medoids, Partition aorund medoids

Methods for selecting K:
- Gap statistics
- Silhouettes statistics
- Prediction strength
