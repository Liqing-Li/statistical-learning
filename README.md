# statistical-learning

#Notes for Variable Selection :

-subset selection
-Lasso
-Ridge
-Example
#Notes for Nonlinear regression
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
