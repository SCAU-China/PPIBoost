#################################
#
#    author: Ken Wang
#    e-mail:ken711198@hotmail.com
#
#################################

# we also offer S kernel SVM, proposed by shen.et.al 2007 PNAS
SkernelSVM <- function(x_train, x_test,y_train, gamma){
#  training dataset  (m*686)
#  test dataset    (n*686)
#  y_train: the output of x_train, data type is factor
#  output is SVM prediction result

#  S kernel Function
editKernel <- function(x,y,gamma=gamma){
  x1 <- x[1:343]
  x2 <- x[344:686]
  y1 <- y[1:343]
  y2 <- y[344:686]
  
  S <- min(c((sum((x1-y1)^2)+sum((x2-y2)^2)),(sum((x1-y2)^2)+sum((x2-y1)^2))))
  K <- exp(-S*gamma)
  K
}

#######
Ktrain <- kernelMatrix(editKernel,x_train)
Ktest <- kernelMatrix(editKernel, x_train, x_test)
svm <- ksvm(Ktrain, y_train, type="C-svc")
Ktest <- t(Ktest)
pre <- predict(svm,Ktest)

return pre
}