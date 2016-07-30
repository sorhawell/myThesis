library(forestFloor)
library(randomForest)
library(rgl)

#make data
X = data.frame(replicate(2,runif(2500)-.5))
y = +(X[,1]^2 + X[,2]^3)^2 - (X[,1]^2 + X[,2]^3)

#make color vector/gradient by y vector
ycolor = fcol(y,RGB = F,alpha=1)
plot(X,col=ycolor)
#or plot in 3D
plot3d(x= X[,1],y= X[,2],z= y,col = ycolor,size=6)


#train one single tree
rf = randomForest(x=X,y=y,ntree=1,nodesize = 1)

#getTree
Tree = getTree(rf,k=1)
class(Tree)
dim(Tree)
Tree[1:50,]

#function to plot surfaces or slices of model structures
vec.plot(model=rf,X=X,i.var=1:2,col=ycolor,grid.lines = 200)
dev.new()
par(mfrow=c(1,2))

#make data
X = data.frame(replic(2,runif(2500)-.5))
y = +(X[,1]^2 + X[,2]^3)^2 - (X[,1]^2 + X[,2]^3)
ynoise =  rnorm(2500)/20

var(ynoise) / (var(y)+var(ynoise))
y=y+ynoise


#make color vector/gradient by y vector
ycolor = fcol(y,RGB = F,alpha=1)


rf = randomForest(x=X,y=y,ntree=1,nodesize = 1)
vec.plot(model=rf,X=X,i.var=1:2,col=ycolor,grid.lines = 200)

rf = randomForest(x=X,y=y,ntree=1,nodesize = 150)
vec.plot(model=rf,X=X,i.var=1:2,col=ycolor,grid.lines = 200)

rf = randomForest(x=X,y=y,ntree=500,mtry=1)
vec.plot(model=rf,X=X,i.var=1:2,col=ycolor,grid.lines = 200)

rf = randomForest(x=X,y=y,ntree=1500,nodesize=30)
rf
vec.plot(model=rf,X=X,i.var=1:2,col=ycolor,grid.lines = 200)

rf = randomForest(x=X,y=y,ntree=1500,sampsize=250)
rf
vec.plot(model=rf,X=X,i.var=1:2,col=ycolor,grid.lines = 200)

#fix variance be not grow full tree
rf = randomForest(x=X,y=y,ntree=500)

#make color vector/gradient by y vector
ycolor = fcol(y,RGB = F,alpha=1)
plot(X,col=ycolor,alpha=0.8)
#or plot in 3D
plot3d(x= X[,1],y= X[,2],z= y,col = ycolor,size=6,alpha=.8)
vec.plot(model=rf,X=X,i.var=1:2,col=ycolor,grid.lines = 200)



rf = randomForest(x=X,y=y,ntree=500)
#make color vector/gradient by y vector
ycolor = fcol(y,RGB = F,alpha=1)
plot(X,col=ycolor,alpha=0.8)
#or plot in 3D
plot3d(x= X[,1],y= X[,2],z= y,col = ycolor,size=6,alpha=.8)
vec.plot(model=rf,X=X,i.var=1:2,col=ycolor,grid.lines = 200)




rf = randomForest(x=X,y=y,ntree=1500,sampsize=50)
vec.plot(model=rf,X=X,i.var=1:2,col=ycolor,grid.lines = 200)


rf = randomForest(x=X,y=y,ntree=1,nodesize=150)
vec.plot(model=rf,X=X,i.var=1:2,col=ycolor,grid.lines = 200)





#single fully grown trees sensitive to noise
#make data
X = data.frame(replicate(2,runif(2500)-.5))
y = -X[,1]^2  -X[,2]^2 +rnorm(2500)*.1

Xtest = data.frame(replicate(2,runif(2500)-.5))
ytest = -Xtest[,1]^2  -Xtest[,2]^2+rnorm(2500)*.1



ycolor = fcol(y,RGB = F,alpha=1)
plot3d(x= X[,1],y= X[,2],z= y,col = ycolor,size=6)


#train one single tree
rf = randomForest(x=X,y=y,ntree=1,nodesize=1)
open3d()
vec.plot(model=rf,X=X,i.var=1:2,col=ycolor,grid.lines = 200)



nodesizes = c(1:15,ceiling(seq(20,1500,length.out=100)))#vector of different nodesizes

#iterate
modelErr = sapply( # iterator
  nodesizes,       # what to iterate
  function(i){     # function to run with different i's
    rf = randomForest(X,y,ntree=1,nodesize = i,#train a forest with nodesize=i
                      samplesize=2500,replace=F,mtry=2) 
    preds = predict(rf,Xtest) #predict
    mse = sqrt(sum((preds-ytest)^2)/length(preds)) #compute error
    return(mse) #return error from each model
  }
)

#plot test calibration to select optimal nodesize00
plot(nodesizes,modelErr,type="l",log="x",main="calibrate nodesize")

rf = randomForest(x=X,y=y,ntree=1,nodesize=50)
preds = predict(rf,Xtest) #predict
sqrt(sum((preds-ytest)^2)/length(preds)) #compute error
vec.plot(model=rf,X=X,i.var=1:2,col=ycolor,grid.lines = 200)

#run default RF instead
rf = randomForest(x=X,y=y,mtry=2)
vec.plot(model=rf,X=X,i.var=1:2,col=ycolor,grid.lines = 200) #ouch looks very overfitted!
preds = predict(rf,Xtest) #predict
sqrt(sum((preds-ytest)^2)/length(preds)) #compute error

#*# try to lower nodesize to 50 and 

#*# try to increase nodesize a little bit and check if error lowers

#*# try to lower sampsize to around 50 to 1500 to obtain a smooth model structure and
#*#  and lowest error.



