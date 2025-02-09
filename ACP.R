mean_f = function(mean,X){
  for(i in 1:ncol(X)){
  mean[i] = mean(X[,i])  
}  
return (mean)
}


x1 = c(1,2,3,4,9)
x2 = c(5,10,8,8,12)

X = cbind(x1,x2)
colnames(X)  = c("X1","X2")
n = dim(X)[1]
m = dim(X)[2]

mean = rep(0,m)
mean =mean_f(mean,X) 


Xc = X*0
for(i in 1:ncol(X)){
    Xc[,i] = X[,i]-mean[i]
}

variance_f = function(Xc){
    var= rep(0,ncol(Xc))#0,0
    for(i in 1:ncol(Xc)){
    sum=0
    for(j in 1:nrow(Xc)) sum = sum + (Xc[j,i]**2)
    
    var[i]= sum/nrow(Xc)
        
    }

    
    return (var)
    
   
    
}

variance = variance_f(Xc)
std = sqrt(variance)


cov_f = function(Xc){
    cov =0
    for(i in 1:nrow(Xc)){
        cov = cov + (Xc[i,1]*Xc[i,2])
    }
    return (cov/nrow(Xc))
}

#V = 1/n * (t(Xc)%*%Xc)
cov = cov_f(Xc)

V = matrix(cov,m,m)
diag(V) = variance
D = diag(1/std)
Xcr  = Xc %*%D
r_coef=cov/prod(std)

R = matrix(r_coef,m,m)
#R = 1/n * (t(Xcr)%*%Xcr)
diag(R) = 1

eig = eigen(R)
R = eig$vectors %*% diag(eig$values)%*% t(eig$vectors)

#composant principal
U = eig$vectors
lamdas = eig$values
CP = Xcr %*%U


#cordonneed des variables

var_cordinate = U %*% diag(sqrt(lamdas))

#table de quality 

quality_Table = X*0
contribution_table = X*0
sumXcr = matrix(0,1,n)

for(j in 1:n){
p=0
for (i in Xcr[j,]){
    p =p+ (i*i)
}
sumXcr[j]= p;

}


#print((0.131**2)/sumXcr[4])
quality_Table_f = function(quality_Table,CP,sumXcr){

for(j in 1:n)
for(i in 1:m){
quality_Table[j,i] = CP[j,i]**2/sumXcr[j]
}

return (quality_Table)
    
}

quality_Table = quality_Table_f(quality_Table,CP,sumXcr)





contribution_Table_f = function(contribution_table,CP,lamdas){

for(j in 1:n)
for(i in 1:m){
contribution_table[j,i] = CP[j,i]**2/(nrow(CP)*lamdas[i])
}

return (contribution_table)
    
}
contribution_table = contribution_Table_f(contribution_table,CP,lamdas)
print(contribution_table)


#draw ind
plot(CP[,1], CP[,2], 
     xlab="X Axis", ylab="Y Axis", 
     main="Scatter Plot with X and Y Axes", 
     col="blue", pch=19, asp=1)

# Add X and Y axes at zero
abline(h=0, col="black", lwd=2)  # Horizontal (X-axis)
abline(v=0, col="black", lwd=2)  # Vertical (Y-axis)

grid()  # Add grid for better visualization


#draw cercle

 

plot(var_cordinate, xlim=c(-1,1), ylim=c(-1,1), pch=19, col="blue", asp=1)
symbols(0, 0, circles=1, add=TRUE, inches=FALSE, lwd=2)  # Draw circle




