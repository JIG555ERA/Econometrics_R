#Q.1
x= c(120,115,130,140,180,210,180,120,130,150,100,190,210,160,150,190,200,170,152)
mean(x)
n=length(x)
Hm= n/sum(1/x)
Hm
median(x)
ln=log10(x)
Gm= 10^mean(ln)
Gm
Q1=quantile(x,0.25)
Q1
D3=quantile(x,0.3)
D3
P42=quantile(x,0.42)
P42
Q3=quantile(x,0.75)
Q3

#Q.2
X=c(1,2,3,4,5,6,7,8)
F=c(7,11,10,8,5,4,3,2)
M=rep(X,F)
mean(M)
ln=log10(M)
ln
Gm=10^mean(ln)
Gm
n= length(M)
n
Hm=n/sum(1/M)
Hm
median(M)
T= table(M)
T
mode(T)
var(M)
Q3=quantile(M,0.75)
Q3
lb=seq(0,70,10)
up=seq(10,80,10)
f=c(4,8,15,24,16,14,12,7)
x=c(lb+up/2)
x

#Q.3
lb=seq(0,70,10);ub=seq(10,80,10);h=10
f=c(4,8,15,24,16,14,12,7);n=sum(f)
x=(lb+ub)/2
am=sum(f*x)/n;gm=10^(sum(f*log10(x))/n);hm=n/sum(f/x)
cat("arithmetic mean",am)
cat("geometric mean",gm)
cat("harmonic mean",hm)

#Q.4
x=c(20,15,19,17,21,16,15,22,24,12,17,13,12,18,17,19,16,13,23,10)
n=length(x);m=mean(x)
min=min(x);max=max(x);r=max-min;cr=r/(max+min)
q1=quantile(x,0.25);q3=quantile(x,0.75)
qd=(q3-q1)/2;cqd=(q3-q1)/(q3+q1)
md=sum(abs(x-m))/n;cmd=md/m
v1=var(x)
sd=(var(x))^1/2
cv=sd*100/m
cat("Arithmetic Mean",m)
cat("Range",r)
cat("Co-efficient of Range",cr)
cat("Quantile Deviation",qd)
cat("Co-efficient of Quantile Deviation",cqd)
cat("Mean Deviation",md)
cat("Co-efficient of Mean Deviation",cmd)
cat("Variance",v1)
cat("Standard Deviation",sd)
cat("Co-efficient of Variation",cv)

#Q.5
x=0:9;f=c(24,42,85,120,110,98,75,56,32,18)
n=length(x)
min=min(x);max=max(x);r=max-min;cr=r/(max+min)
q1=quantile(x,0.25);q3=quantile(x,0.75)
qd=(q3-q1)/2;cqd=(q3-q1)/(q3+q1)
m=sum(f*x)/n
md=sum(f*abs(x-m))/n;cmd=md/m
v1=var(f*x)
sd=(var(f*x))^1/2
cv=sd*100/m
cat("Arithmetic Mean",m)
cat("Range",r)
cat("Co-efficient of Range",cr)
cat("Quantile Deviation",qd)
cat("Co-efficient of Quantile Deviation",cqd)
cat("Mean Deviation",md)
cat("Co-efficient of Mean Deviation",cmd)
cat("Variance",v1)
cat("Standard Deviation",sd)
cat("Co-efficient of Variation",cv)

#Q.6
x= c(82,66,76,95,45,30,57,86,78,64,89,92,58,74,64,85,48,72,65,80)
m= mean(x);n=length(x)
m2=sum((x-m)^2)/n;m3=sum((x-m)^3)/n;m4=sum((x-m)^4)/n
cat("2nd central moment=",m2)
cat("3rd central moment=",m3)
cat("4th central moment=",m4)

#Q.7
x= c(182,166,176,195,145,130,157,106,198,124,199,197,128,104,94,115,128,102,125,130)
m= mean(x);n=length(x)
m2=sum((x-m)^2)/n;m3=sum((x-m)^3)/n;m4=sum((x-m)^4)/n
s=m2^0.5;q1=quantile(x,0.25);q3=quantile(x,0.75);q2=quantile(x,0.5)
sk=3*(m-q2)/s;sb=(q3+q1-2*q2)/(q3-q1)
ms=m3^2/m2^3;mk=m4/m2^2
cat("Karl pearson coefficient of skewness=",sk)
cat("Bowley's coefficient of skewness=",sb)
cat("Coefficient od Skewness based on moments=",ms)
cat("Measure of kurtosis=",mk)

#Q.8
x=2:7;f=c(5,10,15,20,15,10)
n=length(x)
m=sum(f*x)/n;m2=sum(f*(x-m)^2)/n;m3=sum(f*(x-m)^3)/n;m4=sum(f*(x-m)^4)/n
s=m2^20.5;mc=which(f==max(f));mo=x[mc]
sk=(m-mo)/s
y=rep(x,f);q1=quantile(y,0.25);q2=quantile(y,0.5);q3=quantile(y,0.75)
sb=(q3+q1-2*q2)/(q3-q1)
ms=m3^2/m2^3;mk=m4/m2^2
cat("Karl Pearson Co-efficient of Skewness is ",sk)
cat("Bowleys Co-efficient of Skewness is ",sb)
cat("Measure of Skewness Based on Moments is ",ms)
cat("Measure of Kurtosis Based on Moments is ",mk)

#Q.9
lb=seq(400,900,100);ub=seq(500,1000,100);h=100
f=c(8,16,20,17,7,2);n=sum(f)
x=(lb+ub)/2
m=sum(f*x)/n;m2=sum(f*(x-m)^2)/n;m3=sum(f*(x-m)^3)/n;m4=sum(f*(x-m)^4)/n
sd=m2^0.5
moc=which(f==max(f))
mo=lb[moc]+h*((f[moc-1])/(2*f[moc]-f[moc-1]-f[moc+1]))
sk=(m-mo)/sd
lcf=cumsum(f)
q1c=min(which(lcf>=n/4))
q1=lb[q1c]+(n/4-lcf[q1c-1])*h/f[q1c]
q2c=min(which(lcf>=2*n/4))
q2=lb[q2c]+(2*n/4-lcf[q2c-1])*h/f[q2c]
q3c=min(which(lcf>=3*n/4))
q3=lb[q3c]+(3*n/4-lcf[q3c-1])*h/f[q3c]
sk=m-mo/sd
sb=(q3+q1-2*q2)/(q3-q1)
ms=m3^2/m2^3
mk=m4/m2^2
cat("Karl Pearson Co-efficient of Skewness is ",sk)
cat("Bowleys Co-efficient of Skewness is ",sb)
cat("Measure of Skewness Based on Moments is ",ms)
cat("Measure of Kurtosis Based on Moments is ",mk)
