#Q1
x= c(120,115,130,140,180,210,180,120,130,150,100,190,210,160,150,190,200,170,152)
mean(x)
n = length(x)
n
hm = n/sum(1/x)
hm
ln = log10(x)
gm = 10^mean(log10(x))
gm
median(x)
q1 = quantile(x,0.25)
q1
d3 = quantile(x,0.3)
d3
p42 = quantile(x,0.42)
p42
q3 = quantile(x,0.75)
q3

#Q2
X = c(1,2,3,4,5,6,7,8)
F = c(7,11,10,8,5,4,3,2)
M = rep(X,F)
mean(M)

median(M)

mode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
mode(M)
q3 = quantile(M,0.75)
q3

variance = var(M)
variance

#Q3
lb = seq(0,70,10)
ub = seq(10,80,10)
f = c(4,8,15,24,16,14,12,7)
x = (lb+ub)/2
n = sum(f)
am = sum(f*x)/n
am

gm = 10^(sum(f*log10(x))/n)
gm         

hm = n/sum(f/x)
hm

#4
x=c(20,15,19,17,21,16,15,22,24,12,17,13,12,18,17,19,16,13,23,10)

max1 = max(x)
min1 = min(x)
range = max - min
range

cor = range/(max + min)
cor

q1 = quantile(x, 0.25)
q3 = quantile(x, 0.75)
qd = (q3 - q1)/2
qd

coqd = (q3 - q1)/(q3 + q1)
coqd

n = length(x)
m = mean(x)

md = sum(abs(x - m))/n
md

comd = md/m
comd

var1 = var(x)
sd = (var1)^1/2
cov = (sd * 100)/m
cov

# Q5
x=0:9
f=c(24,42,85,120,110,98,75,56,32,18)

n = length(x)
max1 = max(x)
min1 = min(x)
range = max1 - min1
range
cor = range/(max1 + min1)

q1 = quantile(x, 0.25)
q3 = quantile(x, 0.75)
qd = (q3 - q1)/2
qd

coqd = (q3 - q1)/(q3 + q1)
coqd

n = length(x)
mean = sum(f*x)/sum(f)

md = sum(f*abs(x - m))/n
md

comd = md/mean
comd

var1 = var(f*x)
sd = (var1)^1/2
cov = sd * 100/mean
cov

# Q6
lb = seq(20,140,20)
ub = seq(40,160,20)
x = (lb+ub)/2
f = c(8,11,15,24,16,14,12)
f
x
q1 = quantile(x, 0.25)
q3 = quantile(x, 0.75)
qd = (q3 - q1)/2
qd

coqd = (q3 - q1)/(q3 + q1)
coqd

mean = sum(f*x)/sum(f)
var1 = var(f*x)
sd = (var1)^1/2
cov = sd * 100/mean
sd
cov

# Q7
x = c(2,3,4,5,6,7)
f = c(5,10,15,20,15,10)

# karl pearson coefficient of skewness
mean = sum(f*x)/sum(f)

mode = x[which.max(f)]

var1 = sum(f*((mean - x)^2))/sum(f)
sd = (var1)^1/2

kpcs = (mean - mode)/sd
kpcs

# bowley's coefficient of skewness
y = rep(x,f)
q1 = quantile(y, 0.25)
q2 = quantile(y, 0.5)
q3 = quantile(y, 0.75)

bcs = (q3 + q1 - 2 * q2)/(q3 - q1)
bcs

m2 = sum(f*((x - mean)^2))/sum(f)

m3 = sum(f*((x - mean)^3))/sum(f)

m4 = sum(f*((x - mean)^4))/sum(f)

sk_moments = m3/(m2)^3/2
sk_moments

kurtosis = m4/m2^2
kurtosis

# Q8
lb = seq(400,900,100)
ub = seq(500,1000,100)
x = (lb+ub)/2
f = c(8,16,20,17,7,2)

# karl pearson coefficient of skewness
mean = sum(f*x)/sum(f)
cat("Mean :",mean)

mode = x[which.max(f)]
cat("Mode :",mode)

variance = sum(f*((x - mean)^2))/sum(f)
cat("Variance :",variance)

sd = sqrt(variance)
cat("Standard deviation :",sd)

kpcs = (mean - mode)/sd
cat("karl pearson coefficient of skewness for the given data is",kpcs)

# bowley's coefficient of skewness
y = rep(x,f)
q1 = quantile(y,0.25)
q2 = quantile(y,0.5)
q3 = quantile(y,0.75)

bcs = (q3 + q1 - 2 * q2)/(q3 - q1)
cat("bowley's coefficient of skewness for the given data is",bcs)

# coefficient of skewness based on moments

m2 = sum(f*((x - mean)^2))/sum(f)
cat(m2)

m3 = sum(f*((x - mean)^3))/sum(f)
cat(m3)

m4 = sum(f*((x - mean)^4))/sum(f)
cat(m4)

sk_moments = m3 / (m2)^3/2
cat(sk_moments)

kurtosis = m4 / (m2)^2
cat(kurtosis)
