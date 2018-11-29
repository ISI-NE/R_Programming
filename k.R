data = read.csv("wine.csv",sep=";")

head(data)

X = data[,"citric.acid"]
Y = data[,"quality"]

X=c(1,2,4,5)
Y=c(1,2,4,5)
#function to calucate length of data
data_len = function(x)
{
  l=0
  for(i in x)
  {
    l=l+1
  }
  return(l)
}

#function to get the arithmetic mean
arithmetic_mean=function(x)
{
  sum_x = 0
  len =data_len(x)
  for(i in x)
  {
    sum_x = sum_x+i
  }
  mean_x =sum_x/len
  return(mean_x)
}

#function to get the geometric mean
geometric_mean=function(x)
{
  product =1
  for(i in x)
  {
    product = product *i
  }
  len = data_len(x)
  gm = product^(1/len)
  return(gm)
}

#function to get the harmonic mean
harmonic_mean=function(x)
{
  len = data_len(x)
  inv_sum =0
  for(i in x)
  {
    inv_sum = inv_sum+(1/i)
  }
  hm = len/inv_sum
  return(hm)
}

arithmetic_mean(X)
geometric_mean(X)
harmonic_mean(X)
arithmetic_mean(Y)
geometric_mean(Y)
harmonic_mean(Y)

#function to get the varience
get_varience=function(x)
{
  sum=0
  len=data_len(x)
  mean = arithmetic_mean(x)
  for(i in x)
  {
    sum=sum+(i-mean)^2
  }
  var = sum/len
  return(var)
}

get_varience(X)
get_varience(Y)


#funtion to calculate the correlation
get_corr=function(x,y)
{
  if(data_len(x)!=data_len(y))
  {
    print("data are of different size")
    return()
  }
  sum=0
  len = data_len(x)
  mean_x = arithmetic_mean(x)
  mean_y = arithmetic_mean(y)
  sd_x = get_varience(x)^(1/2)
  sd_y = get_varience(y)^(1/2)
  for(i in 1:data_len(x))
  {
    z_x = (x[i]-mean_x)/sd_x
    z_y = (y[i]-mean_y)/sd_y
    z = z_x * z_y
    sum=sum+z
  }
  crr = sum/len
  return(crr)
}

get_corr(X,Y)

arithmetic_mean(Y)
get_varience(X)
