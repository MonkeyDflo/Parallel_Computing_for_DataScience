#Florent Jakubowski
#22/09/2020
#TP2



joe = function(x,k){
  n=length(x)
  r=NULL
  for(i in 1:(n-(k-1))) {
    if(all(x[ i : (i + k - 1) ] == 1)) r<-c(r,i)
  }
  return(r);
}

print('toto')

x= c(1,0,0,1,1,0,1,1,1)

joe(x, k=2)

#debug( joe )
#joe(x, k=2)

