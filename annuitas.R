setwd("E:\\SEMESTER 6\\PSK\\PSK R")

#nilai pv

annuitas<- function(num,k,i,t,m=TRUE)
  switch(num, 
         satu = {
           j=i/m
           n=t*m
           v=1/(1+j)
           an_akhir = k*(1-v^n)/j
           sn_akhir = k*((1+j)^n-1)/j
           cat('an annuitas akhir:', an_akhir)
           cat( 'sn annuitas akhir:',sn_akhir)
         },
         dua = {
           j=i/m
           n=t*m
           v=1/(1+j)
           an_Awal = k*(1-(v^n))/(j*v)
           sn_Awal = k*((((1+j)^n)-1)/(j*v))
           cat( "nilai an annuitas awal",an_Awal)
           cat( "nilai sn annuitas awal",sn_Awal)
         })
  annuitas("satu",2000, 0.08,4,2)  
  annuitas("dua",2000, 0.08,4,2)  
 

  