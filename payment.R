setwd("E:\\SEMESTER 6\\PSK\\PSK R")

#nilai pv

annuitas<- function(num,nilai,i,t,m=TRUE)
  switch(num, 
         satu = {
           j=i/m
           n=t*m
           v=1/(1+j)
           k_anakhir= nilai/((1-(v^n))/j)
           cat( 'nilai angsuran an annuitas akhir:', k_anakhir)
         },
         dua = {
           j=i/m
           n=t*m
           v=1/(1+j)
           k_snakhir = nilai/((((1+j)^n)-1)/j)
           cat( "nilai angsuran sn annuitas awal",k_snakhir)
         },
         tiga = {
           j=i/m
           n=t*m
           v=1/(1+j)
           k_anawal= nilai/((1-(v^n))/(j*v))
           cat( 'nilai angsuran an annuitas akhir:', k_anawal)
         },
         empat = {
           j=i/m
           n=t*m
           v=1/(1+j)
           k_snawal = nilai/((((1+j)^n)-1)/(j*v))
           cat( "nilai angsuran sn annuitas awal",k_snawal)
         })
  annuitas("satu",8000, 0.08,5,m=6)  
  annuitas("dua",8000, 0.08,5,m=6) 
  annuitas("tiga",8000, 0.08,5,m=6) 
  annuitas("empat",8000, 0.08,5,m=6) 

  