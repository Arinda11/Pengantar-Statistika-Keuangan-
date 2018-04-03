setwd("E:\\SEMESTER 6\\PSK\\PSK R")
#mencari bunga
#A:nilai akumulasi, k:modals, i:bunga, t:tahun, m:banyak pemberian bunga dalam 
#pilih:pilihan (1:bunga tunggal, 2:bunga majemuk, 3:bunga majemuk kontinu)
bunga <- function(num, A, k, t, m=TRUE)
  switch(num, 
         satu = {
           tunggal = ((A/k)-1)/t
           cat("nilai bunga tunggal",tunggal)
         },
         dua = {
           majemuk.nominal = ((A/k)^(1/(m*t))-1)*m
           cat( "nilai bunga majemuk nominal",majemuk.nominal)
         },
         tiga = {
           majemuk.kontinu = (log(A/k))/t
           cat("nilai bunga majemuk kontinu",majemuk.kontinu)}
  )
bunga("satu", 4000, 500, 30)
bunga("dua", 4000, 500, 30)
bunga("dua", 4000, 500, 30 )
bunga("tiga", 4000, 500, 30)

