setwd("E:\\SEMESTER 6\\PSK\\PSK R")
#mencari fungsi PV
#A:nilai akumulasi, i:bunga, t:tahun, m:banyak pemberian bunga dalam 
#pilih:pilihan (1:bunga tunggal, 2:bunga majemuk, 3:bunga majemuk kontinu)
pv <- function(num, A, i, t, m=TRUE)
  switch(num, 
         satu = {
           tunggal = A/(1+i*t)
           cat("nilai bunga tunggal",tunggal)
         },
         dua = {
           majemuk.nominal = A/((1+i*t)^(m*t))
           cat( "nilai bunga majemuk nominal",majemuk.nominal)
         },
         tiga = {
           majemuk.kontinu = A/exp(i*t)
           cat("nilai bunga majemuk kontinu",majemuk.kontinu)}
  )
# bunga("satu", 10000, 0.069, 20)
# bunga("dua", 10000, 0.069, 20)
# bunga("tiga", 10000, 0.069, 20)

