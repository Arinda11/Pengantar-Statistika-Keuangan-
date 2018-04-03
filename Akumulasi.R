#fungsi nilai akumulasi bunnga majemuk (tahunan)
Nilai.ABMT  <- function(k,i,t){  
  Nilai.ABM.Tahunan = k*(1+i)^t  
  return(Nilai.ABM.Tahunan)
}
Nilai.ABMT(2500,0.14,3)

#modifikasi fungsi tsb sehingga dapat digunakan untuk menghitung NABM nominal
Nilai.ABM  <- function(k,i,t,m){  
  Nilai.ABM.nominal = k*(1+(i/m))^(m*t)
  return(Nilai.ABM.nominal)
}
Nilai.ABM(100,0.06,0.5,4)

#fungsi untuk mencari nilai akumulasi
#k:modals, i:bunga, t:tahun, m:banyak pemberianbunga dalam 
#pilih:pilihan (1:bunga tunggal, 2:bunga majemuk, 3:bunga majemuk kontinu)
#modifikasi fungsi tsb sehingga tercipta pilihan input untuk menghitung bunga tunggal majemuk nominal dan majemuk kontinu
bunga <- switch(3, "tunggal", "majemuk nominal", "majemuk kontinu")
print(bunga)

bunga <- function(num, k, t, i, m=TRUE)
  switch(num, 
         satu = {
           tunggal = k*(1+(i*t))
           cat("nilai akumulasi bunga tunggal",tunggal)
         },
         dua = {
          majemuk.nominal = k*(1+(i/m))^(m*t)
           cat( "nilai akumulasi bunga majemuk nominal",majemuk.nominal)
         },
         tiga = {
           majemuk.kontinu = k*(exp(i*t))
           cat("nilai akumulasi bunga majemuk kontinu",majemuk.kontinu)}
  )
bunga("satu", 2000, 4, 0.08)
bunga("dua", 2000, 4, 0.08, m=1)
bunga("tiga", 2000, 4, 0.08, m=1)
