multinorm.test <- function(X){
  ##########################################
  # input X adalah matriks berukuran n x p
  ##########################################
  if (anyNA(X)) stop("Input matrix contains missing values")
  mu <- colMeans(X) # menghitung vector mean dari matriks X
  S <- cov(X) # menghitung matriks kovarian
  invS <- solve(S)
  d <- matrix(rep(0,nrow(X)),nrow(X),1)
  eval <- matrix(rep(0,nrow(X)),nrow(X),1)
  q <- qchisq(0.5,ncol(X)) # menghitung titik kritis
  
  # Menghitung jarak dan mengevaluasinya terhadap titik kritis
  for (i in 1:nrow(X)){
    d[i] <- as.numeric(X[i,] - mu) %*% invS %*% as.numeric(t(X[i,] - mu))
    eval[i] <- (d[i] <= q)
  }
  
  # Menghitung proporsi jarak yang memenuhi kriteria pengujian
  prop <- mean(eval)
  result <- list(distance = d, chisquared = q, proportion = prop)
  return(result)
}

# Memanggil data, pastikan direktorinya sudah tepat
data <- read_excel("C:/Users/wulandari/Downloads/TESTING NORMALITAS MULTI.xlsx")
# Menjalankan fungsi multinorm.test
multinorm.test(data)



###############################################################

###############################################################
homocov <- function(covlist,nlist,alpha) {
  #####################################################################
  # input covlist: list berupa dua atau lebih matriks kovarian p x p #
  # nlist: list berupa banyaknya observasi untuk setiap grup #
  # alpha: level signifikan dari pengujian hipotesis #
  #####################################################################
  if (is.list(covlist)) {
    if (is.list(nlist)) {
      if (length(covlist) == length(nlist)) {
        if (length(covlist) < 2)
          stop("covlist must be a list with more than one element")
        p1 <- as.vector(sapply(covlist,dim))
        if (sum(p1[1] == p1) != length(p1))
          stop("all covariance matrices must have the same dimension")
      }
      else
        stop("covlist and nlist must have the same length")
    }
    else
      stop("nlist must be a list")
  }
  else
    stop("covlist must be a list")
  # Menunjukkan banyaknyanya grup yang dibandingkan
  g <- length(covlist)
  # Menunjukkan banyaknya prediktor
  p <- p1[1]
  Ncora <- 0
  Ncor <- 0
  A <- 0
  Sln <- matrix(c(0),p1[1],p1[1])
  for (l in 1:g){
    Ncora <- (1/((nlist[[l]]) - 1)) + Ncora
    Ncor <- ((nlist[[l]]) - 1) + Ncor
    Sln <- (covlist[[l]] * (nlist[[l]]-1)) + Sln
    A <- ((nlist[[l]] - 1) * log(det(covlist[[l]]))) + A
  }
  # Menghitung matriks covarian gabungan dari ke-g grup
  Spooled <- Sln/Ncor
  M <- (Ncor * log(det(Spooled))) - A
  u <- (Ncora - (1/Ncor))*(2*p^2+3*p-1)/(6*(p+1)*(g-1))
  # Menghitung statistik uji yang berdistribusiChi-sq
  C <- (1 - u)*M
  # Menghitung derajat bebas untuk menentukan titik kritis
  df <- (p*(p+1)*(g-1))/2
  # Menentukan titik kritis
  q <- qchisq(1-alpha,df)
  result <- list(statistic = C, degree_of_freedom = df,
                 critical_value = q)
  return(result)
}
# Mengekstrak data latihan 1 dari baris pertama sampai kelima
datcov1 <- ex14[1:5,]
# Mengekstrak data latihan 1 dari baris keenam sampai kesepuluh
datcov2 <- ex14[6:10,]
# Menjalankan fungsi homocov
homocov(list(cov(datcov1),cov(datcov2)),list(5,5),0.05)
