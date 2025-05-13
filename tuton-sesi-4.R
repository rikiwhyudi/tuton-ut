info_mahasiswa <- function(nama, daerah) {
    cat("Nama:", nama, "\n");
    cat("UT Daerah:", daerah, "\n");
};

operasi_matriks <- function(vektor, rows, cols) {
  
    M <- matrix(c(vektor), nrow = rows, ncol = cols);
    cat("Matriks M:\n");
    print(M);
    cat("\n");
    
    perkalian_elemen <- M * M;
    cat("Perkalian Elemen Matriks:\n");
    print(perkalian_elemen);
    cat("\n");
    
    perkalian_aljabar <- M %*% M;
    cat("Perkalian Matriks (Aljabar):\n");
    print(perkalian_aljabar);
    cat("\n");
    
    invers_M <- tryCatch({
      solve(M);
    }, error = function(e) {
      cat("Kemungkinan matriks tidak memiliki invers.\n");
      return(NULL);
    });
    if (!is.null(invers_M)) {
      cat("Invers Matriks:\n");
      print(invers_M);
      cat("\n");
    };
    
    transpos_M <- t(M);
    cat("Transpos Matriks:\n");
    print(transpos_M);
    cat("\n");
    
    MT_kali_M <- transpos_M %*% M;
    cat("M^T * M:\n");
    print(MT_kali_M);
    cat("\n");
    
    
    A <- matrix(c(1, 4, 5, 6), nrow = 1);
    cat("Matriks A:\n");
    print(A)
    cat("\n");
    
    B <- matrix(1:4, nrow = 2, ncol = 4, byrow = TRUE);
    cat("Matriks B:\n");
    print(B)
    cat("\n");
    
    M1 <- M[c(2, 3), c(2, 3)];
    cat("Mengambil anak matriks M1:\n");
    print(M1);
    cat("\n");
    
    M2 <- M[c(1, 4), c(1, 4)]
    cat("Mengambil anak matriks M2:\n");
    print(M2)
    cat("\n");
    
    M3 <- rbind(M, A);
    cat("Menambahkan A sebagai baris ke-5 dari M:\n");
    print(M3);
    cat("\n");
    
    Bt <- t(B)
    cat("Transpos dari matriks B:\n");
    print(Bt)
    cat("\n");
    
    cat("Menambahkan B transpos kedalam kolom ke-1 dan ke-2 dari M sebagai M4:\n");
    M4 <- cbind(Bt, M)
    print(M4)
    cat("\n");

    matriks_identitas(4L, "4x4:\n");
    hitung_elemen_matriks(M4, "M4 yaitu:");
};

matriks_identitas <- function(n, deskripsi) {
    if (!is.integer(n) || length(n) != 1 || n <= 0) {
      stop("Error: 'n' harus berupa bilangan bulat positif skalar.");
    };
    
    identitas <- matrix(0, nrow = n, ncol = n);
    for (i in 1:n) {
      identitas[i, i] <- 1;
    };
    
    cat("Membuat Matriks Identitas", deskripsi);
    print(identitas);
    cat("\n");
};

hitung_elemen_matriks <- function(matriks, deskripsi) {
  if (!is.matrix(matriks)) {
    stop("Input harus berupa matriks.");
  }
  
  jumlah <- sum(matriks);
  cat("Total elemen matriks", deskripsi, jumlah, "\n");
};

main <- function() {
    cat("\n");  
    info_mahasiswa("Riki Wahyudi", "Jakarta");
    cat("\n");
    operasi_matriks(c(7, 0, 0, 0, 6, 9, 0, 0, 5, 7, 1, 0, 4, 5, 2, 3), 4, 4);
};

main();

