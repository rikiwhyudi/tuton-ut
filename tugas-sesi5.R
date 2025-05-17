info_mahasiswa <- function(nama, daerah) {

  cat("\nNama:", nama, "\n");
  cat("UT Daerah:", daerah, "\n");
  
};

operasi_matriks <- function(vektor, rows) {
  
  M <- matrix(c(vektor), nrow = rows, byrow = TRUE);
  cat("\nMatriks M:\n");
  print(M);
  cat("\n");
  
  data_matriks <- matrix(c(7, 4, 5), ncol = 1);
 
  M1 <- cbind(M, data_matriks);
  cat("Menambahkan M1 pada kolom ke-4 untuk matriks M, sebagai M1:\n");
  print(M1);
  cat("\n");
  
  M2 <- cbind(data_matriks, M);
  cat("Menambahkan M2 pada kolom ke-1 untuk matriks M, sebagai M2:\n");
  print(M2);
  cat("\n");
  
  perkalian_elemen <- M1 * M2;
  cat("Perkalian Elemen Matriks:\n");
  print(perkalian_elemen);
  cat("\n");
  
  penjumlahan_elemen <- M1 + M2;
  cat("Penjumlahan Elemen Matriks:\n");
  print(penjumlahan_elemen);
  
};

data_visualization <- function(Nilai, Frekuensi){
  
  nilai_mahasiswa <- rbind(Nilai, Frekuensi);
  colnames(nilai_mahasiswa) <- rep("", ncol(nilai_mahasiswa));
  print(nilai_mahasiswa);
  
  plot(Nilai, Frekuensi, 
       main = "Plot Nilai vs Frekuensi", 
       xlab = "Nilai", 
       ylab = "Frekuensi");
  
  sunflowerplot(Nilai, Frekuensi, 
                main = "Sunflowerplot Nilai vs Frekuensi", 
                xlab = "Nilai", 
                ylab = "Frekuensi");
  
  pie(Frekuensi, labels = Nilai, main = "Pie Chart Nilai Mahasiswa");
  boxplot(Nilai, main = "Boxplot Nilai", ylab = "Nilai");
  boxplot(Frekuensi, main = "Boxplot Frekuensi", ylab = "Frekuensi");
  
};

data_mahasiswa <- function(directory, fileName, separator) {
  
  setwd(directory);
  read_data <- read.table(fileName, header = TRUE, sep = separator);
  
  tryCatch({
    pander(read_data);
  }, error = function(e) {
    cat("\n");
    print(read_data);
  });
  
};

main <- function(){
  
  info_mahasiswa("Riki Wahyudi", "Jakarta");
  operasi_matriks(c(2, 5, 6, 1, 8, 10, 9, 4, 7), 3);
  data_visualization(c(60, 67, 69, 72, 75, 77, 80, 83, 87, 90), c(2, 5, 6, 7, 9, 2, 6, 8, 1, 3));
  data_mahasiswa("D:/Programming/coding/r-lang", "Nilai_Komputer.txt", ",");
  
};

main();
