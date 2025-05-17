
info_mahasiswa <- function(nama, daerah) {
  
  cat("\nNama:", nama, "\n");
  cat("UT Daerah:", daerah, "\n\n");
  
};


create_layout <- function(data_matriks, rows, cols, n) {
  
  data_layout <- matrix(c(data_matriks), nrow = rows, ncol = cols);
  cat("\nData Matriks 3x2:\n\n");
  print(data_layout);
  cat("\n");
  layout(data_layout);
  layout.show(n);
  
};

data_vektor <- function(x, y, kategori, frekuensi){
  
  data_xy <- rbind(x, y);
  colnames(data_xy) <- rep("", ncol(data_xy));
  print(data_xy);
  cat("\n");
  
  data_kategori <- rbind(kategori, frekuensi);
  colnames(data_kategori) <- rep("", ncol(data_kategori));
  print(data_kategori);
  cat("\n");
  
};


data_visualization <- function(kategori, frekuensi) {
  
  barplot(frekuensi, names.arg = kategori, 
        xlab = "Kategori", ylab = "Frekuensi",
        main = "Bar Plot Frekuensi Kategori");
  
  persentase <- frekuensi / sum(frekuensi)
  pie(persentase, 
      labels = paste0(kategori, "(", round(persentase * 100), "%)"), 
      main = "Pie Chart Proporsi Kategori")

};

main <- function() {
  
  info_mahasiswa("Riki Wahyudi", "Jakarta");
  x <- c(2, 1, 2, 4, 1, 2, 3, 4, 2, 3, 1);
  y <- c(4, 3, 4, 4, 3, 4, 5, 4, 4, 5, 3);
  sunflowerplot(x, y, xlab = "X", ylab = "Y",  main = "Sunflowerplot Data (x, y)");
  plot(x, y, xlab = "X", ylab = "Y",  main = "Plot Data (x, y)");
  kategori <- c("A", "B", "C", "D");
  frekuensi <- c(20, 35, 15, 30);
  data_visualization(kategori, frekuensi);
  create_layout(c(1, 2, 2, 3, 3, 4), 3, 2, 4);
  data_vektor(x, y, kategori, frekuensi);
  
};
