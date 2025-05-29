
info_mahasiswa <- function(nama, daerah) {
  
  cat("\nNama:", nama, "\n");
  cat("UT Daerah:", daerah, "\n");
  
};

create_matrix_data <- function(matrix_file, delimiter){
  
  cat("\n");
  matrix_data <- scan(file = matrix_file, sep = delimiter);
  
  cat("\n");
  create_matrix <- matrix(matrix_data, nrow = 4, ncol = 10, byrow = TRUE)
  print(create_matrix);
  cat("\n");
  
};

create_sales_data <- function(sales_file, delimiter){
  
  library(pander);
  create_sales_table <- read.csv(file = sales_file, sep = delimiter, header = TRUE);
  
  tryCatch({
    pander(create_sales_table,);
  }, error = function(e) {
    cat("\n");
    print(create_sales_table);
  });
  
};

statistic_calculation <- function(vector_data) {
  
  if (!is.numeric(vector_data)) {
    stop("Input harus berupa vektor numerik.");
  };
  
  cat("Vektor data yang dianalisis:", vector_data, "\n\n");

  mean_data <- mean(vector_data, na.rm = TRUE);
  cat("Rata-rata:", mean_data, "\n");
  
  median_data <- median(vector_data, na.rm = TRUE);
  cat("Median data:", median_data, "\n");
  
  sd_data <- sd(vector_data, na.rm = TRUE)
  cat("Standar deviasi data:", sd_data, "\n");
  

  cat("\nStatistic summary:\n");
  summary(vector_data);
}

main <- function(){
  
  setwd("D:/Programming/coding/r-lang");
  info_mahasiswa("Riki Wahyudi", "Jakarta");
  create_matrix_data("data2.txt", ",");
  create_sales_data("penjualan.csv", ",");
  statistic_calculation(c(85, 90, 78, 92, 88, 75, 95, 80, 83, 89));
  
};
