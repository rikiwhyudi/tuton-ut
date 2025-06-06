
info_mahasiswa <- function(nama, daerah) {
  
  cat("\nNama:", nama, "\n");
  cat("UT Daerah:", daerah, "\n\n");
  
};


plot_normal_curve <- function(n) {
  
  x <- rnorm(n);
  density_x <- density(x);
  plot(density_x, main = "Kurva Normal", xlab= "x", ylab = "density");
  abline(v = mean(x), col = "green", lwd = 2);
  cat("Soal 1: Kurva normal telah ditampilkan.\n\n");
  
}

generate_binomial_matrix <- function(n_obs, sizes, probs, cols) {
  
  value <- rbinom(n_obs, size = sizes, prob = probs);
  binomial_data <- matrix(value, ncol = cols);
  cat("Soal 2: Data binomial (matriks 6x5):\n\n");
  print(binomial_data);
  cat("\n");
  
}

sum_negative_series <- function(j, k){
  
  rezult <- 0
  cat("Soal 3: Proses perhitungan:\n\n");
  for (i in j:k) {
    term <- -i * 2
    rezult <- rezult + term
    cat("-", i, "* 2 =", term, "-> Akumulasi =", rezult, "\n");
  }
  
  cat("\nHasil penjumlahan for loop (", j, "sampai", k, "dikali -2):", rezult, "\n\n");
  
};



main <- function(){
  
  info_mahasiswa("Riki Wahyudi", "Jakarta");
  plot_normal_curve(500);
  generate_binomial_matrix(30, 10, 0.25, 5);
  sum_negative_series(1, 10);
  
};
