
info_mahasiswa <- function(nama, daerah) {
  
  cat("\nNama:", nama, "\n");
  cat("UT Daerah:", daerah, "\n\n");
  
};

generate_binomial_matrix <- function(n_obs, sizes, probs, cols, title) {
  
  binomial_value <- rbinom(n_obs, size = sizes, prob = probs);
  data_binomial <- matrix(binomial_value, ncol = cols);
  cat(title, " Data binomial:\n\n");
  print(data_binomial);
  cat("\n");
  
};

plot_chisquare <- function(start_range, end_range, degree_val, title) {
  
  x <- seq(start_range, end_range, length.out = 100)
  y <- dchisq(x, df = degree_val)
  
  plot(x, y, type = "l", 
       main = paste0("Distribusi Chi-Square (df = ", degree_val, ")"),
       xlab = "x", 
       ylab = "Kepadatan",
       lwd = 2, 
       col = "blue")
  cat(title, " Distribusi Chi-Square berhashil ditampilkan.\n\n");
  
};


main <- function(){
  
  info_mahasiswa("Riki Wahyudi", "Jakarta");
  generate_binomial_matrix(30, 10, 0.3, 5, "Soal 1.");
  plot_chisquare(0, 5, 1, "Soal 2.");
  plot_chisquare(0, 40, 10, "Soal 3.");
  generate_binomial_matrix(40,5, 0.6, 8, "Soal 3.");
  
};
