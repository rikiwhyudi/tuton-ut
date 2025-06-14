info_mahasiswa <- function(nama, daerah) {
  
  cat("\nNama:", nama, "\n");
  cat("UT Daerah:", daerah, "\n\n");
  
};

oddEven_numbers <- function(j, k) {
  
  cat("Soal No. 1:\n");
  for (i in j:k) {
    if (i %% 2 == 0) {
      cat(i, "\n")
    };
  };
  
};

quadratic_vectors <- function(j, k, rand) {
  

  v <- rnorm(rand);
  cat("\nSoal No. 2:\n");
  for (i in j:k) {
    result <- v[i] * v[i]
    cat(result, "\n");
  };
  
};
 

prime_numbers <- function(j, k) {
  
  cat("\n");
  for (i in j:k) {
    is_prime <- TRUE
    for (j in 2:(i - 1)) {
      if (i %% j == 0) {
        is_prime <- FALSE
        break
      };
    };
    if (is_prime) {
      cat(i, "is a prime number\n");
    };
  };
  
};

diamond_generator <- function(j, k) {
  
  cat("\n");
  for (rows in j:k) {
    cat(rep(" ", k - rows), sep = "");
    cat(rep("*", rows * 2 - 1), sep = "");
    cat("\n");
  };
  
  for (rows in (k - 1):j) {
    cat(rep(" ", k - rows), sep = "");
    cat(rep("*", rows * 2 - 1), sep = "");
    cat("\n");
  };
  
};

fibonacci <- function(n) {
  
  fn <- 1
  fn_1 <- 0
  cat("\n");
  for (i in 0:(n - 1)) {
    cat(fn, " ");
    temp <- fn
    fn <- fn + fn_1
    fn_1 <- temp
  }
  cat("\n\n");  
  
};


question_three <- function(title) {
  
  cat(title);
  prime_numbers(2, 20);
  diamond_generator(1, 5);
  fibonacci(10);
  
};


main <- function() {
  
  info_mahasiswa("Riki Wahyudi", "Jakarta");
  oddEven_numbers(1, 10);
  quadratic_vectors(1, 10, 30);
  question_three("\nSoal No. 3:");
  
};
