###Building and checking

R CMD build --resave-data ../../pkg
R CMD check --as-cran ./enaR_*.tar.gz
