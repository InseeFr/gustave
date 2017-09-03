
set.seed(1)
n <- 1000
q <- 3
H <- 22
idref3 <- 1:n
wref3 <- rbinom(n,1500,0.9)
survey <- data.frame(
  id3 = idref3, quanti = rnorm(n)
  , quali = letters[sample.int(n) %% q + 1]
  , bynoNA = letters[sample.int(n) %% H + 1]
  , w3 = wref3
)
survey$quantiNA <- ifelse(runif(n) > 0.9, NA, survey$quanti)
survey$qualiNA <- as.factor(ifelse(runif(n) > 0.9, NA, survey$quali))
survey$byNA <- ifelse(runif(n) > 0.9, NA, survey$bynoNA)
survey <- survey[sample.int(floor(n/2)), ]
ref <- list(idref = idref3, wref = wref3)
