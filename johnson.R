johnson <-
  function(x, clase, valorZ) {
    if (!is.element(clase, c(1, 2, 3))) stop("El valor que identifica a que familia de Johnson pertenece la variable debe ser 1, 2 o 3")
    z <- c(-3 * valorZ, -valorZ, valorZ, 3 * valorZ)
    q <- pnorm(z)
    m <- quantile(x, q[4]) - quantile(x, q[3])
    n <- quantile(x, q[2]) - quantile(x, q[1])
    p <- quantile(x, q[3]) - quantile(x, q[2])
    QR <- m * n / p^2
    if (clase == 1 & p != 0) {
      mu <- valorZ / acosh(0.5 * sqrt((1 + p / m) * (1 + p / n)))
      gamma <- mu * asinh((p / n - p / m) * sqrt((1 + p / m) * (1 + p / n) - 4) / 2 / (1 / QR - 1))
      lambda <- p * sqrt(((1 + p / m) * (1 + p / n) - 2)^2 - 4) / (1 / QR - 1)
      eps <- (quantile(x, q[3]) + quantile(x, q[2])) / 2 - lambda / 2 + p * (p / n - p / m) / 2 / (1 / QR - 1)
      Z <- gamma + mu * log((x - eps) / (lambda + eps - x))
    }
    if (clase == 2 & m / p != 1) {
      mu <- 2 * valorZ / log(m / p)
      gamma <- mu * log((m / p - 1) / p / sqrt(m / p))
      eps <- (quantile(x, q[3]) + quantile(x, q[2])) / 2 - p * (m / p + 1) / 2 / (m / p - 1)
      Z <- gamma + mu * log(x - eps)
    }
    if (clase == 3 & n != 0 & m != 0) {
      mu <- 2 * valorZ / (acosh(0.5 * (m / p + n / p)))
      gamma <- mu * asinh((n / p - m / p) / (2 * sqrt(QR - 1)))
      lambda <- 2 * p * sqrt(QR - 1) / (m / p + n / p - 2) / sqrt(m / p + n / p + 2)
      eps <- (quantile(x, q[3]) + quantile(x, q[2])) / 2 + p * (n / p - m / p) / 2 / (m / p + n / p - 2)
      Z <- gamma + mu * asinh((x - eps) / lambda)
    }
    return(Z)
  }
