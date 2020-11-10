tjohnson <-
  function(x) {
    S <- seq(0.25, 1.25, 0.01)
    W <- rep(0, 101)
    L <- min(5000, length(x))
    for (i in 1:101) {
      # para cada z calculamos los cuantiles q1, q2, q3, q4 = phi(-2z), phi(-z), phi(z), phi(2z):
      z <- c(-3 * S[i], -S[i], S[i], 3 * S[i])
      q <- pnorm(z)
      m <- quantile(x, q[4]) - quantile(x, q[3])
      n <- quantile(x, q[2]) - quantile(x, q[1])
      p <- quantile(x, q[3]) - quantile(x, q[2])
      (QR <- m * n / p^2)
      if (is.na(QR) == FALSE) {
        if (QR < 1 & m != 0 & n != 0) {
          mu <- S[i] / acosh(0.5 * sqrt((1 + p / m) * (1 + p / n)))
          gamma <- mu * asinh((p / n - p / m) * sqrt((1 + p / m) * (1 + p / n) - 4) / 2 / (1 / QR - 1))
          lambda <- p * sqrt(((1 + p / m) * (1 + p / n) - 2)^2 - 4) / (1 / QR - 1)
          eps <- (quantile(x, q[3]) + quantile(x, q[2])) / 2 - lambda / 2 + p * (p / n - p / m) / 2 / (1 / QR - 1)
          if (sum((x - eps) / (lambda + eps - x) > 0) == length(x)) {
            Z <- gamma + mu * log((x - eps) / (lambda + eps - x))
            W[i] <- shapiro.test(sample(Z, L))$statistic
          }
        }
        if (QR > 1 & p != 0) {
          mu <- 2 * S[i] / (acosh(0.5 * (m / p + n / p)))
          gamma <- mu * asinh((n / p - m / p) / (2 * sqrt(QR - 1)))
          lambda <- 2 * p * sqrt(QR - 1) / (m / p + n / p - 2) / sqrt(m / p + n / p + 2)
          eps <- (quantile(x, q[3]) + quantile(x, q[2])) / 2 + p * (n / p - m / p) / 2 / (m / p + n / p - 2)
          Z <- gamma + mu * asinh((x - eps) / lambda)
          W[i] <- shapiro.test(sample(Z, L))$statistic
        }
        if (QR == 1 & m / p != 1 & m / p > 0 & p != 0) {
          mu <- 2 * S[i] / log(m / p)
          gamma <- mu * log((m / p - 1) / p / sqrt(m / p))
          eps <- (quantile(x, q[3]) + quantile(x, q[2])) / 2 - p * (m / p + 1) / 2 / (m / p - 1)
          if (x > eps) {
            Z <- gamma + mu * log(x - eps)
            W[i] <- shapiro.test(sample(Z, L))$statistic
          }
        }
      }
    }
    # seleccionamos el indice donde se maximiza W y aplicamos la transformacion correspondiente
    Z <- x
    if (sum(W) > 0) {
      j <- which(W == max(W))
      z <- c(-3 * S[j], -S[j], S[j], 3 * S[j])
      q <- pnorm(z)
      m <- quantile(x, q[4]) - quantile(x, q[3])
      n <- quantile(x, q[2]) - quantile(x, q[1])
      p <- quantile(x, q[3]) - quantile(x, q[2])
      QR <- m * n / p^2
      if (QR < 1 & p != 0) {
        mu <- S[j] / acosh(0.5 * sqrt((1 + p / m) * (1 + p / n)))
        gamma <- mu * asinh((p / n - p / m) * sqrt((1 + p / m) * (1 + p / n) - 4) / 2 / (1 / QR - 1))
        lambda <- p * sqrt(((1 + p / m) * (1 + p / n) - 2)^2 - 4) / (1 / QR - 1)
        eps <- (quantile(x, q[3]) + quantile(x, q[2])) / 2 - lambda / 2 + p * (p / n - p / m) / 2 / (1 / QR - 1)
        Z <- gamma + mu * log((x - eps) / (lambda + eps - x))
        clase <- 1
      }
      if (QR > 1 & n != 0 & m != 0) {
        mu <- 2 * S[j] / (acosh(0.5 * (m / p + n / p)))
        gamma <- mu * asinh((n / p - m / p) / (2 * sqrt(QR - 1)))
        lambda <- 2 * p * sqrt(QR - 1) / (m / p + n / p - 2) / sqrt(m / p + n / p + 2)
        eps <- (quantile(x, q[3]) + quantile(x, q[2])) / 2 + p * (n / p - m / p) / 2 / (m / p + n / p - 2)
        Z <- gamma + mu * asinh((x - eps) / lambda)
        clase <- 3
      }
      if (QR == 1 & m / p != 1) {
        mu <- 2 * S[j] / log(m / p)
        gamma <- mu * log((m / p - 1) / p / sqrt(m / p))
        eps <- (quantile(x, q[3]) + quantile(x, q[2])) / 2 - p * (m / p + 1) / 2 / (m / p - 1)
        Z <- gamma + mu * log(x - eps)
        clase <- 2
      }
    }
    par(mfrow = c(2, 2))
    x.points.Z <- seq(min(Z), max(Z), 0.1)
    x.points.x <- seq(min(x), max(x), 0.1)
    y.points.Z <- dnorm(x.points.Z, mean(Z), sd(Z))
    y.points.x <- dnorm(x.points.x, mean(x), sd(x))
    vZ <- max(y.points.Z) * 1.5
    vx <- max(y.points.x) * 1.5
    hist(Z, freq = F, main = "histrograma var transformada", ylim = c(0, vZ))
    lines(density(Z), col = "red", lwd = 2)
    lines(x.points.Z, y.points.Z, col = "green", lwd = 3)
    hist(x, freq = F, main = "histrograma var original", ylim = c(0, vx))
    lines(density(x), col = "red", lwd = 2)
    lines(x.points.x, y.points.x, col = "green", lwd = 3)
    qqnorm(Z)
    qqline(Z, col = "red")
    qqnorm(x)
    qqline(x, col = "red")
    cat("Valor Ã³ptimo para Z =  ", S[j], "\n")
    cat("Cociente de cuantiles (QR) = ", QR, "\n")
    return(list(Z = Z, valorZ = S[j], clase = clase))
  }
