hw_common_case <- function(pop_n = 200, pop_size = 200) {
    #Function for demonstration of hardy weinberg equilibrium
    p <- seq(0, 1, length.out = pop_n)
    q <- 1 - p
    pq <- cbind(p, q)
    freq <- function(x,pop_size){
        #Internal function for new frequences culculation
        #x -- frequence of p or (A = "a_big") allele
        a_big <- rmultinom(1, pop_size, prob = c((x[1] ** 2), (2 * x[1] * x[2]), (x[2] ** 2)))
        a_big <- as.numeric(a_big)
        a_big <- a_big / pop_size
        return(a_big)
    }

    #Calculation of Hardy-Weinberg frequences for each
    #starting conditions
    freq_vect <- apply(pq, 1, freq, pop_size = pop_size)
    freq_vect <- t(freq_vect)
    colnames(freq_vect) <- c("AA", "Aa", "aa")

    #Calculate teoretical (expected) genotypes frequences
    AA_line <- seq(0, 1, length.out = 100) ** 2
    Aa_line <- 2 * seq(0, 1, length.out = 100) * (1 - seq(0, 1, length.out = 100))
    aa_line <- (1 - seq(0, 1, length.out = 100)) ** 2

    #Plot expected frequences as lines
    plot(NULL, xlim = c(0, 1), ylim = c(0, 1),
         xlab = "Частоты гамет f(A) или 1/f(a)",
         ylab = "Частоты генотипов",
         main = "Распределение частот по Харди и Вайнбергу",
         font = 2, font.lab = 2, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
    points(seq(0, 1, length.out = 100), AA_line, t = "l", lwd = 3, col = 2)
    points(seq(0, 1, length.out = 100), Aa_line, t = "l", lwd = 3, col = "forestgreen")
    points(seq(0, 1, length.out = 100), aa_line, t = "l", lwd = 3, col = "blue")

    #Plot simulated values as points
    points(p, freq_vect[, 1], cex = 1.5, col = "red")
    points(p, freq_vect[, 2], cex = 1.5, col = "forestgreen")
    points(p, freq_vect[, 3], cex = 1.5, col = "blue")

    #Add the legend to the plot
    legend("top", legend = c("АА", "Аа", "аа"),
           pch = c(1, 1, 1), lwd = c(2, 2, 2),
           col = c("red", "forestgreen", "blue"), ins = 0.1)
}
