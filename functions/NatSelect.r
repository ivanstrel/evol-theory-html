nat_select <- function(n_gen = 50, pa = 0.5, pop_size = 10000, pop_n = 5, fAA = 0.8, fAa = 0.85, faa = 0.9) {
    #Function for butch simulation of allele frequences under an influence
    #of selection (gene drift cause deviation when pop_size is small)
    #Generation of color palette
    col <- rainbow(pop_n)
    #Plot initial frequences
    plot(c(1:n_gen), rep(pa, n_gen),
         type = "l", ylim = c(0, 1), lwd = 2, lty = 2,
         xlab = "Время (поколения)",
         ylab = "Частота аллели",
         main = "Результаты симуляции естественного отбора")
    res_tab <- NULL
    col_ind <- 1
    for (i in c(1:pop_n)){
        #loop over n_gen populations to calculate forward frequences
        freq_dom <- pa
        freq_res <- freq_dom
        for (j in c(2:n_gen)){
            freq_rec <- 1 - freq_dom
            pop <- sample(c(1, 2, 3), pop_size,
                          prob = c((freq_dom**2) * fAA,
                                   (2 * freq_rec * freq_dom) * fAa,
                                   (freq_rec**2) * faa),
                          rep = TRUE)
            freq_dom <- (sum(pop == 2) + (sum(pop == 1)) * 2) / (pop_size * 2)
            freq_res[length(freq_res) + 1] <- freq_dom
        }
        #Add lines of each population frequences to the plot
        lines(c(1:n_gen), freq_res, t = "l", lwd = 3, col = col[col_ind])
        col_ind <- col_ind + 1
        res_tab <- rbind(res_tab, freq_res)
    }
}