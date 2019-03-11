hw_nat_select <- function(n_gen = 50, fp = 0.5, pop_size = 10000, fAA = 0.85, fAa = 0.9, faa =0.8) {
    #Function for visualization of genoripes proportions under an
    #influence of selection and gene drift
    #fp -- frequence of A allele
    #fq -- frequence of a allele
    #freq_init <- fp
    #calculate fq
    fq <- 1 - fp
    #set initial 
    res_tab <- rmultinom(1, pop_size, prob = c((fp ** 2), (2 * fq * fp), (fq ** 2)))
    res_tab <- as.numeric(res_tab)
    #Generate genotypes sequences
    for (i in c(2:n_gen)){
        fq <- 1 - fp
        #Generation of new frequences with multinomial distribution of size 3
        pop <- rmultinom(1, pop_size, prob = c((fp ** 2) * fAA, (2 * fq * fp) * fAa, (fq ** 2) * faa))
        pop <- as.numeric(pop)
        fp <- (pop[1] * 2 + pop[2]) / (pop_size * 2)
        res_tab <- rbind(res_tab, pop)
    }
    #Plot results
    par(mar = c(7, 4, 1, 1))
        #Add empty plot
    plot(NULL, ylim = c(1, pop_size), xlim = c(1, n_gen),
         xlab = "Поколений", ylab = "Количество генотипов",
         font.lab = 2, font = 2,
         cex.lab = 1.5, cex.axis = 1.5)
    AA <- res_tab[, 1]
    Aa <- res_tab[, 2]
    aa <- res_tab[, 3]

    #Create polygons for each genotype (by count)
    polygon(c(1, 1:n_gen, n_gen), c(0, aa, 0), col = "lightblue")
    Aa <- Aa + aa
    polygon(c(1:n_gen, n_gen:1), c(Aa, rev(aa)), col = "olivedrab2")
    AA <- AA + Aa
    polygon(c(1:n_gen, n_gen:1), c(AA, rev(Aa)), col = "brown1")

    #Add lines of starting genotypes counts
    abline(h = aa[1], lty = 2, lwd = 2)
    abline(h = Aa[1], lty = 2, lwd = 2)

    #Add legend
    par(mar = c(0, 4, 1, 1), new = TRUE)
    plot(NULL, xlim = c(1, 1), ylim = c(1, 1),
         xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    legend("bottom", legend = c("AA", "Aa", "aa"),
           fill = rev(c("lightblue", "olivedrab2", "brown1")),
           bty = "n", horiz = TRUE, cex = 1.5, xpd = TRUE)

    #Add computation of Hardy-Weinberg
    tab <- res_tab[c(1, n_gen), ]
    #chisq.test(Tab)
}
