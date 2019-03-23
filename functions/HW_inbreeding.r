hw_inbrieeding <- function(n_gen = 50, fp = 0.5, pop_size = 1000, self_poll_ratio = 1 / 2){
    #function for visualization of inbreeding
    #n_gen -- number of generations
    #fp -- initial frequency of A allele
    #pop_size -- population size
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #   Internal functions
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ret_gamete <- function(genotype = 1) {
        #Function to produce random gamete from genotype
        #genotype - one of 1, 2, 3
        #where
        #1=AA, 2=Aa, 3=aa
        if (genotype == 1){
            return(1)
        }
        if (genotype == 2){
            return(sample(c(1, 2), 1))
        }
        if (genotype == 3){
            return(2)
        }
    }

    prod_genotype <- function(pop, self_poll_ratio) {
    #Function to produce breed genotype
    #from two or one parents genotypes
        self <- sample(c(TRUE, FALSE), 1, rep = TRUE, prob = c(self_poll_ratio, 1 - self_poll_ratio))
        gen_1 <- sample(pop, 1)
        if (self){
            gen_2 <- gen_1
        }else{
            gen_2 <- sample(pop, 1)
        }
        gamete_1 <- ret_gamete(gen_1)
        gamete_2 <- ret_gamete(gen_2)
        if ((gamete_1 + gamete_2) == 2){
            return(1)
        }              #AA
        if ((gamete_1 + gamete_2) == 3){
            return(2)
        }              #Aa
        if ((gamete_1 + gamete_2) == 4){
            return(3)
        }              #aa
    }

    new_population <- function(pop){
    #Function to produce breed population
    #from crossing random parents from
    #initial population
        len <- length(pop)
        new_pop <- sapply(pop, function(x) prod_genotype(pop, self_poll_ratio))
        return(new_pop)
    }

    t_fun <- function(x){
        if (all(x == c(1, 0, 0))){
            return(1)
        }
        if (all(x == c(0, 1, 0))){
            return(2)
        }
        if (all(x == c(0, 0, 1))){
            return(3)
        }
    }
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #   Main part
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fq <- 1 - fp
    pop <- rmultinom(pop_size, 1, prob = c((fp ** 2), (2 * fq * fp), (fq ** 2)))
    pop <- apply(pop, 2, t_fun)
    res_tab <- as.numeric(table(pop))
    len <- length(pop)
    for (i in c(2:n_gen)){
        pop <- new_population(pop)
        pop <- c(pop, c(1, 2, 3))
        res_tab <- rbind(res_tab, as.numeric(table(pop)) - c(1, 1, 1))
        pop <- pop[1:len]
    }

    #Plot results
    par(mar = c(7, 4, 1, 1))
        #Add empty plot
    plot(NULL, ylim = c(1, pop_size), xlim = c(1, n_gen),
         xlab = "Поколений", ylab = "Количество генотипов",
         font.lab = 2, font = 2)
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
    plot(NULL, xlim = c(1, 1), ylim = c(1, 1), xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    legend("bottom", legend = c("AA", "Aa", "aa"),
           fill = rev(c("lightblue", "olivedrab2", "brown1")),
           bty = "n", horiz = TRUE, cex = 1.5, xpd = TRUE)

    res_tab <- res_tab[c(1, NROW(res_tab)), ]
    rownames(res_tab) <- c("Начало", "Конец")
    colnames(res_tab) <- c("AA", "Aa", "aa")
    return(res_tab)
}
