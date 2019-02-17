BotNeckSim <- function(pop_size=1500, bot_size=10, f_a=0.5, f_b=0.5, f_c=0.5, f_d=0.5, f_e=0.5){
    # NGen=200
    # Bottleneck reduction on 30 step
    freq_vector <- c(f_a, f_b, f_c, f_d, f_e)
    col_mat <- c("darkorange", "dodgerblue", "firebrick1", "deeppink", "chartreuse")
    add_transparency <- function(color, alpha=0.4){

    #Add alpha level to color in character notation
        T <- as.numeric(col2rgb(color) / 255)
        color <- rgb(T[1], T[2], T[3], alpha)
        return(color)
    }
    col_transp <- as.character(sapply(col_mat, add_transparency))

    #Add new tranparent colors to color matrix
    col_mat <- as.matrix(rbind(col_mat, col_transp))

    sample_size <- rep(pop_size, 29)
    sample_size <- c(sample_size, bot_size)

    #Growth with sigmoidal function
    T <- seq(-6, 6, by = 0.1)
    size <- pop_size / (1 + exp(1) ** (-T))

    #Add growth curve to sample_size vector
    size <- size[size > bot_size]
    sample_size <- c(sample_size, size)
    #Expound sample_size vector to length 180 with pop_size values
    sample_size <- c(sample_size, rep(pop_size, 160 - length(sample_size)))

    gen_fun <- function(size, prob){
    #Function to perform computation of allele frequence
    #in next generation
        temp <- rbinom(1, size, prob)
        return(temp / size)
    }

    #Calculation of all frequences vectors
    res_freq <- NULL
    for (i in c(1:5)){
        res <- freq_vector[i]
        for (j in c(2:length(sample_size))){
            res <- c(res, gen_fun(round(sample_size[j] * 2), res[length(res)]))
        }
        res_freq <- cbind(res_freq, res)
    }
    colnames(res_freq) <- c("A", "B", "C", "D", "E")

    #Plotting
    layout(matrix(c(1, 2, 1, 3), 2))
    #Plot initial frame for frequences
    par(mar = c(3, 3, 2, 3), mgp = c(1, 0.25, 0), tck = 0.01)
    plot(NULL, xlim = c(1, length(sample_size) + 1),
         ylim = c(0, pop_size + pop_size * 0.05),
         xaxt = "n", yaxt = "n", ylab = "", xlab = "")

    #Add polygon of population size
    p_x <- c(1:length(sample_size))
    p_x <- c(p_x[1], p_x, p_x[length(p_x)])
    p_y <- c(0, sample_size, 0)
    polygon(p_x, p_y, col = "lightgrey", border = "lightgrey")
    #Add some axis labels and text
    axis(side = 4)
    axis(side = 1)
    mtext(side = 4, line = 1.2, "Размер популяции", font = 2, cex = 1.1)
    mtext(side = 1, line = 1.2, "Поколение")

    #Add lines for each frequence
    par(new = TRUE)
    plot(NULL, xlim = c(1, length(sample_size) + 1), 
         ylim = c(0, 1), xaxt = "n", yaxt = "n", ylab = "", xlab = "")

    for (i in c(1:5)){
        points(res_freq[, i], t = "l", lwd = 2, col = rev(col_mat[1, ])[i])
    }
    axis(side = 2)
    mtext(side = 2, line = 1.2, "Частота аллели", font = 2, cex = 1.1)

    #Add barplots of frequences
        #Calcilate matrixes for barplots
    f_st <- res_freq[5, ]
    f_en <- res_freq[length(res_freq[, 1]) - 5, ]
    f_st <- rbind(f_st, 1 - f_st)
    f_en <- rbind(f_en, 1 - f_en)
    t_st <- matrix(rep(0, 50), 10)
    t_en <- matrix(rep(0, 50), 10)
    for (i in c(1:5)){
        m <- c(9, 10)
        t_st[m - (i - 1) * 2, i] <- f_st[, i]
        t_en[m - (i - 1) * 2, i] <- f_en[, i]
    }
    colnames(t_st) <- colnames(f_st)
    colnames(t_en) <- colnames(f_en)
        #Add barplots to plotting window
    par(mar = c(3, 3, 2, 1))
    barplot(t_st, col = col_mat, main = "До редукции")
    mtext(side = 2, "Частота аллели", line = 1.2, font = 2, cex = 1.1)
    par(mar = c(3, 1, 2, 3))
    barplot(t_en, col = col_mat, main = "После редукции", yaxt = "n")
    axis(side = 4)
    mtext(side = 4, "Частота аллели", line = 1.2, font = 2, cex = 1.1)
}
