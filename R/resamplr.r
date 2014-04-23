library(ggplot2)

# library functions
deagg <- function(v) unlist(sapply(seq(1, length(v)), function(n) rep(n, v[n])))
resample <- function(data, n) length(unique(sample(data, n, replace=T)))
diversity <- function(data, size, count) sapply(rep(size, count), function(n) resample(data, n))

# rarefaction curve computation
rarefact <- function(data, step, iter) {
    val <- seq(0, length(data), step)
    boot <- sapply(val, function(x) diversity(data, x, iter))
    data.frame(mean=apply(boot, 2, mean), sd=apply(boot, 2, sd), size=val)
}

# rarefaction curve plot
rarefact.plot <- function(counts, iter=1e3) {
    step <- ceiling(sum(counts) / 5e2)
    dfx <- rarefact(deagg(counts), step, iter)
    p <- ggplot(subset(dfx, mean > 0), aes(x=size, y=mean))
    p <- p + ggtitle(paste("N = [", Reduce(paste, counts), "], i = ", iter))
    p <- p + geom_line()
    p <- p + geom_ribbon(aes(ymax=(mean + sd), ymin=(mean - sd)), alpha=1/4)
    p <- p + scale_x_continuous("sample size")
    p <- p + scale_y_continuous("number of classes", breaks=1:length(counts))
    p
}
