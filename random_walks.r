# install.packages("ggplot2")
library(ggplot2)

RW <- function(N) {
  steps <- c(0, sample(c(-1, 1), N-1, replace = TRUE))
  x <- cumsum(steps)
  return(x)
}

clip_values <- function(x) {
  x <- pmax(-3, pmin(3, x))
  if (length(x[x == 3]) > 0) {
    x[(which(x == 3)[1] + 1):length(x)] <- 3
  }
  if (length(x[x == -3]) > 0) {
    x[(which(x == -3)[1] + 1):length(x)] <- -3
  }
  return(x)
}

set.seed(NULL)

num_walks <- 100
num_steps <- 100

for (i in 1:10) {
  result_list <- vector("list", num_walks)
  for (j in 1:num_walks) {
    P <- RW(num_steps)
    P <- clip_values(P)
    result_list[[j]] <- P
  }

  min_length <- min(sapply(result_list, length))
  result_list <- lapply(result_list, function(x) x[1:min_length])
  
  data_df <- data.frame(step = rep(1:min_length, num_walks),
                        value = unlist(result_list),
                        walk_num = factor(rep(1:num_walks, each = min_length)),
                        variant = factor(rep(i, num_walks * min_length)))
  
  p <- ggplot(data_df, aes(x = step, y = value, group = walk_num, color = walk_num)) +
    geom_line(alpha = 0.5) +
    theme_minimal() +
    labs(x = "Step", y = "Value", title = paste0("Random Walk Simulations - Variant ", i),
         subtitle = "100 different random walks") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  p <- p + coord_cartesian(ylim = c(-3, 3))
  
  filename <- paste0("random_walk_variant_", i, ".png")
  ggsave(filename, plot = p, width = 8, height = 6, dpi = 300)
}

