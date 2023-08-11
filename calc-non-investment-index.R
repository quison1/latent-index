
# install.packages("tidyverse")
library(tidyverse)

# install.packages("mirt")
library(mirt)

# loading data
noninv_sample <- read.csv("noninv_sample_simulated.csv")

# fit a 2PL model, with 1 latent variable, suppress printing iteration info
model1 <- mirt(noninv_sample, model = 1, itemtype = "2PL", verbose = TRUE, technical = list(NCYCLES=2500))

# retrieve the fitted parameters
model1_coef <- coef(model1)[-(ncol(noninv_sample)+1)] %>% do.call(rbind, .) %>% data.frame()
rownames(model1_coef) <- colnames(noninv_sample)

# The parameters given by mirt are:
# logit( Prob(pi_i) ) = d (intercept) + al (slope) * z1 (latent factor)
# g and u are the guessing and ceiling parameters (for 3PL and 4PL)

# IRT parameterisation uses Dscrmn and Dffclt param:
# logit( Prob(pi_i) ) = Dscrmn * (z1 - Dffclt)

# therefore, we retrieve the Dscrmn and Dffclt like so
model1_coef <- model1_coef %>% mutate(Dscrmn=a1, Dffclt=-d/a1)

# to visualise it, we use the tracePlot function from `ggmirt`
# `ggmirt` is pre-production at the time of writing
# see https://github.com/masurp/ggmirt

tracePlot <- function (model, items = NULL, theta_range = c(-4, 4), title = "Item Characteristics Curves", 
                       n.answers = 5, facet = TRUE, legend = FALSE) 
{
  data <- model@Data$data %>% as.data.frame
  theta_range = seq(theta_range[1], theta_range[2], by = 0.01)
  type <- model@Model$itemtype
  if (type[1] == "graded") {
    trace <- probtrace(model, Theta = theta_range) %>% 
      as_tibble %>% mutate(Theta = theta_range) %>% gather(key, 
                                                           value, -Theta) %>% separate(key, c("var", "response"), 
                                                                                       sep = ifelse(n.answers > 10, -4, -3))
    p <- ggplot(trace, aes(x = Theta, y = value)) + geom_line(aes(color = response)) + 
      facet_wrap(~var) + theme_minimal() + labs(x = expression(theta), 
                                                y = expression(P(theta)), title = title) + scale_color_brewer(palette = 7)
  }
  else {
    trace <- NULL
    for (i in 1:length(data)) {
      extr <- extract.item(model, i)
      theta <- matrix(theta_range)
      trace[[i]] <- probtrace(extr, theta)
    }
    if (!is.null(items)) {
      trace <- trace[items]
    }
    names(trace) <- paste("item", 1:length(trace))
    trace_df <- do.call(rbind, trace)
    item <- rep(names(trace), each = length(theta))
    d <- cbind.data.frame(theta, item, trace_df)
    d$item <- as.factor(d$item)
    if (isFALSE(facet)) {
      p <- ggplot(d, aes(theta, P.1, colour = item, linetype = item)) + 
        geom_line() + 
        scale_color_brewer(palette = 7) +
        scale_linetype_manual(values = "solid") +
        labs(x = expression(theta), y = expression(P(U[i]*"="*"1"*"|"*theta)), title = title) + 
        theme_minimal()
      if (isFALSE(legend)) {
        p <- p + guides(color = "none")
      }
    }
    else {
      p <- ggplot(d, aes(theta, P.1)) + geom_line() + facet_wrap(~item) + 
        labs(x = expression(theta), y = expression(P(U[i]*"="*"1"*"|"*theta)), 
             title = title) + theme_minimal()
    }
  }
  return(p)
}

tracePlot(model1)

# setting up the metadata for visualisation
metadf <- data.frame(
  cols = c("black", "grey", "blue", "grey", "grey"),
  linetypes = c("solid", "dotted", "solid", "solid", "dashed"),
  labs = c("Had any cash flow activity", "Receives email communications", "Registered on portal", "Nominated beneficiary", "Made a phone call"))

# visualise the item characteristics curves
tracePlot(model1, facet = F, legend = T) +
  coord_cartesian(xlim=c(-1.3, 1.5)) +
  scale_color_manual(labels = metadf$labs, values = metadf$cols) +
  scale_linetype_manual(labels = metadf$labs, values = metadf$linetypes) +
  labs(color = "", linetype = "", title="", x="Non-investment activity index", y="") +
  theme_bw()

# ggsave(filename = "sample-non-investment-activity.png")

# retrieve thetas (latent variable), non-investment activity index
noninv_res <- noninv_sample %>% mutate(theta = fscores(model1)[, "F1"])

summary(noninv_res)

# calculating probabilities
get_item_prob <- function(model, thetas) {
  data <- model@Data$data %>% as.data.frame
  type <- model@Model$itemtype
  if (type[1] == "graded") {
    trace <- probtrace(model, Theta = thetas) %>% 
      as_tibble %>% mutate(theta = thetas) %>% relocate(theta)
    return(trace)    
  } else {
    trace <- NULL
    for (i in 1:length(data)) {
      extr <- extract.item(model, i)
      theta <- matrix(thetas)
      trace[[i]] <- probtrace(extr, theta)
    }
    names(trace) <- colnames(data) # paste("item", 1:length(trace))
    trace_df <- do.call(rbind, trace)
    item <- rep(names(trace), each = length(theta))
    d <- cbind.data.frame(theta, item, trace_df)
    d$item <- as.factor(d$item)
    df <- d %>% select(theta, item, P.1) %>%
      pivot_wider(id_cols=theta, values_from=P.1, names_from=item)
    return(df)
  }
}

get_item_prob(model1, c(0, 1.5))
