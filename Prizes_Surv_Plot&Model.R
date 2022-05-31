
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)

survdata <- read.csv('survdata_v2.csv') # need to be in the correct directory

#------------------------------------------------------------ Functions ------------------------------------------------------------#
# Helper function to customize plot labels
customize_labels <- function (p, font.title = NULL, font.subtitle = NULL, font.caption = NULL, font.legend = NULL,
                              font.x = NULL, font.y = NULL, font.xtickslab = NULL, font.ytickslab = NULL) {
  original.p <- p
  if(is.ggplot(original.p)) list.plots <- list(original.p)
  else if(is.list(original.p)) list.plots <- original.p
  else stop("Can't handle an object of class ", class (original.p))
  .set_font <- function(font){
    font <- ggpubr:::.parse_font(font)
    ggtext::element_markdown (size = font$size, face = font$face, colour = font$color)
  }
  for(i in 1:length(list.plots)){
    p <- list.plots[[i]]
    if(is.ggplot(p)){
      if (!is.null(font.title)) p <- p + theme(plot.title = .set_font(font.title))
      if (!is.null(font.subtitle)) p <- p + theme(plot.subtitle = .set_font(font.subtitle))
      if (!is.null(font.caption)) p <- p + theme(plot.caption = .set_font(font.caption))
      if (!is.null(font.legend)) p <- p + theme(legend.text = .set_font(font.legend))
      if (!is.null(font.x)) p <- p + theme(axis.title.x = .set_font(font.x))
      if (!is.null(font.y)) p <- p + theme(axis.title.y = .set_font(font.y))
      if (!is.null(font.xtickslab)) p <- p + theme(axis.text.x = .set_font(font.xtickslab))
      if (!is.null(font.ytickslab)) p <- p + theme(axis.text.y = .set_font(font.ytickslab))
      list.plots[[i]] <- p
    }
  }
  if(is.ggplot(original.p)) list.plots[[1]]
  else list.plots
}

# Main function to generate plots
survplot <- function(fit, data) {
  ggsurvplot(
    fit,                    # survfit object with calculated statistics.
    size = 1,                    # change line size
    data = data,             # data used to fit survival curves.
    pval = TRUE,             # show p-value of log-rank test.
    conf.int = TRUE,         # show confidence intervals for point estimates of survival curves.
    palette = c("steelblue1", "Gold"), 
    xlim = c(0,40),         # present narrower X axis, but not affect survival estimates.
    xlab = NULL,
    break.time.by = 10,     # break X axis in time intervals by 10.
    risk.table = TRUE,           # Add risk table
    risk.table.col = "strata",   # Risk table color by groups
    tables.height = 0.2,
    legend.labs = c("Contenders", "Winners"),    # change legend labels.
    ggtheme = theme_bw()         # Change ggplot2 theme
  )
}

# Changing the font size, style and color
survplot_cl <- function(survplot) {
  customize_labels(
    survplot,
    font.title     = c(14, "bold", "black"),
    font.subtitle  = c(13, "italic", "black"),
    font.caption   = c(12, "bold", "black"),
    font.legend    = c(12, "plain", "black"), 
    font.x         = c(13, "plain", "black"),
    font.y         = c(13, "plain", "black"),
    font.xtickslab = c(12, "plain", "black"),
    font.ytickslab = c(12, "plain")
  )
}

#-------------------------------------------------- Compare two groups (overall and by age group) --------------------------------------------------#
# Overall
fit <- survfit(Surv(time, event) ~ class, data = survdata)
test1 <- survdiff(Surv(time, event) ~ class, data = survdata)

plot_all <- survplot(fit, survdata)
# Labels for Survival Curves (plot)
plot_all$plot <- plot_all$plot + labs(
  title    = "Survival curves",
  subtitle = "for all prizewinners and contenders"
)
plot_all <- survplot_cl(plot_all)
plot_all

# By age group
fit_young <- survfit(Surv(time, event) ~ class, data = survdata, subset = which(survdata$group == 'Young'))
fit_middle <- survfit(Surv(time, event) ~ class, data = survdata, subset = which(survdata$group == 'Middle'))
fit_senior <- survfit(Surv(time, event) ~ class, data = survdata, subset = which(survdata$group == 'Senior'))

plot_young <- survplot(fit_young, survdata)
plot_young$plot <- plot_young$plot + labs(
  title    = "Survival curves",
  subtitle = "for young prizewinners and their matching contenders"
)
plot_young <- survplot_cl(plot_young)
plot_young

plot_middle <- survplot(fit_middle, survdata)
plot_middle$plot <- plot_middle$plot + labs(
  title    = "Survival curves",
  subtitle = "for middle-aged  prizewinners and their matching contenders"
)
plot_middle <- survplot_cl(plot_middle)
plot_middle

plot_senior <- survplot(fit_senior, survdata)
plot_senior$plot <- plot_senior$plot + labs(
  title    = "Survival curves",
  subtitle = "for senoir prizewinners and their matching contenders"
)
plot_senior <- survplot_cl(plot_senior)
plot_senior



#-------------------------------------------------- Cox model with shared frailty (PairID) --------------------------------------------------#
survdata$group <- factor(survdata$group, levels = c("Senior", "Young", "Middle")) # set "Senior" as the reference group
survdata$area <- factor(survdata$area)
survdata$area <- relevel(survdata$area, ref = "general") # set "general" as the reference group
coxfit1 <- coxph(Surv(time, event) ~ class + frailty(PairID), data = survdata)
coxfit2 <- coxph(Surv(time, event) ~ class * group + frailty(PairID), data = survdata)
coxfit3 <- coxph(Surv(time, event) ~ class * group + area + frailty(PairID), data = survdata) # the result presented in the paper
summary(coxfit3)
