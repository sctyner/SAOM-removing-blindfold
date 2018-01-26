# Try to make font in graphs match the size of the font in paper

# Source: https://gist.github.com/noamross/7576436

library(ggplot2)
sleepplot = ggplot(data = msleep, aes(x = log(bodywt), y = sleep_total))+geom_point(aes(color=vore))
sleepplot
#' We made a beautiful model of this relationship
slp = lm(sleep_total ~ log(bodywt), data=msleep)
summary(slp)
#'Let's put the model on the plot
sleepplot = sleepplot + geom_abline(intercept=coef(slp)[1], slope=coef(slp)[2])
sleepplot

#' First make the labels a little more useful.
sleepplot = sleepplot + labs(x="Log body weight (Kg)", y="Time asleep (hrs/day)")
sleepplot

#' Now let's fix the legend.
#' You would think you do this with some sort of "legend" command, but *no*,
#' what you are looking for is "scale". 
sleepplot + scale_color_discrete(name="Functional\nfeeding group",
                                 labels = c("carnivore", "herbivore", "insectivore", "omnivore"))

sleepplot + scale_color_brewer(name="Functional \n feeding group",
                               labels = c("carnivore", "herbivore", "insectivore", "omnivore"),
                               type = "qual", palette = 1)

sleepplot2 = ggplot(data = msleep, aes(x = log(bodywt), y = sleep_total)) + 
  geom_point(aes(shape=vore), size=3) + #' This time we will vary the feeding groups by shapes instead of colors
  geom_abline(intercept=coef(slp)[1], slope=coef(slp)[2])
sleepplot2
#' Now to fix the labels and legend again:
sleepplot2 = sleepplot2 + labs(x="Log body weight (Kg)", y="Time asleep (hrs/day)") +
  #' we will use scale_shape_discrete instead of scale_color_discrete
  scale_shape_discrete(name="Functional\nfeeding group",
                       labels = c("carnivore", "herbivore", "insectivore", "omnivore"))
sleepplot2
#' Now, let's work on how the plot looks overall.
#' 
#' ggplot uses "themes" to adjust plot appearence without changes the actual presentation of the data.
sleepplot2 + theme_bw(base_size=12, base_family = "Helvetica")
#' `theme_bw()` will get rid of the background, and gives you options to 
#' change the font. Science recomends Helvetica, wich happens to be R's
#' default, but we will specify it here anyway. 
#'
#' Check out the other fonts out here:
#'
#'    ??postscriptFonts
#'
#' For even more fonts, see the `extrafont` package.
#'
#' Other pre-set themes can change the look of your plot
sleepplot2 + theme_minimal()
sleepplot2 + theme_classic()
#'
#' For more themes,
library(ggthemes)
#' If you want to publish in the Wall Street Journal...
sleepplot2 + theme_wsj()
#' But we want to publish in Science, not the Wall Street Journal, so let's get back to our black and white theme.
sleepplot2 = sleepplot2 + theme_bw(base_size=12, base_family = "CM Roman")
sleepplot2
#' You can't really see the gridlines with the `bw` theme, so we are going to tweak the 
#' pre-set theme using the `theme` function.
#'`theme` allows you to do all kinds of stuff involved with how the plot looks.
#'
#'    ?theme
sleepplot2 + 
  #increase size of gridlines
  theme(panel.grid.major = element_line(size = .5, color = "grey"),
        #increase size of axis lines
        axis.line = element_line(size=.7, color = "black"),
        #Adjust legend position to maximize space, use a vector of proportion
        #across the plot and up the plot where you want the legend. 
        #You can also use "left", "right", "top", "bottom", for legends on t
        #he side of the plot
        legend.position = c(.85,.7),
        #increase the font size
        text = element_text(size=14)) 

#' You can save this theme for later use
science_theme = theme(panel.grid.major = element_line(size = .5, color = "grey"),
                      axis.line = element_line(size=.7, color = "black"),
                      legend.position = c(.85,.7),
                      text = element_text(size=14)) 
sleepplot2 = sleepplot2 + science_theme
sleepplot2

#' That looks pretty good. Now we need to get it exported properly.
#' The instructions say the figure should be sized 
#' to fit in one or two columns (2.3 or 4.6 inches),
#' so we want them to look good at that resolution.
pdf(file = "sleepplot.pdf", width= 6, height = 4, #' see how it looks at this size
    useDingbats=F) #I have had trouble when uploading figures with digbats before, so I don't use them
sleepplot2 #print our plot
dev.off() #stop making pdfs
#'
#' ### A few other tricks to improve the look of your plots:
#'
#' Let's say we are grouping things by categories instead of a regression
sleepcat = ggplot(msleep, aes(x=vore, y=sleep_total,color=conservation)) 
sleepcat + geom_point()
#' It's hard to see what's going on there, so we can jitter the points to make
#'them more visible.
sleepcat + geom_point(position = position_jitter(w=0.1))

#' Maybe this would be better with averages and error bars instead of every point:
library(plyr)
msleepave = ddply(msleep, .(vore, conservation), summarize, meansleep = mean(sleep_total), sdsleep = sd(sleep_total)/sqrt(22))
sleepmean = ggplot(msleepave, aes(x=vore, y = meansleep, color=conservation))
#' Plot it with means and error bars +/- 1 stadard deviation
sleepmean + geom_point() + geom_errorbar(aes(ymax = meansleep + sdsleep, ymin=meansleep + sdsleep), 
                                         width = 0.2)
#' Spread them out, but in an orderly fashion this time, with position_dodge rather than jitter
sleepmean +  geom_point(position = position_dodge(width=.5), size=2) +
  geom_errorbar(aes(ymax = meansleep + sdsleep, ymin=meansleep - sdsleep), 
                position = position_dodge(width=.5), width = .5)
#' Note that dodging the points gives the conservation status in the same order for each
#' feeding type category. A little more organized.
#'
#' ### Some other things you might want to do with formatting:
#'
#' Add annotation to the plot
sleepplot2 + annotate("text", label = "R2 = 0.999", x=-4, y=17)
#' Let's put that annotation in italics
sleepplot2 + annotate("text", label = "R2 = 0.999", x=-4, y=17, fontface=3)

#' NOW. Let's put half that annotation in italics, the other half plain,
#' then insert five greek characters and rotate it 90 degrees!
#'
#' OR we can beat our head against a wall until it explodes and
#' export our plot into an actual graphics program.
#'
#' Not everything has to be done in R. 'SVG' files are vector graphic files that can be easily edited in the
#' FREE GUI-based program [Inkscape](http://inkscape.org/).  Make and SVG and you can edit it by hand for final tweaks.
#' Inkscape can also edit and export PDFs.
svg(filename = "sleepplot.svg", width=6, height=4)


library(ggplot2)
library(extrafont)

loadfonts()

pdf("ggplot_cm.pdf", width=4, height=4)
p <- qplot(c(1,5), c(1,5)) +
  xlab("Made with CM fonts") + ylab("Made with CM fonts") +
  ggtitle("Made with CM fonts")

# Equation
eq <- "italic(sum(frac(1, n*'!'), n==0, infinity) ==
lim(bgroup('(', 1 + frac(1, n), ')')^n, n %->% infinity))"

# Without the new fonts
p + annotate("text", x=3, y=3, parse=TRUE, label=eq)

# With the new fonts
p2 <- p + 
  geom_text(data = NULL, aes(x=3, y=3, label=eq) , parse=TRUE) +
  theme(text         = element_text(size=16, family="CM Roman"),
        axis.title.x = element_text(face="italic"),
        axis.title.y = element_text(face="bold"))

ggsave("plot_cm_ggplot22.pdf", plot=p,  width=4, height=4)


dev.off()

# Embed the fonts
embed_fonts("ggplot_cm.pdf", outfile="ggplot_cm_embed.pdf")

loadfonts()

pdf("plot_cm.pdf", family="CM Roman", width=5, height=5)

plot(c(1,5), c(1,5), main="Made with CM fonts")
text(x=3, y=3, cex=1.5,
     expression(italic(sum(frac(1, n*'!'), n==0, infinity) ==
                         lim(bgroup('(', 1 + frac(1, n), ')')^n, n %->% infinity))))

dev.off()
embed_fonts("plot_cm.pdf", outfile="plot_cm_embed.pdf")

pdf("font_plot.pdf", family="Impact", width=4, height=4)
plot(mtcars$mpg, mtcars$wt, 
     main = "Fuel Efficiency of 32 Cars",
     xlab = "Weight (x1000 lb)",
     ylab = "Miles per Gallon")
dev.off()


library(ggplot2)
p <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +
  ggtitle("Fuel Efficiency of 32 Cars") +
  xlab("Weight (x1000 lb)") + ylab("Miles per Gallon") +
  theme(text=element_text(size=16, family="CM Roman"))
p

ggsave("font_ggplot.pdf", plot=p,  width=4, height=4)
