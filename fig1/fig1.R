# clean up R workspace
rm(list=ls(all=TRUE))

# # install libraries
# install.packages("ggplot2")
# install.packages("ggsignif")
# install.packages("multcomp")

# load libraries
library(ggplot2)
library(ggsignif)
library(multcomp)

# data
data <- read.csv("/Users/hidaka/Documents/102ND610/nagasaki_length.csv")

# boxplot
plt <- ggplot(data, aes(x=group, y=length)) + 
  scale_x_discrete(limit=c("normal_male", "large_male", "female")) +         # order of x-axis
  stat_boxplot(geom="errorbar", width=0.15) +                                # add error bar
  geom_boxplot(width=0.4)
# plt


# custom
plt2 <- ggplot(data, aes(x=group, y=length, fill=group)) + 
  ylim(30, 50) +
  scale_x_discrete(limit=c("normal_male", "large_male", "female"),           # order of x-axis, change labels
                   labels=c("normal_male"="Normal male", "large_male"="Large male", "female"="Female")) +
  stat_boxplot(geom="errorbar", width=0.15, lwd=2) +                       # add error bar
  geom_boxplot(width=0.4, lwd=2) +
  scale_fill_manual(values = c("gray50","gray80","white")) +                 # color
  labs(y="Length [mm]", x=NULL) +  # 軸ラベル
  theme(axis.text=element_text(size=27, face = "bold", colour = "black"),    # size, font, color of axis numbers
        #axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title=element_text(size=32,face="bold", colour = "black"),      # size, font, color of axis title
        axis.line = element_line(linewidth=1.2, colour = "black"),             # size, color of axis
        panel.grid.major = element_blank(),                                  # below, delete the background
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none"#,                                           # delete legend
        #legend.position=c(0.80,0.80), 
        #legend.background = element_text(fill = "white", color = "black"),  # background, color of legend
        #legend.title = element_text(size = 37, face = "bold"),              # size, font, color of legend title
        #legend.text = element_text(size = 37, face = "bold")
  )
# plt2



# t-test to get p-value
Tukey <- TukeyHSD(aov(data$length~data$group))
Tukey_df <- as.data.frame(Tukey$`data$group`)

# write.csv(Tukey_df, file="/Users/hidaka/Documents/102ND610/result/nagasaki_test.csv")


# function that returns asterisks depending on p-value
# p > 0.05 "n.s.",0.05 ≧ p > 0.01 "*", 0.01 ≧ p > 0.001 "**", 0.001 > p "***"
sig <- function(a=0.05) {
  if (a > 0.05) {
    return("n.s.")
  } else {
    if ((a <= 0.05)&&(a > 0.01)) {
      return("*")
    } else {
      if ((a <= 0.01)&&(a > 0.001)) {
        return("**")
      } else return("***")
    }
  }
}


# significance
plt3 = plt2 +
  geom_signif(
    comparisons=list(c("large_male", "female")),            # choose compare group
    y_position=46,
    annotation=sig(Tukey_df["large_male-female","p adj"]),  # characters to display
    tip_length=0.03,                                        # length of the down line
    textsize=10,                                             # size of asterisk
    size=1.8
  ) +
  geom_signif(
    comparisons=list(c("normal_male", "female")),
    y_position=48,
    annotation=sig(Tukey_df["normal_male-female","p adj"]),
    tip_length=0.03,
    textsize=10,
    size=1.8
  ) +
  geom_signif(
    comparisons=list(c("normal_male", "large_male")),
    y_position=44,
    annotation=sig(Tukey_df["normal_male-large_male","p adj"]),
    tip_length=0.03,
    textsize=10,
    size=1.8
  )
plt3

# ggsave(filename="/Users/hidaka/Documents/102ND610/result/nagasaki_boxplot.png", width = 9, height = 9, plot=plt3, dpi=300)



# jitter
set.seed(6)
plt4 <- ggplot(data, aes(x=group, y=length, fill=group)) + 
  ylim(30, 50) +
  scale_x_discrete(limit=c("normal_male", "large_male", "female"),           # order of x-axis, change labels
                   labels=c("normal_male"="Early male\n(n=10)", "large_male"="Late male\n(n=7)", "female"="Female\n(n=22)")) +
  stat_boxplot(geom="errorbar", width=0.15, lwd=2) +                       # add error bar
  geom_boxplot(width=0.4, lwd=2) +
  scale_fill_manual(values = c("white","white","white")) +                 # color
  labs(y="Forewing length [mm]", x=NULL) +  # 軸ラベル
  theme(axis.text=element_text(size=27, face = "bold", colour = "black"),    # size, font, color of axis numbers
        #axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title=element_text(size=32,face="bold", colour = "black"),      # size, font, color of axis title
        #axis.line = element_line(linewidth=1.2, colour = "black"),             # size, color of axis
        axis.ticks.y = element_line(size = 1),                            # memori no settei
        axis.ticks.x = element_line(size = 1),
        axis.ticks.length.y = unit(-0.25, "cm"), 
        axis.ticks.length.x = unit(-0.25, "cm"), 
        axis.text.y = element_text(margin = margin(r = 7)),        # 文字とメモリの間のスペースを調整
        axis.text.x = element_text(margin = margin(t = 7)), 
        panel.grid.major = element_blank(),                                  # below, delete the background
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 3)  # 枠線の追加
        ) +
  geom_signif(
    comparisons=list(c("large_male", "female")),            # choose compare group
    y_position=46,
    annotation=sig(Tukey_df["large_male-female","p adj"]),  # characters to display
    tip_length=0.03,                                        # length of the down line
    textsize=10,                                             # size of asterisk
    size=1.8
  ) +
  geom_signif(
    comparisons=list(c("normal_male", "female")),
    y_position=48,
    annotation=sig(Tukey_df["normal_male-female","p adj"]),
    tip_length=0.03,
    textsize=10,
    size=1.8
  ) +
  geom_signif(
    comparisons=list(c("normal_male", "large_male")),
    y_position=44,
    annotation=sig(Tukey_df["normal_male-large_male","p adj"]),
    tip_length=0.03,
    textsize=10,
    size=1.8
  ) +
  geom_jitter(aes(col=group), size=5, height=0, width =0.1) +                               # add jitter plot
  scale_color_manual(values = adjustcolor(c("tomato","deepskyblue","royalblue"),alpha=1)) + # set color
  annotate("text", x=2.82, y=31.5, label=expression(bold("*** ; p < 0.001")), size=7, colour = "black") + 
  annotate("text", x=2.95, y=30.5, label=expression(bold("Tukey-Kramer test")), size=7, colour = "black")


plt4

# ggsave(filename="/Users/hidaka/Documents/102ND610/result/nagasaki_boxplot_jitter_8.png", width = 9, height = 9.5, plot=plt4, dpi=300)




