# Ebert
# 12 January 2017

library(ggplot2)
library(reshape)

num_words = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
AUC = c(.7420, .7994, .8204, .8158, .8275, .8344, .8351, .8368, .8350, .8340) # This is for Emoji
Accuracy = c(.7158, .7311, .7430, .7520, .7568, .7580, .7570, .7580, .7590, .7588) # This is for Emoji
s140_auc =  c(.6497, .7133, .7302, .7546, .7610, .7557, .7510, .7580, .7467, .7540) # This is for Sent140
s140_acc =  c(.6324, .6923, .7264, .7094, .7350, .7264, .7350, .7179, .7179, .7260) # This is for Sent140
minutes = c(5.0, 14.1, 28.1, 29.4, 40.8, 50.5, 1.03*60, 1.23*60, 1.37*60, 1.5*60)
df = as.data.frame(cbind(num_words, AUC, Accuracy))

melt(df, id=c("num_words"))

p = ggplot(data = melt(df, id=c("num_words")), aes(x = num_words, y = value, colour = variable)) +
  geom_line(aes(linetype = variable), size = 0.9) +
  xlab("Number of NDSI words used") + ylab("") +
  #ggtitle("Accuracy and AUC vs NDSI Words Used") +
  theme_bw() +
  theme(legend.title=element_blank()) +
  scale_fill_discrete(name="Blah",
                      breaks=c("emoji_auc", "emoji_auc"),
                      labels=c("AUC", "Accuracy 1")) +
  scale_color_manual(values=c("red", "black")) +
  theme(legend.position = c(.85,.2)) +
  scale_x_continuous(name="Number of NDSI words used", breaks=seq(0,1000,200)) +
  scale_y_continuous(name="", limits=c(0.7, 0.85), breaks = seq(0.7, 0.85, 0.05))
p








# Junk below here, I think.


# First plot
p = ggplot(data = df, aes(num_words))
p = p + geom_line(aes(y = emoji_auc, colour = "Emoji AUC"))
p = p + geom_line(aes(y = emoji_acc, colour = "Emoji Accuracy"))

p = p + scale_colour_manual("",
                            breaks = c("Emoji AUC", "Emoji Accuracy"),
                            values = c("red", "blue")) +
  xlab("Number of NDSI words used") + ylab("") +
  ggtitle("Accuracy and AUC vs NDSI Words Used") +
  theme_bw() +
  theme(legend.position = c(.85,.2)) +
  scale_x_continuous(name="Number of words used", breaks=seq(0,1000,200)) +
  scale_y_continuous(name="", limits=c(0.7, 0.85), breaks = seq(0.7, 0.85, 0.05))
p



p = ggplot(data = df, aes(num_words))
p = p + geom_line(aes(y = s140_auc, colour = "Sentiment140 AUC"))
p = p + geom_line(aes(y = s140_acc, colour = "Sentiment140 Accuracy"))
p = p + scale_colour_manual("",
                            breaks = c("Sentiment140 AUC", "Sentiment140 Accuracy"),
                            values = c("red", "blue")) +
  xlab("Number of NDSI words used") + ylab("") +
  ggtitle("Accuracy and AUC vs NDSI Words Used") +
  theme_bw() +
  theme(legend.position = c(.85,.2)) +
  scale_x_continuous(name="Number of words used", breaks=seq(0,1000,200)) +
  scale_y_continuous(name="", limits=c(0.6, 0.8), breaks = seq(0.6, 0.8, 0.05))
p


# Second Attempt
p = ggplot(data = df, aes(num_words))
p = p + geom_line(aes(y = emoji_auc, colour = "AUC", linetype="Emoji"))
p = p + geom_line(aes(y = emoji_acc, colour = "Accuracy", linetype="Emoji"))
p = p + geom_line(aes(y = s140_auc, colour = "AUC", linetype="Sentiment140"))
p = p + geom_line(aes(y = s140_acc, colour = "Accuracy", linetype = "Sentiment140"))
#p = p + scale_linetype_manual("Data Set", values=c("Emoji"=2,"Sentiment140"=1)) +
p = p + scale_colour_manual("Metric", values=c("AUC"=2,"Accuracy"=1)) +
  xlab("Number of words used") + ylab("") + 
  ggtitle("Accuracy and AUC vs NDSI Words Used") +
  theme_bw() +
  theme(legend.position = c(.85,.2)) +
  scale_x_continuous(name="Number of words used", breaks=seq(0,1000,200)) +
  scale_y_continuous(name="", limits=c(0.6, 0.85), breaks = seq(0.6, 0.85, 0.05))
p


# Third attempt: Use reshape?
