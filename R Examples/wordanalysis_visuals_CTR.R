library(xlsx)

bbt <- read.xlsx("BBT Content Analysis 9.22.2017.xlsx", sheetIndex=1)

bbt2 <- bbt[1:605,]   # subset without empty rows
bbt2$Text.Message <- iconv(bbt2$Text.Message, "UTF-8", "UTF-8")  # convert to UTF-8 to fix apostrophe encoding



na <- bbt2[is.na(bbt2$Month),]# check for na values
messages <- na$Text.Message
messages


bbt3 <- bbt2[!is.na(bbt2$Month),]

# fix words
bbt3$Text.Message <- gsub("BBT: ", "", bbt3$Text.Message)#subset out ignoring messages above & remove BBT: in text.message
bbt3$Text.Message <- gsub("toddler.+", "toddler", bbt3$Text.Message)  # fix toddler & toddler's to toddler
bbt3$Text.Message <- gsub("baby.+", "baby", bbt3$Text.Message)  # baby & baby's
bbt3$Text.Message <- gsub("baby's", "baby", bbt3$Text.Message) # baby's to baby
bbt3$Text.Message <- gsub("babies+", "baby", bbt3$Text.Message) # baby & babies
bbt3$Text.Message <- gsub("toy.+", "toy", bbt3$Text.Message)  # toy & toys
bbt3$Text.Message <- gsub("symptom.+", "symptom", bbt3$Text.Message)  # toy & toys
bbt3$Text.Message <- gsub("feeling.+", "feeling", bbt3$Text.Message)  # feeling & feelings
bbt3$Text.Message <- gsub("child.+", "child", bbt3$Text.Message) # child & child's
bbt3$Text.Message <- gsub("tip.+", "tips", bbt3$Text.Message) # tips & tip
bbt3$Text.Message <- gsub("child.+", "child", bbt3$Text.Message) # childs & childs
bbt3$Text.Message <- gsub("skill.+", "skill", bbt3$Text.Message) # skill & skills
bbt3$Text.Message <- gsub("cry.+", "cry", bbt3$Text.Message) # cry & crying
bbt3$Text.Message <- gsub("cries.+", "cry", bbt3$Text.Message) # cries & cry
bbt3$Text.Message <- gsub("game.+", "game", bbt3$Text.Message) # games & game
bbt3$Text.Message <- gsub("grow.+", "grow", bbt3$Text.Message) # grow & grows
bbt3$Text.Message <- gsub("fear.+", "fear", bbt3$Text.Message) # fear & fear
bbt3$Text.Message <- gsub("strategies", "strategy", bbt3$Text.Message) # strategy & strategies
bbt3$Text.Message <- gsub("like.+", "like", bbt3$Text.Message) # like & likes
bbt3$Text.Message <- gsub("smell.+", "semell", bbt3$Text.Message) # semlls & smell
bbt3$Text.Message <- gsub("milestone.+", "milestone", bbt3$Text.Message) # milestone & milestones
bbt3$Text.Message <- gsub("week.+", "week", bbt3$Text.Message) # weeks to week
bbt3$Text.Message <- gsub("tantrum.+", "trantrum", bbt3$Text.Message) # trantrums to tantrum
bbt3$Text.Message <- gsub("consequence.+", "consequences", bbt3$Text.Message) # consequence to consequences
bbt3$Text.Message <- gsub("techniques", "technique", bbt3$Text.Message) # techniques to technique
bbt3$Text.Message <- gsub("tummies", "tummy", bbt3$Text.Message) # tummies to tummy


print(summary(bbt3$CTR)) # mean is .11, median is .1186

behaviorMSG <- bbt3[grep("behavior.+", bbt3$Text.Message),]
print(behaviorMSG)
print(summary(behaviorMSG$CTR)) # median is 0.175, mean is 0.17

milestoneMSG <- bbt3[grep("milestone.+", bbt3$Text.Message),]
print(milestoneMSG)
# Question: Does Behavior 