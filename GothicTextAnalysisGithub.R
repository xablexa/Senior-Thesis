#load all of the necessary packages first
library(gutenbergr)
library(ggplot2)
library(tidytext)
library(stringr)
library(NLP)
library(tm)  # make sure to load this prior to openNLP
library(openNLP)
library(openNLPmodels.en)
library(tidyverse)
library(ggtext)
library(glue)
library(Rmisc)
library(dplyr) #dplyr needs to be loaded last


# Need to download and format list of texts for analysis using gutenbergr #


EAG <- NULL
temp <- gutenberg_works(str_detect(author, "Brown, Charles"))
EAG <- temp[temp$gutenberg_id %in% c(792, 8223, 18508, 8404, 36289, 36290, 36291), ] # Ormond needs to be combined into one
temp <- gutenberg_works(str_detect(author, "Perkins"))
EAG <- rbind(EAG, temp[temp$gutenberg_id %in% c(1952), ])
temp <- gutenberg_works(str_detect(author, "Southworth"))
EAG <- rbind(EAG, temp[temp$gutenberg_id %in% c(29866), ])
temp <- gutenberg_works(str_detect(author, "Poe"))
EAG <- rbind(EAG, temp[temp$gutenberg_id %in% c(932, 2148, 2149), ]) # Need to separate out specific short stories from the anthology
# Also need to potentially expand number of Poe stories
temp <- gutenberg_works(str_detect(author, "O'Brien"))
EAG <- rbind(EAG, temp[temp$gutenberg_id %in% c(23169), ])
temp <- gutenberg_works(str_detect(author, "Beecher"))
EAG <- rbind(EAG, temp[temp$gutenberg_id %in% c(203), ])
temp <- gutenberg_works(str_detect(author, "Melville"))
EAG <- rbind(EAG, temp[temp$gutenberg_id %in% c(2489, 34970), ])
temp <- gutenberg_works(str_detect(author, "Cooper"))
EAG <- rbind(EAG, temp[temp$gutenberg_id %in% c(940, 6450), ])
temp <- gutenberg_works(str_detect(author, "Rowlandson"))
EAG <- rbind(EAG, temp[temp$gutenberg_id %in% c(851), ])
temp <- gutenberg_works(str_detect(author, "Hawthorne"))
EAG <- rbind(EAG, temp[temp$gutenberg_id %in% c(33, 77, 2181, 508, 512, 513), ]) # combine Marble Faun // 
#Separate Proph Pics and MBV from Twice Told Tales // Sep YGB from Mosses // Sep Man of A from Snow Image
temp <- gutenberg_works(str_detect(author, "Bird, R"))
EAG <- rbind(EAG, temp[temp$gutenberg_id %in% c(13970), ])
temp <- gutenberg_works(str_detect(author, "Irving"))
EAG <- rbind(EAG, temp[temp$gutenberg_id %in% c(41, 2048), ]) # sep RVW from Sketch Book
temp <- gutenberg_works((str_detect(title, "Famous Modern")))
EAG <- rbind(EAG, temp[temp$gutenberg_id %in% c(15143), ]) # sep What Was It? by FJO'B


# Now we will shorten collections and combine volumes as necessary #
EAG$gutenberg_id
EAGtext <- NULL
temp3 <- gutenberg_download(792, meta_fields = "title") #Wieland
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(8223, meta_fields = "title")#Edgar Huntly
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(8404, meta_fields = "title") #Jane Talbot
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(18508, meta_fields = "title") #Arthur Mervyn
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(36289, meta_fields = "title") #Ormond Vol 1
temp4 <- gutenberg_download(36290, meta_fields = "title") #Ormond Vol 2
temp3 <- rbind(temp3, temp4)
temp5 <- gutenberg_download(36291, meta_fields = "title") #Ormond Vol 3
temp3 <- rbind(temp3, temp5) #combine into a single text
temp3$title <- "Ormond; Or, The Secret Witness"
EAGtext <- rbind(EAGtext, temp3)
#temp3 <- gutenberg_download(1952, meta_fields = "title") #The Yellow Wallpaper
#EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(29866, meta_fields = "title") #Hidden Hand
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(932, meta_fields = "title") #Fall of the House of Usher
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(2148, meta_fields = "title") #The Tell-Tale Heart
temp3 <- temp3[-c(35:8486, 8676:9468),] #remove other texts from the anthology, leaving table of contents
temp3$title <- "The Tell-Tale Heart"
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(2149, meta_fields = "title") #Arthur Gordon Pym
temp3 <- temp3[-c(6881:10135),] #subset the story we want from the rest of the anthology
temp3$title <- "The Narrative of Arthur Gordon Pym of Nantucket"
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(23169, meta_fields = "title") #The Diamond Lens
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(203, meta_fields = "title") #Uncle Tom's Cabin
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(2489, meta_fields = "title") #Moby Dick
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(34970, meta_fields = "title") #Pierre or the Ambiguities
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(940, meta_fields = "title") #Last of the Mohicans
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(6450, meta_fields = "title") #The Prairie
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(851, meta_fields = "title") #Narrative
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(33, meta_fields = "title") #The Scarlett Letter
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(77, meta_fields = "title") #The House of the Seven Grables
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(508, meta_fields = "title") #The Minister's Black Veil
temp3 <- temp3[-c(38:697, 1265:9173),] #remove other stories from the anthology
temp$title <- "The Minister's Black Veil"
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(512, meta_fields = "title") #Young Goodman Brown
temp3 <- temp3[-c(31:706, 1242:7328),]
temp3$title <- "Young Goodman Brown"
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(9240, meta_fields = "title") #The Man of Adamant
temp3$title <- "The Man of Adamant"
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(13707, meta_fields = "title") #Prophetic Pictures
temp3 <- temp3[-c(104:4955, 5508:14728),] #remove from rest of anthology
temp3$title <- "Prophetic Pictures"
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(2181, meta_fields = "title") #The Marble Faun
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(13970, meta_fields = "title") #Nick of the Woods
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(41, meta_fields = "title") #Legend of Sleepy Hollow
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(2048, meta_fields = "title") #Rip Van Winkle
temp3 <- temp3[-c(58:1142, 1772: 12760),]
temp3$title <- "Rip Van Winkle"
EAGtext <- rbind(EAGtext, temp3)
temp3 <- gutenberg_download(15143, meta_fields = "title") #What was it?
temp3 <- temp3[-c(35:344, 396:7102, 7620:10849),]
temp3$title <- "What Was It?"
EAGtext <- rbind(EAGtext, temp3)

EAGfixed <- data.frame("gutenberg_id" = unique(EAGtext$gutenberg_id), "title" = "", "author" = "") #format summary dataframe
tempTitles <- unique(EAGtext$title)
tempTitle2 <- list("Ormond; Or, The Secret Witness", "Ormond; Or, The Secret Witness")
tempTitles <- append(tempTitles, tempTitle2, after=4)
EAGfixed$title <- tempTitles
EAGfixed$genre <- EAG$genre
EAGfixed <- EAGfixed[-c(6,7),]

EAG <- EAG[,-c(5, 7, 8)] #remove useless columns
EAGfixed$genre <- c("FG", "FG", "EAG", "FG", "FG", "EAG", "EAG", "EAG", "FG", "EAG", "FG", "FG", "EAG", "FG", "FG",
                    "FG", "FG", "FG", "EAG", "FG", "EAG", "EAG", "EAG", "FG", "FG", "EAG", "EAG") #add in genres

## Word Frequncy ##
EAGtextSumm <- EAGtext %>%
  unnest_tokens(word, text) %>%
  count(title, word, sort = TRUE)

total_words <- EAGtextSumm %>% 
  group_by(title) %>% 
  summarize(total = sum(n))

EAGtextSumm <- left_join(EAGtextSumm, total_words)

EAGtextSumm
# Generate a new tf_idf based on stop list & make one for EAG and one for FG #
book_tf_idf <- EAGtextSumm %>%
  bind_tf_idf(word, title, n)

book_tf_idf
book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# Checking the pre-generated wordlist #
#wordlist <- EAGtextSumm[grep("tree", EAGtextSumm$word),] #'street' comes up too
# Words related to geography (ie related to FG) #
FGwordlist <- EAGtextSumm[EAGtextSumm$word == "tree",]
FGwordlist <- rbind(FGwordlist, EAGtextSumm[EAGtextSumm$word == "trees",])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("native", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("indian", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[EAGtextSumm$word == "edge",])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("woods", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("forest", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("prairie", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("mountain", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("cave", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("wild", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("expanse", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("frontier", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("unknown", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("rock", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("dark", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("thirst", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("hunger", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("open", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("quiet", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("captive", EAGtextSumm$word),])
FGwordlist <- rbind(FGwordlist, EAGtextSumm[grep("green", EAGtextSumm$word),])

# Words related to Gothic (ie EAG and FG - shouldn't see a separation) #
EAGwordlist <- EAGtextSumm[grep("mad ", EAGtextSumm$word),]
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("mind", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("brick", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("fear", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("heart", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("love", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("night", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("dead", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("death", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("sin", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("shame", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("guilt", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("hell", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("gloom", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("old", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("ancient", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("fear", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("home", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("house", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("cellar", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("night", EAGtextSumm$word),])
EAGwordlist <- rbind(EAGwordlist, EAGtextSumm[grep("dark", EAGtextSumm$word),])

setwd("")
FGs <- unique(FGwordlist$word)
EAGs <- unique(EAGwordlist$word)
write.csv(FGs, "")
write.csv(EAGs, "")
write.csv(EAGfixed, "")


for(val in unique(EAGfixed$title)) {
  EAGfixed$EAGn[EAGfixed$title == val] <- sum(EAGwordlist$n[EAGwordlist$title == val])
  EAGfixed$FGn[EAGfixed$title == val] <- sum(FGwordlist$n[FGwordlist$title == val])
  EAGfixed$total[EAGfixed$title == val] <- sum(EAGwordlist$total[EAGwordlist$title == val])
}
for(i in 1:nrow(EAGfixed)) {
  EAGfixed$FGratio[i] <- (EAGfixed$FGn[i] / EAGfixed$total[i]) * 100
  EAGfixed$EAGratio[i] <- (EAGfixed$EAGn[i] / EAGfixed$total[i]) * 100
}
EAGfixed$title <- as.character(EAGfixed$title)
EAGfixed$gutenberg_id <- as.character(EAGfixed$gutenberg_id)
## time to graph them! ##
ggplot(EAGfixed, aes(x = gutenberg_id, y = FGratio, fill = genre)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(EAGfixed$FGratio), linetype = "dashed") +
  #  geom_hline(yintercept = mean(EAGfixed$FGratioNew) + sd(EAGfixed$FGratioNew), linetype = "dashed") +
  #  geom_hline(yintercept = mean(EAGfixed$FGratioNew) - sd(EAGfixed$FGratioNew), linetype = "dashed") +
  geom_text(aes(label=genre), position=position_dodge(width=0), vjust=-.025) +
  theme(axis.text.x=element_markdown(size=8, angle = 90))

ggplot(EAGfixed, aes(x = gutenberg_id, y = EAGratio, fill = genre)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(EAGfixed$EAGratio), linetype = "dashed") +
  #  geom_hline(yintercept = mean(EAGfixed$EAGratioNew) + sd(EAGfixed$EAGratioNew), linetype = "dashed") +
  #  geom_hline(yintercept = mean(EAGfixed$EAGratioNew) - sd(EAGfixed$EAGratioNew), linetype = "dashed") +
  geom_text(aes(label=genre), position=position_dodge(width=0), vjust=-.025) +
  theme(axis.text.x=element_markdown(size=8, angle = 90))

## Now to generate e new (unbiased) wordlist! ##
setwd("")
bookWordList <- read.csv(file = "wordlist.csv")
bookWordList <- as.vector(bookWordList)
bookWordList <- bookWordList[-c(1:10),]
bookWordList <- as.data.frame(bookWordList)
bookWordList$bookWordList <- as.character(bookWordList$bookWordList)
bookWordList$bookWordList <- str_trim(bookWordList$bookWordList, side = "left")
class(bookWordList$bookWordList)
class(book_tf_idf$word)
book_tf_idf <- book_tf_idf[!book_tf_idf$word %in% bookWordList$bookWordList,]

EAGtextSumm$genre <- ""
for(val in unique(EAGfixed$title)) {
  EAGtextSumm$genre[EAGtextSumm$title == val] <- EAGfixed$genre[EAGfixed$title == val]
}

EAG_tf_idf <- EAGtextSumm[EAGtextSumm$genre == "EAG",] %>%
  bind_tf_idf(word, title, n)
EAG_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))
EAG_tf_idf <- EAG_tf_idf[!EAG_tf_idf$word %in% bookWordList$bookWordList,]

FG_tf_idf <- EAGtextSumm[EAGtextSumm$genre == "FG",] %>%
  bind_tf_idf(word, title, n)
FG_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))
FG_tf_idf <- FG_tf_idf[!FG_tf_idf$word %in% bookWordList$bookWordList,]
# Some visualizations to picture the information calculated above
ggplot(EAG_tf_idf, aes(idf, tf)) +
  geom_point() +
  geom_smooth(method='lm')
ggplot(EAG_tf_idf, aes(n, tf_idf)) +
  geom_point() +
  geom_smooth(method='lm') 
ggplot(FG_tf_idf, aes(idf, tf)) +
  geom_point() +
  geom_smooth(method='lm')
ggplot(FG_tf_idf, aes(n, tf_idf)) +
  geom_point() +
  geom_smooth(method='lm') 
ggplot(book_tf_idf, aes(n, tf_idf)) +
  geom_point() +
  geom_smooth(method='lm') 

mean(book_tf_idf$tf_idf)
sd(book_tf_idf$tf_idf)
median(book_tf_idf$tf_idf)
quantile(book_tf_idf$tf_idf)
EAGwords <- data.frame("word" = unique(book_tf_idf$word[book_tf_idf$n >= 50 & book_tf_idf$tf_idf <= 2.117576e-05]),
                       "id" = length(unique(book_tf_idf$word[book_tf_idf$n >= 50 & book_tf_idf$tf_idf <= 2.117576e-05])))
FGwords <- data.frame("word" = unique(book_tf_idf$word[book_tf_idf$n >= 50 & book_tf_idf$tf_idf <= 3.980959e-05 & book_tf_idf$tf_idf >= 1.242523e-05]),
                      "id" = length(unique(book_tf_idf$word[book_tf_idf$n >= 50 & book_tf_idf$tf_idf <= 3.980959e-05 & book_tf_idf$tf_idf >= 1.242523e-05])))
FGwords$word <- as.character(FGwords$word)
EAGwords$word <- as.character(EAGwords$word)
FGwords2 <- FGwords[!c(FGwords$word %in% EAGwords$word),]
EAGwords <- data.frame("word" = unique(book_tf_idf$word[book_tf_idf$n >= 50 & book_tf_idf$tf_idf <= 2.117576e-05]),
                       "id" = length(unique(book_tf_idf$word[book_tf_idf$n >= 50 & book_tf_idf$tf_idf <= 2.117576e-05])))
FGwords <- data.frame("word" = unique(book_tf_idf$word[book_tf_idf$n >= 50 & book_tf_idf$tf_idf <= 3.980959e-05 & book_tf_idf$tf_idf >= 1.242523e-05]),
                      "id" = length(unique(book_tf_idf$word[book_tf_idf$n >= 50 & book_tf_idf$tf_idf <= 3.980959e-05 & book_tf_idf$tf_idf >= 1.242523e-05])))
FGwords$word <- as.character(FGwords$word)
EAGwords$word <- as.character(EAGwords$word)
EAGwords2 <- EAGwords[!c(EAGwords$word %in% FGwords$word),] # for some reason the 2nd %in% doesn't work unless you reset it first #
FGwords2$word
EAGwords2$word
EAGwords <- data.frame("word" = unique(book_tf_idf$word[book_tf_idf$n >= 50 & book_tf_idf$tf_idf <= 2.117576e-05]),
                       "id" = length(unique(book_tf_idf$word[book_tf_idf$n >= 50 & book_tf_idf$tf_idf <= 2.117576e-05])))
FGwords <- data.frame("word" = unique(book_tf_idf$word[book_tf_idf$n >= 50 & book_tf_idf$tf_idf <= 3.980959e-05 & book_tf_idf$tf_idf >= 1.242523e-05]),
                      "id" = length(unique(book_tf_idf$word[book_tf_idf$n >= 50 & book_tf_idf$tf_idf <= 3.980959e-05 & book_tf_idf$tf_idf >= 1.242523e-05])))
FGwords$word <- as.character(FGwords$word)
EAGwords$word <- as.character(EAGwords$word)
both <- FGwords[c(FGwords$word %in% EAGwords$word),] # can switch it around b/c both wil give the same list of 29

#write.csv(both, "")
#write.csv(EAGwords2, "")
#write.csv(FGwords2, "")


#OPTIONAL:#
######### Same analysis but using ratio of n/total to balance out differing book lengths ##########
book_tf_idf$ratio <- book_tf_idf$n/book_tf_idf$total
mean(book_tf_idf$ratio)
quantile(book_tf_idf$ratio, prob = seq(0, 1, length = 11), type = 5)
EAGwords <- data.frame("word" = unique(book_tf_idf$word[book_tf_idf$ratio >= 7.913396e-05 & book_tf_idf$tf_idf <= 2.117576e-05]),
                       "id" = length(unique(book_tf_idf$word[book_tf_idf$ratio >= 7.913396e-05 & book_tf_idf$tf_idf <= 2.117576e-05])))
FGwords <- data.frame("word" = unique(book_tf_idf$word[book_tf_idf$ratio >= 7.913396e-05 & book_tf_idf$tf_idf <= 3.980959e-05 & book_tf_idf$tf_idf >= 1.242523e-05]),
                      "id" = length(unique(book_tf_idf$word[book_tf_idf$ratio >= 7.913396e-05 & book_tf_idf$tf_idf <= 3.980959e-05 & book_tf_idf$tf_idf >= 1.242523e-05])))
FGwords$word <- as.character(FGwords$word)
EAGwords$word <- as.character(EAGwords$word)
FGwords2 <- FGwords[!c(FGwords$word %in% EAGwords$word),]
EAGwords <- data.frame("word" = unique(book_tf_idf$word[book_tf_idf$ratio >= 7.913396e-05 & book_tf_idf$tf_idf <= 2.117576e-05]),
                       "id" = length(unique(book_tf_idf$word[book_tf_idf$ratio >= 7.913396e-05 & book_tf_idf$tf_idf <= 2.117576e-05])))
FGwords <- data.frame("word" = unique(book_tf_idf$word[book_tf_idf$ratio >= 7.913396e-05 & book_tf_idf$tf_idf <= 3.980959e-05 & book_tf_idf$tf_idf >= 1.242523e-05]),
                      "id" = length(unique(book_tf_idf$word[book_tf_idf$ratio >= 7.913396e-05 & book_tf_idf$tf_idf <= 3.980959e-05 & book_tf_idf$tf_idf >= 1.242523e-05])))
FGwords$word <- as.character(FGwords$word)
EAGwords$word <- as.character(EAGwords$word)
EAGwords2 <- EAGwords[!c(EAGwords$word %in% FGwords$word),] # for some reason the 2nd %in% doesn't work unless you reset it first #
FGwords2$word
EAGwords2$word
EAGwords <- data.frame("word" = unique(book_tf_idf$word[book_tf_idf$ratio >= 7.913396e-05 & book_tf_idf$tf_idf <= 2.117576e-05]),
                       "id" = length(unique(book_tf_idf$word[book_tf_idf$ratio >= 7.913396e-05 & book_tf_idf$tf_idf <= 2.117576e-05])))
FGwords <- data.frame("word" = unique(book_tf_idf$word[book_tf_idf$ratio >= 7.913396e-05 & book_tf_idf$tf_idf <= 3.980959e-05 & book_tf_idf$tf_idf >= 1.242523e-05]),
                      "id" = length(unique(book_tf_idf$word[book_tf_idf$ratio >= 7.913396e-05 & book_tf_idf$tf_idf <= 3.980959e-05 & book_tf_idf$tf_idf >= 1.242523e-05])))
FGwords$word <- as.character(FGwords$word)
EAGwords$word <- as.character(EAGwords$word)
both <- FGwords[c(FGwords$word %in% EAGwords$word),] # can switch it around b/c both wil give the same list of 29
both$word

# Now need to find every row where book_tf_idf contains a word from each respective list #
EAGwordlist2 <- book_tf_idf[book_tf_idf$word %in% EAGwords2$word,]
length(unique(EAGwords2$word)) == length(unique(EAGwordlist2$word)) #check that it worked
FGwordlist2 <- book_tf_idf[book_tf_idf$word %in% FGwords2$word,] #repeat for FGs
length(unique(FGwords2$word)) == length(unique(FGwordlist2$word)) #check that it worked
# Now we can remake the same plots as before #
for(val in unique(EAGfixed$title)) {
  EAGfixed$EAGnNew[EAGfixed$title == val] <- sum(EAGwordlist2$n[EAGwordlist2$title == val])
  EAGfixed$FGnNew[EAGfixed$title == val] <- sum(FGwordlist2$n[FGwordlist2$title == val])
  EAGfixed$total[EAGfixed$title == val] <- sum(EAGwordlist2$total[EAGwordlist2$title == val])
}
for(i in 1:nrow(EAGfixed)) {
  EAGfixed$FGratioNew[i] <- (EAGfixed$FGnNew[i] / EAGfixed$total[i]) * 100
  EAGfixed$EAGratioNew[i] <- (EAGfixed$EAGnNew[i] / EAGfixed$total[i]) * 100
}
EAGfixed$title <- as.character(EAGfixed$title)
EAGfixed$gutenberg_id <- as.character(EAGfixed$gutenberg_id)

## time to graph them! ##
ggplot(EAGfixed, aes(x = gutenberg_id, y = FGratioNew, fill = genre)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(EAGfixed$FGratioNew), linetype = "dashed") +
  #geom_hline(yintercept = mean(EAGfixed$FGratioNew[EAGfixed$genre == "FG"]), linetype = "dashed", color = "blue") +
  #geom_hline(yintercept = mean(EAGfixed$FGratioNew[EAGfixed$genre == "EAG"]), linetype = "dashed", color = "red") +
  #geom_errorbar(aes(ymin=mean(EAGfixed$FGratioNew[EAGfixed$genre == "FG"])-sd(EAGfixed$FGratioNew[EAGfixed$genre == "FG"]), 
  #                  ymax=mean(EAGfixed$FGratioNew[EAGfixed$genre == "FG"])+sd(EAGfixed$FGratioNew[EAGfixed$genre == "FG"])), 
  #              width=.1, position=position_dodge(width=0.1)) +
  #geom_errorbar(aes(ymin=mean(EAGfixed$FGratioNew[EAGfixed$genre == "EAG"])-sd(EAGfixed$FGratioNew[EAGfixed$genre == "EAG"]), 
  #                  ymax=mean(EAGfixed$FGratioNew[EAGfixed$genre == "EAG"])+sd(EAGfixed$FGratioNew[EAGfixed$genre == "EAG"]), color = "red"), 
  #              width=.1, position=position_dodge(width=0.1)) +
#  geom_hline(yintercept = mean(EAGfixed$FGratioNew) + sd(EAGfixed$FGratioNew), linetype = "dashed") +
#  geom_hline(yintercept = mean(EAGfixed$FGratioNew) - sd(EAGfixed$FGratioNew), linetype = "dashed") +
  geom_text(aes(label=genre), position=position_dodge(width=0), vjust=-.025) +
  theme(axis.text.x=element_markdown(size=8, angle = 90))

ggplot(EAGfixed, aes(x = gutenberg_id, y = EAGratioNew, fill = genre)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(EAGfixed$EAGratioNew), linetype = "dashed") +
#  geom_hline(yintercept = mean(EAGfixed$EAGratioNew[EAGfixed$genre == "EAG"]), linetype = "dashed", color = "red") +
#  geom_hline(yintercept = mean(EAGfixed$EAGratioNew[EAGfixed$genre == "FG"]), linetype = "dashed", color = "blue") +
#  geom_errorbar(aes(ymin=mean(EAGfixed$EAGratioNew[EAGfixed$genre == "FG"])-sd(EAGfixed$EAGratioNew[EAGfixed$genre == "FG"]), 
#                                      ymax=mean(EAGfixed$EAGratioNew[EAGfixed$genre == "FG"])+sd(EAGfixed$EAGratioNew[EAGfixed$genre == "FG"])), 
#                                  width=.1, position=position_dodge(width=0.1)) +
#  geom_errorbar(aes(ymin=mean(EAGfixed$EAGratioNew[EAGfixed$genre == "EAG"])-sd(EAGfixed$EAGratioNew[EAGfixed$genre == "EAG"]), 
#                                      ymax=mean(EAGfixed$EAGratioNew[EAGfixed$genre == "EAG"])+sd(EAGfixed$EAGratioNew[EAGfixed$genre == "EAG"]), color = "red"), 
 #                                 width=.1, position=position_dodge(width=0.1)) +
#  geom_hline(yintercept = mean(EAGfixed$EAGratioNew) + sd(EAGfixed$EAGratioNew), linetype = "dashed") +
#  geom_hline(yintercept = mean(EAGfixed$EAGratioNew) - sd(EAGfixed$EAGratioNew), linetype = "dashed") +
  geom_text(aes(label=genre), position=position_dodge(width=0), vjust=-.025) +
  theme(axis.text.x=element_markdown(size=8, angle = 90))


## POS Analysis ##
EAGtext_posSum <- read.csv(file = "EAGtext_posSum.csv")
EAGfixed$title <- as.character(EAGfixed$title)
EAGtext_posSum$genre <- ""
#assign appropriate gnre to each title in POS dataframe
for(i in 1:nrow(EAGfixed)) {
  EAGtext_posSum$genre[EAGtext_posSum$title == EAGfixed$title[i]] <- EAGfixed$genre[i]  
}

#now to graph each text
highlight = function(x, pat, color="black", family="") {
  ifelse(grepl(pat, x), glue("<b style='font-family:{family}; color:{color}'>{x}</b>"), x)
}
ggplot(EAGtext_posSum, aes(x = Var1, y = Freq, color = genre)) +
  geom_point() +
  scale_x_discrete(labels=function(x) highlight(x, "JJ|NN|VB", "red")) +
  theme(axis.text.x=element_markdown(size=8, angle = 90)) 

#create summary dataframe so we compare the two genres directly
EAGtextPosTinyC <- summarySE(EAGtext_posSum, measurevar="Freq", groupvars=c("Var1","genre"))
ggplot(EAGtextPosTinyC, aes(x = Var1, y = Freq)) +
  geom_errorbar(aes(ymin=Freq-se, ymax=Freq+se), width=.1, position=position_dodge(width=0.1)) +
  geom_text(aes(label=genre), position=position_dodge(width=0.2), hjust=-.25) +
  geom_point(aes(color = genre)) +
  scale_x_discrete(labels=function(x) highlight(x, "JJ|NN|VB", "red")) +
  theme(axis.text.x=element_markdown(size=8, angle = 90))


