require(RUnit)
source("TSafer/main/text.processing.R")

text.processing.tests <- function() {
    
    text.corpus <- c("short sentence", 
                     "shit always happens with good people",
                     "last night I saw a nightmare (you know this staff man do not wanna live after)",
                     "Write me on my email atrozkiy@wonderfullworld.com", 
                     "call @Justin if you want to dance to the deth", 
                     "1. I'm not as small as a potato. 2nd Give me this banana", 
                     "look at www.popcornhub.com where every #beatiful_woman can show her big popcorn for only $27.97",
                     "information 100%, I promise",
                     "some other digits 123 1123  ",
                     "  looking for something?! here is terrible noizy",
                     "some   other punctuation $^$^$^#@!",
                     "Write me on my email atrozkiy@wonderfullworld.com",
                     " Вставь сюда немного арабского и странных символов, приятель ",
                     "WTH だから日本人のビットが十分に理解し、飛散する日本の必要性を理解します Japanese are incredible"
                     )
    text.processing.verif <- c("<BOS> <profanity> always happens with good people <EOS>",
                           "<BOS> last night <ich> saw a nightmare <EOS>",
                           "<BOS> you know this staff man do not wanna live after <EOS>",
                           "<BOS> write me on my email <email> <EOS>",
                           "<BOS> call <user> if you want to dance to the deth <EOS>",
                           "<BOS> 1. <ich>'m not as small as a potato <EOS>",
                           "<BOS> <ordinal> give me this banana <EOS>",
                           "<BOS> look at <link> where every <hashtag> can show her big popcorn for only <money> <EOS>",
                           "<BOS> information <percent> I promise <EOS>",
                           "<BOS> some other digits <othdigit> <EOS>",
                           "<BOS> looking for something <EOS>",
                           "<BOS> here is terrible noizy <EOS>",
                           "<BOS> some other punctuation <EOS>",
                           "<BOS> wth <foreign> japanese are incredible <EOS>"
                           )
    
    profanity.vec <- readRDS('TSafer/main/sources/profanity.rds')
    
    text.processing.result <- process.text(text.corpus,profanity.vec,verbose=T)
    
    checkEquals(text.processing.result, text.processing.verif)
}