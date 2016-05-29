require(RUnit)
source("main/text.processing.R")

text.processing.tests <- function() {
    
    text.corpus <- c("short sentence", 
                     "shit always happens with good people",
                     "last night I saw a nightmare (you know this staff man do not wanna live after)",
                     "Write me on my email atrozkiy@wonderfullworld.com", 
                     "call @Justin if you want to dance to the death", 
                     "1. I'm not as small as a potato. 2nd Give me this banana", 
                     "look at www.popcornhub.com where every #beatiful_woman can show her big popcorn for only $27.97",
                     "information 100%, I promise",
                     "some other digits 123 1123  ",
                     "  looking for something?! here is terrible noizy",
                     "some   other punctuation $^$^$^#@!",
                     "Write me on my email atrozkiy@wonderfullworld.com",
                     " Вставь сюда немного арабского и странных символов, приятель ",
                     "WTF だから日本人のビットが十分に理解し、飛散する日本の必要性を理解します Japanese are incredible",
                     "Cheese don't like Mr. braun"
                     )
    text.processing.verif <- c("<BOS> <profanity> always happens with good people <EOS>",
                           "<BOS> last night <ich> saw a nightmare <EOS>",
                           "<BOS> write me on my email <email> <EOS>",
                           "<BOS> call <user> if you want to dance to the death <EOS>",
                           "<BOS> <ich> am not as small as a potato <EOS>",
                           "<BOS> <ordinal> give me this banana <EOS>",
                           "<BOS> look at <link> where every <hashtag> can show her big popcorn for only <money> <EOS>",
                           "<BOS> information <percent> <ich> promise <EOS>",
                           "<BOS> some other digits <othdigit> <EOS>",
                           "<BOS> looking for something <EOS>",
                           "<BOS> here is terrible noizy <EOS>",
                           "<BOS> some other punctuation <EOS>",
                           "<BOS> wtf <foreign> japanese are incredible <EOS>",
                           "<BOS> cheese do not like <mr> braun <EOS>",
                           "<BOS> you know this staff man do not wanna live after <EOS>"
                           )
    
    profanity.vec <- c("ssdfs", "shit")
    
    text.processing.result <- process.text(text.corpus,profanity.vec)
    text.processing.result.par <- process.text.par(text.corpus,profanity.vec)
    print(sprintf("text.processing status: %s", checkEquals(sort(text.processing.result), sort(text.processing.verif))))
    print(sprintf("text.processing.par status: %s", checkEquals(sort(text.processing.result.par), sort(text.processing.verif))))
    
    text.corpus2 <- c("today i am thinking of a million things that would take years to write things about love about longing about dreams things about you and me",
                      "we took the car he rented on the ferry with us over to a we got out of the car and walked over to the deck to look at the city skyline",
                      "i had the weirdest dreams last night" )
    vcb.verif <- c(" am","at","city","deck","ferry","got","had","he","last","longing",
                   "look","love","me","million","night","on","out","rented","skyline","take",
                   "that","thinking","today","took","us","walked","weirdest","with","would","write",
                   "years","you ")
    rr.vcb.res <- compute.rare.vocabulary(text.corpus2, 2)
    print(sprintf("compute.rare.vocabulary: %s", checkEquals(rr.vcb.res, vcb.verif)))
    
    text.corpus2.verif <- c("today i <unk> thinking of a <unk> things <unk> would <unk> years to <unk> things about <unk> about <unk> about dreams things about <unk> and me",
                            "we <unk> the car <unk> rented <unk> the <unk> with <unk> over to a we <unk> out of the car and <unk> over to the <unk> to <unk> at the <unk> skyline",
                            "i <unk> the <unk> dreams <unk> night")
    process.rare.res <- process.rare(text.corpus2, rr.vcb.res)
    process.rare.par.res <- process.rare.par(text.corpus2, rr.vcb.res)
    print(sprintf("process.rare: %s", checkEquals(process.rare.res, text.corpus2.verif)))
    print(sprintf("process.rare.par: %s", checkEquals(process.rare.par.res, text.corpus2.verif)))
}

