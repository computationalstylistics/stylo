
stylo.pronouns = function(corpus.lang = "English") {

  # pronouns (and other words) to be deleted
  # * what are the selection criteria used here? Personal, possessive, ...? *

  # English
  eng.pronouns = c("he", "her", "hers", "herself", "him", "himself", "his", 
    "i", "me", "mine", "my", "myself", "our", "ours", "ourselves", "she", 
    "thee", "their", "them", "themselves", "they", "thou", "thy", "thyself", 
    "us", "we", "ye", "you", "your", "yours", "yourself")
  # Latin
  lat.pronouns = c("ea", "eae", "eam", "earum", "eas", "ego", "ei", "eis", 
    "eius", "eo", "eorum", "eos", "eum", "id", "illa", "illae", "illam", 
    "illarum", "illas", "ille", "illi", "illis", "illius", "illo", "illorum", 
    "illos", "illud", "illum", "is", "me", "mea", "meae", "meam", "mearum", 
    "meas", "mei", "meis", "meo", "meos", "meorum", "meum", "meus", "mihi", 
    "nobis", "nos", "noster", "nostra", "nostrae", "nostram", "nostrarum", 
    "nostras", "nostri", "nostris", "nostro", "nostros", "nostrorum", 
    "nostrum", "sua", "suae", "suam", "suarum", "suas", "sui", "suis", "suo", 
    "suos", "suorum", "suum", "suus", "te", "tibi", "tu", "tua", "tuae", 
    "tuam", "tuarum", "tuas", "tui", "tuis", "tuo", "tuos", "tuorum", "tuum", 
    "tuus", "vester", "vestra", "vestrae", "vestram", "vestrarum", "vestras", 
    "vestri", "vestris", "vestro", "vestros", "vestrorum", "vestrum", "vobis", 
    "vos")
  # French
  fra.pronouns = c("je", "me", "moi", "tu", "te", "toi", "il", "elle", "le", 
    "la", "lui", "se", "lui", "elle", "soi", "nous", "vous", "ils", "elles", 
    "les", "leur", "se", "eux", "elles", "soi")
  # German
  ger.pronouns = c("ich", "mich", "mir", "mein", "meine", "meiner", "meines", 
    "du", "dich", "dir", "dein", "deine", "deiner", "deines", "er", "sich", 
    "ihr", "ihrer", "ihn", "ihnen", "sein", "seiner", "seines", "seine", 
    "sie", "wir", "uns", "unser", "unsere", "euch", "eure", "euer")
  # Italian
  ita.pronouns = c("ci", "gli", "io", "la", "le", "lei", "li", "loro", "lo", 
    "lui", "me", "mi", "noi", "si", "te", "ti", "tu", "vi", "voi", "egli", 
    "ella", "esso", "essa", "essi", "esse", "mio", "mia", "miei", "mie", 
    "tuo", "tua", "tuoi", "tue", "suo", "sua", "suoi", "sue", "nostro", 
    "nostra", "nostri", "nostre", "vostro", "vostra", "vostri", "vostre", 
    "loro", "loro", "loro", "loro")
  # Polish
  pol.pronouns = c("ci", "ciebie", "ci\304\231", "go", "ich", "im", "ja", 
    "j\304\205", "je", "jego", "jej", "jemu", "ma", "m\304\205", "me", "mego", 
    "mej", "memu", "mi", "mn\304\205", "mnie", "moi", "moich", "moim", 
    "moimi", "moja", "moj\304\205", "moje", "mojego", "mojej", "mojemu", 
    "m\303\263j", "mu", "my", "mych", "mym", "mymi", "nam", "nami", "nas", 
    "ni\304\205", "nich", "nie", "niego", "niej", "niemu", "nim", "nimi", 
    "on", "ona", "one", "oni", "ono", "swa", "sw\304\205", "swe", "swego", 
    "swej", "swemu", "swoi", "swoich", "swoim", "swoimi", "swoja", 
    "swoj\304\205", "swoje", "swojego", "swojej", "swojemu", "sw\303\263j", 
    "swych", "swym", "swymi", "tob\304\205", "tobie", "twa", "tw\304\205", 
    "twe", "twego", "twej", "twemu", "twoi", "twoich", "twoim", "twoimi", 
    "twoja", "twoj\304\205", "twoje", "twojego", "twojej", "twojemu", 
    "tw\303\263j", "twych", "twym", "twymi", "ty", "wam", "wami", "was", 
    "wy", "wasz", "wasza", "wasze", "waszym", "waszymi", "waszych", 
    "waszego", "waszej", "wasz\304\205")
  # Hungarian
  hun.pronouns = c("annak", "az", "azzal", "bele", "bel\303\251", 
    "bel\303\251d", "bel\303\251je", "bel\303\251j\303\274k", "bel\303\251m", 
    "bel\303\251nk", "bel\303\251tek", "bel\303\266le", "bel\305\221led", 
    "bel\305\221lem", "bel\305\221letek", "bel\305\221l\303\274k", 
    "bel\305\221l\303\274nk", "benne", "benned", "bennem", "bennetek", 
    "benn\303\274k", "benn\303\274nk", "\303\251n", "ennek", "eny\303\251im", 
    "eny\303\251m", "eny\303\251mek", "\303\251rte", "\303\251rted", 
    "\303\251rtem", "\303\251rtetek", "\303\251rt\303\274k", 
    "\303\251rt\303\274nk", "ez", "ezzel", "hozz\303\241", "hozz\303\241d", 
    "hozz\303\241ja", "hozz\303\241juk", "hozz\303\241m", "hozz\303\241nk", 
    "hozz\303\241tok", "maga", "mag\303\241\303\251", "mag\303\241\303\251i", 
    "maguk", "maguk\303\251", "maguk\303\251i", "mi", "mieink", "mienk", 
    "mi\303\251nk", "n\303\241la", "n\303\241lad", "n\303\241lam", 
    "n\303\241latok", "n\303\241luk", "n\303\241lunk", "neked", "nekem", 
    "neki", "nekik", "nektek", "nek\303\274nk", "\305\221", "\305\221k", 
    "\303\266n", "\303\266n\303\251", "\303\266n\303\251i", "\303\266nnek", 
    "\303\266nnel", "\303\266n\303\266k", "\303\266n\303\266k\303\251", 
    "\303\266n\303\266k\303\251i", "\303\266n\303\266kkel", 
    "\303\266n\303\266knek", "\303\266v\303\251", "\303\266v\303\251i", 
    "\303\266v\303\251ik", "\303\266v\303\251k", "r\303\241d", "r\303\241ja", 
    "rajta", "rajtad", "rajtam", "rajtatok", "rajtuk", "rajtunk", 
    "r\303\241juk", "r\303\241m", "r\303\241nk", "r\303\241tok", 
    "r\303\263la", "r\303\263lad", "r\303\263lam", "r\303\263latok", 
    "r\303\263luk", "r\303\263lunk", "te", "ti", "tied", "ti\303\251d", 
    "tieid", "tieitek ", "tietek", "ti\303\251tek", "t\305\221le", 
    "t\305\221led", "t\305\221lem", "t\303\266letek", "t\305\221l\303\274k", 
    "t\305\221l\303\274nk", "vele", "veled", "velem", "veletek", 
    "vel\303\274k", "vel\303\274nk")
  # Dutch
  dut.pronouns = c("hij", "haar", "haarzelf", "hijzelf", "hemzelf", "hem", 
    "ik", "ikzelf", "mijn", "mij", "mijzelf", "me", "mezelf", "zich", 
    "zichzelf", "ons", "onze", "onszelf", "u", "uw", "uzelf", "zij", 
    "zijzelf", "wij", "wijzelf", "jij", "jijzelf", "jouw", "jouwe", "jou", 
    "jouzelf", "elkaar", "hen", "henzelf", "hun", "hunzelf", "zich", 
    "elkaar", "wie", "wat", "welke")
  # Spanish
  sp.pronouns = c("yo", "me", "m\303\255", "t\303\272", "te", "t\303\255", "usted", 
    "ud", "le", "lo", "la", "se", "s\303\255", "\303\251l", "lo", "ella", 
    "nos", "nosotros", "nosotras", "vosotros", "vosotras", "ustedes", "ud", 
    "les", "los", "las", "se", "ellos", "los", "ellas", "os", "uds", "vos")

  # The chosen language option should be followed by an assignment of 
  # the appropriate set of pronouns. The following code is responsible for it

  # using the set of English pronouns as default
  pronouns = eng.pronouns

  # if other language has been chosen, the pronouns will be overwritten
  if(tolower(corpus.lang) == "polish") 
      pronouns = pol.pronouns 
  if(tolower(corpus.lang) == "latin" || tolower(corpus.lang) == "latin.corr")
      pronouns = lat.pronouns
  if(tolower(corpus.lang) == "french")
      pronouns = fra.pronouns
  if(tolower(corpus.lang) == "german" )
      pronouns = ger.pronouns
  if(tolower(corpus.lang) == "italian")
      pronouns = ita.pronouns
  if(tolower(corpus.lang) == "hungarian")
      pronouns = hun.pronouns
  if(tolower(corpus.lang) == "dutch")
      pronouns = dut.pronouns
  if(tolower(corpus.lang) == "spanish")
      pronouns = sp.pronouns

  # Windows users are a bit allergic to Unicode; let's make them happy
  # by converting the chosen set of pronouns to local encoding
  if(Sys.info()[["sysname"]] == "Windows") { 
    pronouns = iconv(pronouns, from = "UTF-8")
  }

return(pronouns)
}
