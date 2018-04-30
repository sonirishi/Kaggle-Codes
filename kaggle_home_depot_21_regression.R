rm(list=ls(all=TRUE))
.libPaths(c( .libPaths(), "/app_2/rlibrary"))
library(dplyr)
library(stringr)
library(tm)
library(SnowballC)
library(stringdist)
library(xgboost)
library(data.table)
library(caret)
library(lsa)
library(rARPACK)

read_rows = -1

options(encoding = 'iso-8859-1')

full_data = read.csv("/mapr/projects/GLRM_test/HomeDepot/train.csv",nrow=read_rows)

attr_data = data.frame(fread("/mapr/projects/GLRM_test/HomeDepot/attributes.csv"))

replacePunctuation = function(x) {return (gsub("[[:punct:]]"," ", x))}

full_data$product_title = sapply(full_data$product_title,str_to_lower)

full_data$search_term = sapply(full_data$search_term,str_to_lower)

attr_data_1 = attr_data %>% group_by(product_uid) %>% summarise(desc = paste(value,collapse = ""))

attr_data_1$desc = sapply(attr_data_1$desc,str_to_lower)

attr_data_1 = attr_data_1 %>% mutate(value = gsub("  "," ",desc))

full_data = full_data %>% mutate(product_title = gsub("  "," ",product_title))

full_data = full_data %>% mutate(search_term = gsub("  "," ",search_term))

full_data = full_data %>% mutate(search_term = gsub("sq ft","sqft",search_term),
                                 search_term = gsub("sq feet","sqft",search_term),
                                 search_term = gsub("square feet","sqft",search_term),
                                 search_term = gsub("sq.ft.","sqft",search_term),
                                 search_term = gsub("gallon","gal.",search_term),
                                 search_term = gsub("ounce","oz",search_term),
                                 search_term = gsub("ounces","oz",search_term),
                                 search_term = gsub("cyprees","cypress",search_term),
                                 search_term = gsub("centimeter","cm",search_term),
                                 search_term = gsub("silestone","silestone",search_term),
                                 search_term = gsub("conditionerunit","conditioner unit",search_term),
                                 search_term = gsub("insullation","insulation",search_term),
                                 search_term = gsub("spliterair","spliter air",search_term),
                                 search_term = gsub("air condit ","air conditioner",search_term),
                                 search_term = gsub("window air condit","window air conditioner",search_term),
                                 search_term = gsub("molding","moulding",search_term),
                                 search_term = gsub("milimeter","mm",search_term),
                                 search_term = gsub("pound","lb.",search_term),
                                 search_term = gsub("windowair","window air",search_term),
                                 search_term = gsub("mobilehome","mobile home",search_term),
                                 search_term = gsub("kitchaid","kitchenaid",search_term),
                                 search_term = gsub("wallmount","wall mount",search_term),
                                 search_term = gsub("daltiles","daltile",search_term),
                                 search_term = gsub("feet","ft.",search_term),
                                 search_term = gsub("inches","in.",search_term),
                                 search_term = gsub("inch","in.",search_term),
                                 search_term = gsub("cellphone","cell phone",search_term),
                                 search_term = gsub("otr","over the range",search_term),
                                 search_term = gsub("toliet","toilet",search_term),
                                 search_term = gsub("a/c","air conditioner",search_term),
                                 search_term = gsub("whirpool","whirlpool",search_term),
                                 search_term = gsub("chesapeke","chesapeake",search_term),
                                 search_term = gsub("masonary","masonry",search_term),
                                 search_term = gsub("hoinda","honda",search_term),
                                 search_term = gsub("jeffery","jeffrey",search_term),
                                 search_term = gsub("alure","allure",search_term),
                                 search_term = gsub("vinal","vinyl",search_term),
                                 search_term = gsub("ampere","amp",search_term),
                                 search_term = gsub("vynal","vinyl",search_term),
                                 search_term = gsub("buikids","buy kids",search_term),
                                 search_term = gsub("heath zenity","heath-zenith",search_term),
                                 search_term = gsub("heith-zenith","heath-zenith",search_term),
                                 search_term = gsub("sikalatexr","sikalatex",search_term),
                                 search_term = gsub("robutussin","robitussin",search_term),
                                 search_term = gsub("airconditioner","air conditioner",search_term),
                                 search_term = gsub("frigadaire","frigidaire",search_term),
                                 search_term = gsub("versabon","versabond",search_term),
                                 search_term = gsub("lihonia","lithonia",search_term),
                                 search_term = gsub("preature treated","pressure treated",search_term),
                                 search_term = gsub("lasron","larson",search_term),
                                 search_term = gsub("maytab","maytag",search_term),
                                 search_term = gsub("shelfs","shelves",search_term),
                                 search_term = gsub("pivot angle","pivot angle",search_term),
                                 search_term = gsub("telemechanic square","telemecanique square",search_term),
                                 search_term = gsub("whirlpoolga","milwaukee",search_term),
                                 search_term = gsub("miwaukee","whirlpool ga",search_term),
                                 search_term = gsub(" ac ","air conditioner",search_term),
                                 search_term = gsub("whirlpoolstainless","whirlpool stainless",search_term),
                                 search_term = gsub("pedistal","pedestal",search_term),
                                 search_term = gsub("condtioner","conditioner",search_term),
                                 search_term = gsub("pannel","panel",search_term),
                                 search_term = gsub("greecianmarble","greecian marble",search_term),
                                 search_term = gsub("high boy","highboy",search_term),
                                 search_term = gsub("plexigla","plexi gla",search_term),
                                 search_term = gsub("snowbl","snow bl",search_term),
                                 search_term = gsub("skill","skil",search_term),
                                 search_term = gsub(" one "," 1 ",search_term),
                                 search_term = gsub(" two "," 2 ",search_term),
                                 search_term = gsub(" three "," 3 ",search_term),
                                 search_term = gsub(" four "," 4 ",search_term),
                                 search_term = gsub(" five "," 5 ",search_term),
                                 search_term = gsub(" six "," 6 ",search_term),
                                 search_term = gsub(" seven "," 7 ",search_term),
                                 search_term = gsub(" eight "," 8 ",search_term),
                                 search_term = gsub(" nine "," 9 ",search_term),
                                 search_term = gsub(" zero "," 0 ",search_term),
                                 search_term = gsub("vynil","vinyl",search_term),
                                 search_term = gsub("baracket","bracket",search_term),
                                 search_term = gsub("excaust","exhaust",search_term),
                                 search_term = gsub("phlne","phone",search_term),
                                 search_term = gsub("milwakee","milwaukee",search_term),
                                 search_term = gsub("rustoleum","rust-oleum",search_term),
                                 search_term = gsub("vinil","vinyl",search_term),
                                 search_term = gsub("flors","floors",search_term),
                                 search_term = gsub("steqamers","steamers",search_term),
                                 search_term = gsub("silocoln","silicone",search_term),
                                 search_term = gsub("rustollum","rust-oleum",search_term),
                                 search_term = gsub("lawnmower","lawn mower",search_term),
                                 search_term = gsub("aire acondicionado","air conditioner",search_term),
                                 search_term = gsub("enrty","entry",search_term),
                                 search_term = gsub("closetmade","closetmaid",search_term),
                                 search_term = gsub("garge","garage",search_term),
                                 search_term = gsub("compressro","compressor",search_term),
                                 search_term = gsub("kohlor","kohler",search_term),
                                 search_term = gsub("scurity","security",search_term),
                                 search_term = gsub("rigid ","ridgid",search_term),
                                 search_term = gsub("koehler","kohler",search_term),
                                 search_term = gsub("spectrazide","spectracide",search_term),
                                 search_term = gsub("hamptom","hampton",search_term),
                                 search_term = gsub("phillips","philips",search_term),
                                 search_term = gsub("marrazi","marazzi",search_term),
                                 search_term = gsub("clacier","glacier",search_term),
                                 search_term = gsub("glazer","glacier",search_term),
                                 search_term = gsub("glaciar","glacier",search_term),
                                 search_term = gsub("bath rom","bathroom",search_term),
                                 search_term = gsub("edsel","edsal",search_term),
                                 search_term = gsub("murry","murray",search_term),
                                 search_term = gsub(" sinl "," sink ",search_term),
                                 search_term = gsub("deck over","deckover",search_term),
                                 search_term = gsub("sprkinler","sprinkler",search_term),
                                 search_term = gsub("ourdoor","outdoor",search_term),
                                 search_term = gsub("valvue","valve",search_term),
                                 search_term = gsub("outdoorlounge","outdoor lounge",search_term),
                                 search_term = gsub("outdoorfurniture","outdoor furniture",search_term),
                                 search_term = gsub("tolet","toilet",search_term),
                                 search_term = gsub("basemetnt","basement",search_term),
                                 search_term = gsub("heaterconditioner","heater conditioner",search_term),
                                 search_term = gsub("steele","steel",search_term),
                                 search_term = gsub(" stell "," steel ",search_term),
                                 search_term = gsub(" fece "," fence ",search_term),
                                 search_term = gsub("tiolet","toilet",search_term),
                                 search_term = gsub("gas wayer","gas water",search_term),
                                 search_term = gsub("berh","behr",search_term),
                                 search_term = gsub("thower","thrower",search_term),
                                 search_term = gsub("base board","baseboard",search_term),
                                 search_term = gsub("boundle","bundle",search_term),
                                 search_term = gsub(" sping "," spring ",search_term),
                                 search_term = gsub(" desalt "," dewalt ",search_term),
                                 search_term = gsub("repir","repair",search_term),
                                 search_term = gsub("condtioners","conditioners",search_term),
                                 search_term = gsub("pannel","panel",search_term),
                                 search_term = gsub(" sruds "," studs ",search_term),
                                 search_term = gsub("florescent","fluorescent",search_term),
                                 search_term = gsub("flourescent","fluorescent",search_term),
                                 search_term = gsub("electic","electric",search_term),
                                 search_term = gsub("greecianmarble","grecian marble",search_term),
                                 search_term = gsub("montagnia contina floor tile","montagna cortina floor tile",search_term),
                                 search_term = gsub("porcelin","porcelain",search_term),
                                 search_term = gsub("miricale","miracle",search_term),
                                 search_term = gsub("windos","windows",search_term),
                                 search_term = gsub("vaccum","vacuum",search_term),
                                 search_term = gsub("heather","heater",search_term),
                                 search_term = gsub("bagged cinder mulch","bagged cedar mulch",search_term),
                                 search_term = gsub("hindges","hinges",search_term),
                                 search_term = gsub("hieght","height",search_term),
                                 search_term = gsub("celling","ceiling",search_term),
                                 search_term = gsub("procelian","porcelain",search_term),
                                 search_term = gsub("dewalr","dewalt",search_term),
                                 search_term = gsub("cieling","ceiling",search_term),
                                 search_term = gsub("roybi","ryobi",search_term),
                                 search_term = gsub("cordlessrotary hammerss","cordless rotary hammers",search_term),
                                 search_term = gsub("bathro ","bathroom",search_term),
                                 search_term = gsub("barcello","bercello",search_term),
                                 search_term = gsub("aluminun","aluminum",search_term),
                                 search_term = gsub("lightening","lighting",search_term),
                                 search_term = gsub("fixtues","fixtures",search_term),
                                 search_term = gsub("swantone","swanstone",search_term),
                                 search_term = gsub("everblit","everbilt",search_term),
                                 search_term = gsub("alumanam","aluminum",search_term),
                                 search_term = gsub("fencde","fence",search_term),
                                 search_term = gsub("untility","utility",search_term),
                                 search_term = gsub("paito","patio",search_term),
                                 search_term = gsub("mowre","mower",search_term),
                                 search_term = gsub("ridding","riding",search_term),
                                 search_term = gsub("medicn","medicine",search_term),
                                 search_term = gsub("qtr","quarter",search_term),
                                 search_term = gsub("infered","infrared",search_term),
                                 search_term = gsub("blk pipe","black pipe",search_term),
                                 search_term = gsub("craft an lawn mower","craftsman lawn mower",search_term),
                                 search_term = gsub("tpoilet","toilet",search_term),
                                 search_term = gsub("dhower","shower",search_term),
                                 search_term = gsub("hindged","hinged",search_term),
                                 search_term = gsub("canopie","canopy",search_term),
                                 search_term = gsub("chanpayne","champagne",search_term),
                                 search_term = gsub("ceramick","ceramic",search_term),
                                 search_term = gsub("celing","ceiling",search_term),
                                 search_term = gsub("n9ickel","nickel",search_term),
                                 search_term = gsub("extention","extension",search_term),
                                 search_term = gsub(" gaint "," giant ",search_term),
                                 search_term = gsub("chainlink","chain link",search_term),
                                 search_term = gsub("linoliuml","linoleum",search_term),
                                 search_term = gsub("hagchet","hatchet",search_term),
                                 search_term = gsub("microwarve","microwave",search_term),
                                 search_term = gsub("backsplach","backsplash",search_term),
                                 search_term = gsub("rayoby","ryobi",search_term),
                                 search_term = gsub("naturlas","naturals",search_term),
                                 search_term = gsub("dog fence batt","dog fence battery",search_term),
                                 search_term = gsub("onda pressure washer","honda pressure washer",search_term),
                                 search_term = gsub("fridgdare","frigidaire",search_term),
                                 search_term = gsub("pain windows","pane windows",search_term),
                                 search_term = gsub("robi battery","ryobi battery",search_term),
                                 search_term = gsub("weewacker edger","weed wacker edger",search_term),
                                 search_term = gsub("forimca","formica",search_term),
                                 search_term = gsub("repir cuplings","repair couplings",search_term),
                                 search_term = gsub("hoursepower","horsepower",search_term),
                                 search_term = gsub("gauze","gauge",search_term),
                                 search_term = gsub("didger","dodger",search_term),
                                 search_term = gsub("accordian","accordion",search_term),
                                 search_term = gsub("4x6treaded wood","4x6 treated wood",search_term),
                                 search_term = gsub("preature treated lumber","pressure treated lumber",search_term),
                                 search_term = gsub("closetmade wood","closetmaid wood",search_term),
                                 search_term = gsub("cleanerm mop","cleaner mop",search_term),
                                 search_term = gsub(" shads "," shades ",search_term),
                                 search_term = gsub("contracor","contractor",search_term),
                                 search_term = gsub("rust oleum","rust-oleum",search_term),
                                 search_term = gsub("selves","shelves",search_term),
                                 search_term = gsub("hecurles","hercules",search_term),
                                 search_term = gsub("anderson","andersen",search_term),
                                 search_term = gsub("lasron","larson",search_term),
                                 search_term = gsub("refridgerator","refrigerator",search_term),
                                 search_term = gsub("structue","structure",search_term),
                                 search_term = gsub("cahnnel","channel",search_term),
                                 search_term = gsub("conner","corner",search_term),
                                 search_term = gsub("planel glue","panel glue",search_term),
                                 search_term = gsub("ventenatural","vented natural",search_term),
                                 search_term = gsub("swivrl","swivel",search_term),
                                 search_term = gsub("galvinized","galvanized",search_term),
                                 search_term = gsub("fictures","fixtures",search_term),
                                 search_term = gsub("plasticbathroom","plastic bathroom",search_term),
                                 search_term = gsub("jeffery","jeffrey",search_term),
                                 search_term = gsub("fireplacewater","fireplace water",search_term),
                                 search_term = gsub("waxhers","washers",search_term),
                                 search_term = gsub("genecrac","generac",search_term),
                                 search_term = gsub("acrtlic","acrylic",search_term),
                                 search_term = gsub("padio","patio",search_term),
                                 search_term = gsub("cyculer","circular",search_term),
                                 search_term = gsub("micro wave","microwave",search_term),
                                 search_term = gsub("utility traiter","utility trailer",search_term),
                                 search_term = gsub("consertrated","concentrate",search_term),
                                 search_term = gsub("vynik","vinyl",search_term),
                                 search_term = gsub("riobi","ryobi",search_term),
                                 search_term = gsub("recepicating","reciprocating",search_term),
                                 search_term = gsub("step latter","step ladder",search_term),
                                 search_term = gsub("meguire","meguiar",search_term),
                                 search_term = gsub("refrdgerators","refrigerators",search_term),
                                 search_term = gsub("colbolt","cobalt",search_term),
                                 search_term = gsub("falcet","faucet",search_term),
                                 search_term = gsub("mantle","mantel",search_term),
                                 search_term = gsub("rachet scret drivers","ratchet screwdrivers",search_term),
                                 search_term = gsub("ruotor table","router table",search_term),
                                 search_term = gsub("decoritive","decorative",search_term),
                                 search_term = gsub("threashold","threshold",search_term),
                                 search_term = gsub("horizantel","horizontal",search_term),
                                 search_term = gsub("attatchmens","attachments",search_term),
                                 search_term = gsub("wrachet","ratchet",search_term),
                                 search_term = gsub("waterroo","waterproof",search_term),
                                 search_term = gsub("ftawning","ft awning",search_term),
                                 search_term = gsub("florecent","fluorescent",search_term),
                                 search_term = gsub("hamptom bay cusion","hampton bay cushion",search_term),
                                 search_term = gsub("marrazi","marazzi",search_term),
                                 search_term = gsub("untique","antique",search_term),
                                 search_term = gsub("dinning","dining",search_term),
                                 search_term = gsub("colpay","clopay",search_term),
                                 search_term = gsub("cicular","circular",search_term),
                                 search_term = gsub("snowerblower","snowblower",search_term),
                                 search_term = gsub("birkmann","brinkmann",search_term),
                                 search_term = gsub("sofet","soffit",search_term),
                                 search_term = gsub("phillips","philips",search_term),
                                 search_term = gsub("residental","residential",search_term),
                                 search_term = gsub("luever","leuver",search_term),
                                 search_term = gsub("hamiltton","hamilton",search_term),
                                 search_term = gsub("andersor","andersen",search_term),
                                 search_term = gsub("florisant","fluorescent",search_term),
                                 search_term = gsub("pendent","pendant",search_term),
                                 search_term = gsub("kennal kit","kennel kit",search_term),
                                 search_term = gsub("chesapeke","chesapeake",search_term),
                                 search_term = gsub("raidos","radios",search_term),
                                 search_term = gsub("qucikie","quickie",search_term),
                                 search_term = gsub("napolian","napoleon",search_term),
                                 search_term = gsub("riverera","riviera",search_term),
                                 search_term = gsub("accesories","accessories",search_term),
                                 search_term = gsub("slyvanna","sylvania",search_term),
                                 search_term = gsub("beveragecooler","beverage cooler",search_term),
                                 search_term = gsub("ureka","eureka",search_term),
                                 search_term = gsub("unbralla","umbrella",search_term),
                                 search_term = gsub("asathbula","ashtabula",search_term),
                                 search_term = gsub("elerical","electrical",search_term),
                                 search_term = gsub("mansonry","radios",search_term),
                                 search_term = gsub("industreial","industrial",search_term),
                                 search_term = gsub("martha steward","martha stewart",search_term),
                                 search_term = gsub("canopie","canopy",search_term),
                                 search_term = gsub("flourescent","fluorescent",search_term),
                                 search_term = gsub("daucet","faucet",search_term),
                                 search_term = gsub("millwaukee","milwaukee",search_term),
                                 search_term = gsub("boscj bit","bosch bit",search_term),
                                 search_term = gsub("barbque","barbecue",search_term),
                                 search_term = gsub("brinkman","brinkmann",search_term),
                                 search_term = gsub("malbu","malibu",search_term),
                                 search_term = gsub("stone venner sequia","stone veneer sequoia",search_term),
                                 search_term = gsub("chrisymas","christmas",search_term),
                                 search_term = gsub("refigrator","refrigerator",search_term),
                                 search_term = gsub("cabinent","cabinet",search_term),
                                 search_term = gsub("hasmmock bed","hammock bed",search_term),
                                 search_term = gsub("idylus","idylis",search_term),
                                 search_term = gsub("parquee","parquet",search_term),
                                 search_term = gsub("dermal","dremel",search_term),
                                 search_term = gsub("gardinias","gardenias",search_term),
                                 search_term = gsub("942196brinkmann","942196 brinkmann",search_term),
                                 search_term = gsub("bouganvilla","bougainvillea",search_term),
                                 search_term = gsub("nozzel","nozzle",search_term),
                                 search_term = gsub("acrilic","acrylic",search_term),
                                 search_term = gsub("gibraltor","gibraltar",search_term),
                                 search_term = gsub("balist","ballast",search_term),
                                 search_term = gsub("john deer ","john deere ",search_term),
                                 search_term = gsub("walloven","wall oven",search_term),
                                 search_term = gsub("frigidare","frigidaire",search_term),
                                 search_term = gsub("asburn","ashburn",search_term),
                                 search_term = gsub("reheem","rheem",search_term),
                                 search_term = gsub("dewaqlt","dewalt",search_term),
                                 search_term = gsub("clyvus","clivus",search_term),
                                 search_term = gsub("cadelabra","candelabra",search_term),
                                 search_term = gsub("elvies ","elvis ",search_term),
                                 search_term = gsub("gentec ","gentex ",search_term),
                                 search_term = gsub("vogoro flower","vigoro flower",search_term),
                                 search_term = gsub("caroilne","caroline",search_term),
                                 search_term = gsub("martha suart","martha stewart",search_term),
                                 search_term = gsub("swiming","swimming",search_term),
                                 search_term = gsub("klien","klein",search_term),
                                 search_term = gsub("huskie","husky",search_term),
                                 search_term = gsub("levinton","leviton",search_term),
                                 search_term = gsub("milwaulkee","milwaukee",search_term),
                                 search_term = gsub("shelter logic","shelterlogic",search_term),
                                 search_term = gsub("fisher and penkel","fisher and paykel",search_term),
                                 search_term = gsub("simpon strong","simpson strong",search_term),
                                 search_term = gsub("tribecia","tribeca",search_term),
                                 search_term = gsub("conditionerriding","conditioner riding",search_term),
                                 search_term = gsub("veraluz","varaluz",search_term),
                                 search_term = gsub("kholer","kohler",search_term),
                                 search_term = gsub("shluter","schluter",search_term),
                                 search_term = gsub("greak","greek",search_term),
                                 search_term = gsub("gekko gauges","gecko gauges",search_term),
                                 search_term = gsub("pnematic","pneumatic",search_term),
                                 search_term = gsub("pegro xp","pergo xp",search_term),
                                 search_term = gsub("fiskers","fiskars",search_term),
                                 search_term = gsub("jimmyproof","jimmy proof",search_term),
                                 search_term = gsub("monococcon","monococcion",search_term),
                                 search_term = gsub("savavieh soho","safavieh soho",search_term),
                                 search_term = gsub("minala rope","manila rope",search_term),
                                 search_term = gsub("kraylon","krylon",search_term),
                                 search_term = gsub("mystick","mystik",search_term),
                                 search_term = gsub("kliz primers","kilz primers",search_term),
                                 search_term = gsub("rust oleam","rust-oleum",search_term),
                                 search_term = gsub("playststion","playstation",search_term),
                                 search_term = gsub("perferated","perforated",search_term),
                                 search_term = gsub("hamptonbay","hampton bay",search_term),
                                 search_term = gsub("chlorox","clorox",search_term),
                                 search_term = gsub("divonshire","devonshire",search_term),
                                 search_term = gsub("matage double","maytag double",search_term),
                                 search_term = gsub("tensil ties","tinsel ties",search_term),
                                 search_term = gsub("bonda body filler","bondo body filler",search_term),
                                 search_term = gsub("bousch","bosch",search_term),
                                 search_term = gsub("apriaire","aprilaire",search_term),
                                 search_term = gsub("hoovwe cordless","hoover cordless",search_term),
                                 search_term = gsub("perenial","perennial",search_term),
                                 search_term = gsub("baraar emy","bazaar emy",search_term),
                                 search_term = gsub("weelbarrow","wheelbarrow",search_term),
                                 search_term = gsub("zinser stain","zinsser stain",search_term),
                                 search_term = gsub("centipe grass seed","centipede grass seed",search_term),
                                 search_term = gsub("azelas","azaleas",search_term),
                                 search_term = gsub("suny citrus","sunny citrus",search_term),
                                 search_term = gsub("tuscon patio","tucson patio",search_term),
                                 search_term = gsub(" valae "," valve ",search_term),
                                 search_term = gsub("milwaukie","milwaukee",search_term),
                                 search_term = gsub(" gfi "," gfci ",search_term),
                                 search_term = gsub("emparador","emperador",search_term),
                                 search_term = gsub("moem faucet repair","moen faucet repair",search_term),
                                 search_term = gsub("acuria ","acurio ",search_term),
                                 search_term = gsub("arbourist ","arborist ",search_term),
                                 search_term = gsub("dealt portable ","dewalt portable ",search_term),
                                 search_term = gsub("taracota drain","terracotta drain",search_term),
                                 search_term = gsub("termini mosquito","terminix mosquito",search_term),
                                 search_term = gsub("bostic wood","bostik wood",search_term),
                                 search_term = gsub("surveilance","surveillance",search_term),
                                 search_term = gsub("lazer","laser",search_term),
                                 search_term = gsub("webber","weber",search_term),
                                 search_term = gsub("bbostitch","bostitch",search_term),
                                 search_term = gsub("vitarera","viatera",search_term),
                                 search_term = gsub("rostoluem","rust-oleum",search_term),
                                 search_term = gsub("gerber ","gerbera ",search_term),
                                 search_term = gsub("saw zall","sawzall",search_term),
                                 search_term = gsub("facia corner","fascia corner",search_term),
                                 search_term = gsub("ghroe shower","grohe shower",search_term),
                                 search_term = gsub("portercable","porter cable",search_term),
                                 search_term = gsub("rybi ","ryobi ",search_term),
                                 search_term = gsub("talsrar","talstar",search_term),
                                 search_term = gsub("cender block","cinder block",search_term),
                                 search_term = gsub("fridigidaire","frigidaire",search_term),
                                 search_term = gsub("maratha ","martha ",search_term),
                                 search_term = gsub("clacer bay","glacier bay",search_term),
                                 search_term = gsub("royobi","ryobi",search_term),
                                 search_term = gsub("armstroung ","armstrong ",search_term),
                                 search_term = gsub("hampden bay","hampton bay",search_term),
                                 search_term = gsub("facuet","faucet",search_term),
                                 search_term = gsub("mikita cordless","makita cordless",search_term),
                                 search_term = gsub("samsong stive","samsung stove",search_term),
                                 search_term = gsub("residentialsteel","residential steel",search_term),
                                 search_term = gsub("wirlpool","whirlpool",search_term),
                                 search_term = gsub("moehn kitchen","moen kitchen",search_term),
                                 search_term = gsub("powervent","power vent",search_term),
                                 search_term = gsub("stewartcabinet","stewart cabinet",search_term),
                                 search_term = gsub("samsungelectric","samsung electric",search_term),
                                 search_term = gsub("richman water","richmond water",search_term),
                                 search_term = gsub("oscilliat","oscillat",search_term),
                                 search_term = gsub("huskavarna","husqvarna",search_term),
                                 search_term = gsub("kholerhighland","kohler highline",search_term),
                                 search_term = gsub("huskey 18","husky 18",search_term),
                                 search_term = gsub("rustolem","rust-oleum",search_term),
                                 search_term = gsub("glazer bay","glacier bay",search_term),
                                 search_term = gsub("floorring","flooring",search_term),
                                 search_term = gsub("faucetskitchen","faucets kitchen",search_term),
                                 search_term = gsub("wiemans","weimans",search_term),
                                 search_term = gsub("hampton bat","hampton bay",search_term),
                                 search_term = gsub("diamondplate","diamond plate",search_term),
                                 search_term = gsub("boshe ","bosch ",search_term),
                                 search_term = gsub("g e micewave ","ge microwave ",search_term),
                                 search_term = gsub("golith","goliath",search_term),
                                 search_term = gsub("ralph laren","ralph lauren",search_term),
                                 search_term = gsub("n utdriver","nut driver",search_term),
                                 search_term = gsub("wesleyand","wesleyan",search_term),
                                 search_term = gsub("kaorik wine","kalorik wine",search_term),
                                 search_term = gsub("colaroo","coolaroo",search_term),
                                 search_term = gsub("rubberaid","rubbermaid",search_term),
                                 search_term = gsub("thermadore","thermador",search_term),
                                 search_term = gsub("milwalke","milwaukee",search_term),
                                 search_term = gsub("milwankee","milwaukee",search_term),
                                 search_term = gsub("almeda hickory","alameda hickory",search_term),
                                 search_term = gsub("seimens","siemens",search_term),
                                 search_term = gsub("whitesilicone","white silicone",search_term),
                                 search_term = gsub("kkohler","kohler",search_term),
                                 search_term = gsub("cerowire","cerro wire",search_term),
                                 search_term = gsub("rusteoulm","rust-oleum",search_term),
                                 search_term = gsub("quickrete","quikrete",search_term),
                                 search_term = gsub("lampost","lamp post",search_term),
                                 search_term = gsub("honolule","honolulu",search_term),
                                 search_term = gsub("vigaro","vigoro",search_term),
                                 search_term = gsub("fiet","feit",search_term),
                                 search_term = gsub("hardi lap","hardie lap",search_term),
                                 search_term = gsub("askley","ashley",search_term),
                                 search_term = gsub("maytab bravos","maytag bravos",search_term),
                                 search_term = gsub("milwoki","milwaukee",search_term),
                                 search_term = gsub("verathane","varathane",search_term),
                                 search_term = gsub("glaciar","glacier",search_term),
                                 search_term = gsub("morola tile","merola tile",search_term))

full_data = full_data %>% mutate(product_title = gsub("sq ft","sqft",product_title),
                                 product_title = gsub("sq feet","sqft",product_title),
                                 product_title = gsub("square feet","sqft",product_title),
                                 product_title = gsub("sq.ft.","sqft",product_title),
                                 product_title = gsub("gallon","gal.",product_title),
                                 product_title = gsub("ounce","oz",product_title),
                                 product_title = gsub("ounces","oz",product_title),
                                 product_title = gsub("centimeter","cm",product_title),
                                 product_title = gsub("milimeter","mm",product_title),
                                 product_title = gsub("pound","lb.",product_title),
                                 product_title = gsub("feet","ft.",product_title),
                                 product_title = gsub("inches","in.",product_title),
                                 product_title = gsub("inch","in.",product_title),
                                 product_title = gsub("cellphone","cell phone",product_title),
                                 product_title = gsub("a/c","air conditioner",product_title),
                                 product_title = gsub("otr","over the range",product_title))

attr_data_1 = attr_data_1 %>% mutate(desc = gsub("sq ft","sqft",desc),
                                     desc = gsub("sq feet","sqft",desc),
                                     desc = gsub("square feet","sqft",desc),
                                     desc = gsub("sq.ft.","sqft",desc),
                                     desc = gsub("gallon","gal.",desc),
                                     desc = gsub("ounce","oz",desc),
                                     desc = gsub("ounces","oz",desc),
                                     desc = gsub("centimeter","cm",desc),
                                     desc = gsub("milimeter","mm",desc),
                                     desc = gsub("pound","lb.",desc),
                                     desc = gsub("feet","ft.",desc),
                                     desc = gsub("inches","in.",desc),
                                     desc = gsub("inch","in.",desc),
                                     desc = gsub("cellphone","cell phone",desc),
                                     desc = gsub("a/c","air conditioner",desc),
                                     desc = gsub("otr","over the range",desc))

################ Brand flag ###########################

brand_name = attr_data %>% filter(name == "MFG Brand Name")

rm(attr_data)

brand_name$value = sapply(brand_name$value,str_to_lower)

desc1 = tm_map(Corpus(VectorSource(brand_name$value)),removeWords, stopwords('english'))
desc1 = tm_map(desc1,replacePunctuation)

brand_name$value_stem = as.character(tm_map(desc1,stemDocument))

rm(desc1)

brand_name = brand_name %>% mutate(value_stem = gsub("  "," ",value_stem))

brand_name = brand_name %>% mutate(value_stem_rem = 
                                     sapply(value_stem,
                                            function(x){return(gsub("[[:space:]]","",x))}))

##################### Stemming of the data ####################

desc = tm_map(Corpus(VectorSource(attr_data_1$desc)),removeWords, stopwords('english'))

desc = tm_map(desc,replacePunctuation)

attr_data_1$desc_stem = as.character(tm_map(desc,stemDocument))

rm(desc)

attr_data_1 = attr_data_1[,-2]

product_title_stem = tm_map(Corpus(VectorSource(full_data$product_title)),removeWords, stopwords('english'))
product_title_stem = tm_map(product_title_stem,replacePunctuation)

search_term_stem = tm_map(Corpus(VectorSource(full_data$search_term)),removeWords, stopwords('english'))
search_term_stem = tm_map(search_term_stem,replacePunctuation)

full_data$product_title_stem = as.character(tm_map(product_title_stem,stemDocument))
full_data$search_term_stem = as.character(tm_map(search_term_stem,stemDocument))

rm(product_title_stem)
rm(search_term_stem)

attr_data_1 = attr_data_1 %>% mutate(desc_stem = gsub("  "," ",desc_stem))

full_data = full_data %>% mutate(product_title_stem = gsub("  "," ",product_title_stem))

full_data = full_data %>% mutate(search_term_stem = gsub("  "," ",search_term_stem))

full_data = full_data %>% mutate(title_len = 
                                   str_length(sapply(product_title_stem,
                                                     function(x){return(gsub("[[:space:]]","",x))})))

full_data = full_data %>% mutate(len_search = 
                                   str_length(sapply(search_term_stem,
                                                     function(x){return(gsub("[[:space:]]","",x))})))

for(i in 1:nrow(full_data)){
  search = str_split(full_data$search_term_stem[i]," ")
  search = str_trim(setdiff(search[[1]],""))
  product_title = str_split(full_data$product_title_stem[i]," ")
  product_title = str_trim(setdiff(product_title[[1]],""))
  length_prod = str_length(product_title)
  length_search_tot = sum(str_length(search))
  search_len = length(search)
  match = 0
  temp=as.data.frame(matrix(rep(0,search_len),search_len,1))
  hamming_min = vector()
  lv_min = vector()
  osa_min = vector()
  dl_min = vector()
  lcs_min = vector()
  soundex_min = vector()
  search_left = ""
  string_length = 0
  for (j in 1:search_len){
    if(length(search[j] > 0)){
      if(search[j] %in% product_title)
      {
        match = match + 1
        search_left = paste("",search_left,sep=" ",collapse=NULL)
        string_length = string_length + str_length(str_trim(search[j]))
        if(length(which(product_title %in% search[j]))>0)
        {
          val_index = which(product_title %in% search[j])
          temp[j,"index"] = val_index
          temp[j,"len_str"] = sum(length_prod[1:val_index])
          temp[j,"len_char"] = length_prod[val_index]
        } 
      } else{
        search_left = paste(search_left,search[j],sep=" ",collapse=NULL)
      }
    }
    
    hamming_dist = lapply(product_title,function(x){stringdist(search[j],x,method="hamming")})
    lv_dist = lapply(product_title,function(x){stringdist(search[j],x,method="lv")})
    osa_dist = lapply(product_title,function(x){stringdist(search[j],x,method="osa")})
    dl_dist = lapply(product_title,function(x){stringdist(search[j],x,method="dl")})
    lcs_dist = lapply(product_title,function(x){stringdist(search[j],x,method="lcs")})
    soundex_dist = lapply(product_title,function(x){stringdist(search[j],x,method="soundex")})
    
    hamming_min[j] = min(unlist(hamming_dist))
    hamming_min[j] = ifelse(hamming_min[j] == Inf,str_length(str_trim(search[j])),hamming_min[j])
    lv_min[j] = min(unlist(lv_dist))
    lv_min[j] = ifelse(lv_min[j] == Inf,str_length(str_trim(search[j])),lv_min[j])
    osa_min[j] = min(unlist(osa_dist))
    osa_min[j] = ifelse(osa_min[j] == Inf,str_length(str_trim(search[j])),osa_min[j])
    dl_min[j] = min(unlist(dl_dist))
    dl_min[j] = ifelse(dl_min[j] == Inf,str_length(str_trim(search[j])),dl_min[j])
    lcs_min[j] = min(unlist(lcs_dist))
    lcs_min[j] = ifelse(lcs_min[j] == Inf,str_length(str_trim(search[j])),lcs_min[j])
    soundex_min[j] = min(unlist(soundex_dist))
    soundex_min[j] = ifelse(soundex_min[j] == Inf,1,soundex_min[j])
    
  }
  full_data[i,"search_left"] = search_left
  full_data$match_percent[i] = match/search_len
  full_data$search_word[i] = j
  
  full_data$hamming_match[i] = ifelse(length(hamming_min) == length(which(is.na(hamming_min))),0,(mean(hamming_min,na.rm=T)))
  full_data$lv_match[i] = ifelse(length(lv_min) == length(which(is.na(lv_min))),0,(mean(lv_min,na.rm=T)))
  full_data$osa_match[i] = ifelse(length(osa_min) == length(which(is.na(osa_min))),0,(mean(osa_min,na.rm=T)))
  full_data$dl_match[i] = ifelse(length(dl_min) == length(which(is.na(dl_min))),0,(mean(dl_min,na.rm=T)))
  full_data$lcs_match[i] = ifelse(length(lcs_min) == length(which(is.na(lcs_min))),0,(mean(lcs_min,na.rm=T)))
  full_data$soundex_match[i] = ifelse(length(soundex_min) == length(which(is.na(soundex_min))),0,(mean(soundex_min,na.rm=T)))
  
  full_data$hamming_ratio[i] = ifelse(length(hamming_min) == length(which(is.na(hamming_min))),0,sum(hamming_min,na.rm=T))/length_search_tot
  full_data$lv_ratio[i] = ifelse(length(lv_min) == length(which(is.na(lv_min))),0,sum(lv_min,na.rm=T))/length_search_tot
  full_data$osa_ratio[i] = ifelse(length(osa_min) == length(which(is.na(osa_min))),0,sum(osa_min,na.rm=T))/length_search_tot
  full_data$dl_ratio[i] = ifelse(length(dl_min) == length(which(is.na(dl_min))),0,sum(dl_min,na.rm=T))/length_search_tot
  full_data$lcs_ratio[i] = ifelse(length(lcs_min) == length(which(is.na(lcs_min))),0,sum(lcs_min,na.rm=T))/length_search_tot
  ##### Use only the word count for soundex as the value is only 0/1 depending upon the sound #############################
  full_data$soundex_ratio[i] = ifelse(length(soundex_min) == length(which(is.na(soundex_min))),0,sum(soundex_min,na.rm=T))/search_len
  
  if(length_search_tot == 0){  
    full_data$hamming_ratio[i] = 99
    full_data$lv_ratio[i] = 99
    full_data$osa_ratio[i] = 99
    full_data$dl_ratio[i] = 99
    full_data$lcs_ratio[i] = 99
    full_data$soundex_ratio[i] = 1 
    
    full_data$hamming_match[i] = 99
    full_data$lv_match[i] = 99
    full_data$osa_match[i] = 99
    full_data$dl_match[i] = 99
    full_data$lcs_match[i] = 99
    full_data$soundex_match[i] = 1 
  }
  
  if(match >= 2)
  {
    new = lag(temp$index) * (-1)
    temp1 = cbind(temp,new)
    temp1 = temp1[,-1] 
    full_data[i,"dist_act_search"] = sum(rowSums(temp1[,c("index","new")],na.rm = F),na.rm = T)
    temp = temp %>% arrange(index)
    new = lag(temp$index) * (-1)
    temp1 = cbind(temp,new)
    temp1 = temp1[,-1]
    full_data[i,"string_dist"] = (abs(sum(rowSums(temp1[,c("index","new")],na.rm = F),na.rm = T))-match+1)/match
    
    temp = temp %>% arrange(len_str)
    new = lag(temp$len_str) * (-1)
    temp1 = cbind(temp,new)
    temp1 = temp1[,-1]
    full_data[i,"string_dist_bit"] = abs(abs(sum(rowSums(temp1[,c("len_str","new")],na.rm = F),na.rm = T)) - sum(temp1[2:nrow(temp1),"len_char"],na.rm = T))/match
  } else {
    if(search_len >= 2){
    
      full_data[i,"string_dist"] = search_len
      full_data[i,"string_dist_bit"] = length_search_tot
      full_data[i,"dist_act_search"] = search_len
      
    }else{
      full_data[i,"string_dist"] = 0
      full_data[i,"string_dist_bit"] = 0
      full_data[i,"dist_act_search"] = 0
    }
    
  }
  
  rm(temp)
  rm(temp1)
  print(paste((i/nrow(full_data))*100, "% percent complete",sep="",collapse=NULL))
}

full_data$match_percent[which(is.na(full_data$match_percent))] = 0

full_data = full_data %>% left_join(attr_data_1, by = "product_uid")

full_data = full_data %>% mutate(len_search_left = 
                                   str_length(sapply(search_left,
                                                     function(x){return(gsub("[[:space:]]","",x))})))

full_data$string_dist[which(is.na(full_data$string_dist))] = 0

full_data$string_dist_bit[which(is.na(full_data$string_dist_bit))] = 0

full_data$dist_act_search[which(is.na(full_data$dist_act_search))] = 0

for(i in 1:nrow(full_data)){
  search = str_split(full_data$search_left[i]," ")
  search = str_trim(setdiff(search[[1]],""))
  desc_stem = str_split(full_data$desc_stem[i]," ")
  desc_stem = str_trim(setdiff(desc_stem[[1]],""))
  search_len = length(search)
  length_search_tot = sum(str_length(search))
  match = 0
  hamming_min = vector()
  lv_min = vector()
  osa_min = vector()
  dl_min = vector()
  lcs_min = vector()
  soundex_min = vector()
  temp=as.data.frame(matrix(rep(0,search_len),search_len,1))
  string_length = 0
  for (j in 1:search_len){
    if(length(search[j] > 0)){
      if(search[j] %in% desc_stem)
      {
        match = match + 1
        
        string_length = string_length + str_length(str_trim(search[j]))
      }
    }
    
    hamming_dist = lapply(desc_stem,function(x){stringdist(search[j],x,method="hamming")})
    lv_dist = lapply(desc_stem,function(x){stringdist(search[j],x,method="lv")})
    osa_dist = lapply(desc_stem,function(x){stringdist(search[j],x,method="osa")})
    dl_dist = lapply(desc_stem,function(x){stringdist(search[j],x,method="dl")})
    lcs_dist = lapply(desc_stem,function(x){stringdist(search[j],x,method="lcs")})
    soundex_dist = lapply(desc_stem,function(x){stringdist(search[j],x,method="soundex")})
    
    hamming_min[j] = min(unlist(hamming_dist))
    hamming_min[j] = ifelse(hamming_min[j] == Inf,str_length(str_trim(search[j])),hamming_min[j])
    lv_min[j] = min(unlist(lv_dist))
    lv_min[j] = ifelse(lv_min[j] == Inf,str_length(str_trim(search[j])),lv_min[j])
    osa_min[j] = min(unlist(osa_dist))
    osa_min[j] = ifelse(osa_min[j] == Inf,str_length(str_trim(search[j])),osa_min[j])
    dl_min[j] = min(unlist(dl_dist))
    dl_min[j] = ifelse(dl_min[j] == Inf,str_length(str_trim(search[j])),dl_min[j])
    lcs_min[j] = min(unlist(lcs_dist))
    lcs_min[j] = ifelse(lcs_min[j] == Inf,str_length(str_trim(search[j])),lcs_min[j])
    soundex_min[j] = min(unlist(soundex_dist))
    soundex_min[j] = ifelse(soundex_min[j] == Inf,1,soundex_min[j])
  }
  
  full_data$match_percent_attr[i] = match/search_len
  
  full_data$hamming_match_attr[i] = ifelse(length(hamming_min) == length(which(is.na(hamming_min))),0,(mean(hamming_min,na.rm=T)))
  full_data$lv_match_attr[i] = ifelse(length(lv_min) == length(which(is.na(lv_min))),0,(mean(lv_min,na.rm=T)))
  
  full_data$osa_match_attr[i] = ifelse(length(osa_min) == length(which(is.na(osa_min))),0,(mean(osa_min,na.rm=T)))
  
  full_data$dl_match_attr[i] = ifelse(length(dl_min) == length(which(is.na(dl_min))),0,(mean(dl_min,na.rm=T)))
  
  full_data$lcs_match_attr[i] = ifelse(length(lcs_min) == length(which(is.na(lcs_min))),0,(mean(lcs_min,na.rm=T)))
  
  full_data$soundex_match_attr[i] = ifelse(length(soundex_min) == length(which(is.na(soundex_min))),0,(mean(soundex_min,na.rm=T)))
  
  
  full_data$hamming_ratio_attr[i] = ifelse(length(hamming_min) == length(which(is.na(hamming_min))),0,sum(hamming_min,na.rm=T))/length_search_tot
  full_data$lv_ratio_attr[i] = ifelse(length(lv_min) == length(which(is.na(lv_min))),0,sum(lv_min,na.rm=T))/length_search_tot
  full_data$osa_ratio_attr[i] = ifelse(length(osa_min) == length(which(is.na(osa_min))),0,sum(osa_min,na.rm=T))/length_search_tot
  full_data$dl_ratio_attr[i] = ifelse(length(dl_min) == length(which(is.na(dl_min))),0,sum(dl_min,na.rm=T))/length_search_tot
  full_data$lcs_ratio_attr[i] = ifelse(length(lcs_min) == length(which(is.na(lcs_min))),0,sum(lcs_min,na.rm=T))/length_search_tot
  full_data$soundex_ratio_attr[i] = ifelse(length(soundex_min) == length(which(is.na(soundex_min))),0,sum(soundex_min,na.rm=T))/search_len
  
  if(is.infinite(full_data$match_percent_attr[i])){
    full_data$match_percent_attr[i] = 1
    
    full_data$hamming_match_attr[i] = 0
    
    full_data$lv_match_attr[i] = 0
    
    full_data$osa_match_attr[i] = 0
    
    full_data$dl_match_attr[i] = 0
    
    full_data$lcs_match_attr[i] = 0
    
    full_data$soundex_match_attr[i] = 0
    
    
    full_data$hamming_ratio_attr[i] = 0
    full_data$lv_ratio_attr[i] = 0
    full_data$osa_ratio_attr[i] = 0
    full_data$dl_ratio_attr[i] = 0
    full_data$lcs_ratio_attr[i] = 0
    full_data$soundex_ratio_attr[i] = 0    
    
  }
  print(paste((i/nrow(full_data))*100, "% percent complete",sep="",collapse=NULL))
  
}

for(i in which(is.na(full_data$match_percent_attr))){
  if(full_data$match_percent[i] == 1){
    full_data$match_percent_attr[i] = 1
    full_data$hamming_match_attr[i] = 0
    full_data$lv_match_attr[i] = 0
    full_data$osa_match_attr[i] = 0
    
    full_data$lcs_match_attr[i] = 0
    full_data$soundex_match_attr[i] = 0
    full_data$hamming_ratio_attr[i] = 0  
    full_data$lv_ratio_attr[i] = 0
    full_data$osa_ratio_attr[i] = 0
    full_data$dl_ratio_attr[i] = 0
    full_data$lcs_ratio_attr[i] = 0
    full_data$soundex_ratio_attr[i] = 0
  } 
}

full_data = full_data %>% mutate(ind_string_dist_NA = ifelse(is.na(string_dist),1,0))

full_data = full_data %>% mutate(ind_string_dist_bit_NA = ifelse(is.na(string_dist_bit),1,0))

full_data = full_data %>% mutate(ratio_dist = ifelse(string_dist_bit == 0,0,string_dist/string_dist_bit))

full_data$ratio_dist = ifelse(is.na(full_data$ratio_dist),0,full_data$ratio_dist)

full_data$ind_ratio_dist_NA = ifelse(is.na(full_data$ratio_dist),1,0)

full_data = full_data %>% mutate(search_term_stem_rem = 
                                   sapply(search_term_stem,
                                          function(x){return(gsub("[[:space:]]","",x))}))
brand_name = brand_name[,c(1,5)]

full_data = full_data %>% left_join(brand_name, by = "product_uid")

full_data$value_stem_rem[which(is.na(full_data$value_stem_rem))] = ""

for(i in 1:nrow(full_data)){
  if((full_data$value_stem_rem[i] != "")){
    if(!is.na(str_match(full_data$search_term_stem_rem[i],full_data$value_stem_rem[i]))){
      full_data$ind_brand_search[i] = 1
      full_data$ind_no_brand[i] = 0
    } else{
      full_data$ind_brand_search[i] = 0
      full_data$ind_no_brand[i] = 0
    }  
  }else{
    full_data$ind_brand_search[i] = 0
    full_data$ind_no_brand[i] = 1
  }
}  

full_data$ind_brand_term = as.numeric(full_data$ind_brand_search) + as.numeric(full_data$ind_no_brand)

full_data$ratio_search = full_data$len_search/full_data$title_len

full_data = full_data %>% mutate(ratio_match = ifelse(match_percent_attr > 0, match_percent/match_percent_attr,0))

saveRDS(full_data, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data.rds")

####################################################################################
################################## Test model ######################################
####################################################################################

test_data = read.csv("/mapr/projects/GLRM_test/HomeDepot/test.csv",nrow=read_rows)

test_data = test_data %>% mutate(product_title = gsub("  "," ",product_title))

test_data = test_data %>% mutate(search_term = gsub("  "," ",search_term))

test_data$product_title = sapply(test_data$product_title,str_to_lower)
test_data$search_term = sapply(test_data$search_term,str_to_lower)

test_data = test_data %>% mutate(product_title = gsub("sq ft","sqft",product_title),
                                 product_title = gsub("sq feet","sqft",product_title),
                                 product_title = gsub("square feet","sqft",product_title),
                                 product_title = gsub("sq.ft.","sqft",product_title),
                                 product_title = gsub("gallon","gal.",product_title),
                                 product_title = gsub("ounce","oz",product_title),
                                 product_title = gsub("ounces","oz",product_title),
                                 product_title = gsub("centimeter","cm",product_title),
                                 product_title = gsub("milimeter","mm",product_title),
                                 product_title = gsub("pound","lb.",product_title),
                                 product_title = gsub("feet","ft.",product_title),
                                 product_title = gsub("inches","in.",product_title),
                                 product_title = gsub("inch","in.",product_title),
                                 product_title = gsub("cellphone","cell phone",product_title),
                                 product_title = gsub("a/c","air conditioner",product_title),
                                 product_title = gsub("otr","over the range",product_title))

test_data = test_data %>% mutate(search_term = gsub("sq ft","sqft",search_term),
                                 search_term = gsub("sq feet","sqft",search_term),
                                 search_term = gsub("square feet","sqft",search_term),
                                 search_term = gsub("sq.ft.","sqft",search_term),
                                 search_term = gsub("gallon","gal.",search_term),
                                 search_term = gsub("ounce","oz",search_term),
                                 search_term = gsub("ounces","oz",search_term),
                                 search_term = gsub("cyprees","cypress",search_term),
                                 search_term = gsub("centimeter","cm",search_term),
                                 search_term = gsub("silestone","silestone",search_term),
                                 search_term = gsub("conditionerunit","conditioner unit",search_term),
                                 search_term = gsub("insullation","insulation",search_term),
                                 search_term = gsub("spliterair","spliter air",search_term),
                                 search_term = gsub("air condit ","air conditioner",search_term),
                                 search_term = gsub("window air condit","window air conditioner",search_term),
                                 search_term = gsub("molding","moulding",search_term),
                                 search_term = gsub("milimeter","mm",search_term),
                                 search_term = gsub("pound","lb.",search_term),
                                 search_term = gsub("windowair","window air",search_term),
                                 search_term = gsub("mobilehome","mobile home",search_term),
                                 search_term = gsub("kitchaid","kitchenaid",search_term),
                                 search_term = gsub("wallmount","wall mount",search_term),
                                 search_term = gsub("daltiles","daltile",search_term),
                                 search_term = gsub("feet","ft.",search_term),
                                 search_term = gsub("inches","in.",search_term),
                                 search_term = gsub("inch","in.",search_term),
                                 search_term = gsub("cellphone","cell phone",search_term),
                                 search_term = gsub("otr","over the range",search_term),
                                 search_term = gsub("toliet","toilet",search_term),
                                 search_term = gsub("a/c","air conditioner",search_term),
                                 search_term = gsub("whirpool","whirlpool",search_term),
                                 search_term = gsub("chesapeke","chesapeake",search_term),
                                 search_term = gsub("masonary","masonry",search_term),
                                 search_term = gsub("hoinda","honda",search_term),
                                 search_term = gsub("jeffery","jeffrey",search_term),
                                 search_term = gsub("alure","allure",search_term),
                                 search_term = gsub("vinal","vinyl",search_term),
                                 search_term = gsub("ampere","amp",search_term),
                                 search_term = gsub("vynal","vinyl",search_term),
                                 search_term = gsub("buikids","buy kids",search_term),
                                 search_term = gsub("heath zenity","heath-zenith",search_term),
                                 search_term = gsub("heith-zenith","heath-zenith",search_term),
                                 search_term = gsub("sikalatexr","sikalatex",search_term),
                                 search_term = gsub("robutussin","robitussin",search_term),
                                 search_term = gsub("airconditioner","air conditioner",search_term),
                                 search_term = gsub("frigadaire","frigidaire",search_term),
                                 search_term = gsub("versabon","versabond",search_term),
                                 search_term = gsub("lihonia","lithonia",search_term),
                                 search_term = gsub("preature treated","pressure treated",search_term),
                                 search_term = gsub("lasron","larson",search_term),
                                 search_term = gsub("maytab","maytag",search_term),
                                 search_term = gsub("shelfs","shelves",search_term),
                                 search_term = gsub("pivot angle","pivot angle",search_term),
                                 search_term = gsub("telemechanic square","telemecanique square",search_term),
                                 search_term = gsub("whirlpoolga","milwaukee",search_term),
                                 search_term = gsub("miwaukee","whirlpool ga",search_term),
                                 search_term = gsub(" ac ","air conditioner",search_term),
                                 search_term = gsub("whirlpoolstainless","whirlpool stainless",search_term),
                                 search_term = gsub("pedistal","pedestal",search_term),
                                 search_term = gsub("condtioner","conditioner",search_term),
                                 search_term = gsub("pannel","panel",search_term),
                                 search_term = gsub("greecianmarble","greecian marble",search_term),
                                 search_term = gsub("high boy","highboy",search_term),
                                 search_term = gsub("plexigla","plexi gla",search_term),
                                 search_term = gsub("snowbl","snow bl",search_term),
                                 search_term = gsub("skill","skil",search_term),
                                 search_term = gsub(" one "," 1 ",search_term),
                                 search_term = gsub(" two "," 2 ",search_term),
                                 search_term = gsub(" three "," 3 ",search_term),
                                 search_term = gsub(" four "," 4 ",search_term),
                                 search_term = gsub(" five "," 5 ",search_term),
                                 search_term = gsub(" six "," 6 ",search_term),
                                 search_term = gsub(" seven "," 7 ",search_term),
                                 search_term = gsub(" eight "," 8 ",search_term),
                                 search_term = gsub(" nine "," 9 ",search_term),
                                 search_term = gsub(" zero "," 0 ",search_term),
                                 search_term = gsub("vynil","vinyl",search_term),
                                 search_term = gsub("baracket","bracket",search_term),
                                 search_term = gsub("excaust","exhaust",search_term),
                                 search_term = gsub("phlne","phone",search_term),
                                 search_term = gsub("milwakee","milwaukee",search_term),
                                 search_term = gsub("rustoleum","rust-oleum",search_term),
                                 search_term = gsub("vinil","vinyl",search_term),
                                 search_term = gsub("flors","floors",search_term),
                                 search_term = gsub("steqamers","steamers",search_term),
                                 search_term = gsub("silocoln","silicone",search_term),
                                 search_term = gsub("rustollum","rust-oleum",search_term),
                                 search_term = gsub("lawnmower","lawn mower",search_term),
                                 search_term = gsub("aire acondicionado","air conditioner",search_term),
                                 search_term = gsub("enrty","entry",search_term),
                                 search_term = gsub("closetmade","closetmaid",search_term),
                                 search_term = gsub("garge","garage",search_term),
                                 search_term = gsub("compressro","compressor",search_term),
                                 search_term = gsub("kohlor","kohler",search_term),
                                 search_term = gsub("scurity","security",search_term),
                                 search_term = gsub("rigid ","ridgid",search_term),
                                 search_term = gsub("koehler","kohler",search_term),
                                 search_term = gsub("spectrazide","spectracide",search_term),
                                 search_term = gsub("hamptom","hampton",search_term),
                                 search_term = gsub("phillips","philips",search_term),
                                 search_term = gsub("marrazi","marazzi",search_term),
                                 search_term = gsub("clacier","glacier",search_term),
                                 search_term = gsub("glazer","glacier",search_term),
                                 search_term = gsub("glaciar","glacier",search_term),
                                 search_term = gsub("bath rom","bathroom",search_term),
                                 search_term = gsub("edsel","edsal",search_term),
                                 search_term = gsub("murry","murray",search_term),
                                 search_term = gsub(" sinl "," sink ",search_term),
                                 search_term = gsub("deck over","deckover",search_term),
                                 search_term = gsub("sprkinler","sprinkler",search_term),
                                 search_term = gsub("ourdoor","outdoor",search_term),
                                 search_term = gsub("valvue","valve",search_term),
                                 search_term = gsub("outdoorlounge","outdoor lounge",search_term),
                                 search_term = gsub("outdoorfurniture","outdoor furniture",search_term),
                                 search_term = gsub("tolet","toilet",search_term),
                                 search_term = gsub("basemetnt","basement",search_term),
                                 search_term = gsub("heaterconditioner","heater conditioner",search_term),
                                 search_term = gsub("steele","steel",search_term),
                                 search_term = gsub(" stell "," steel ",search_term),
                                 search_term = gsub(" fece "," fence ",search_term),
                                 search_term = gsub("tiolet","toilet",search_term),
                                 search_term = gsub("gas wayer","gas water",search_term),
                                 search_term = gsub("berh","behr",search_term),
                                 search_term = gsub("thower","thrower",search_term),
                                 search_term = gsub("base board","baseboard",search_term),
                                 search_term = gsub("boundle","bundle",search_term),
                                 search_term = gsub(" sping "," spring ",search_term),
                                 search_term = gsub(" desalt "," dewalt ",search_term),
                                 search_term = gsub("repir","repair",search_term),
                                 search_term = gsub("condtioners","conditioners",search_term),
                                 search_term = gsub("pannel","panel",search_term),
                                 search_term = gsub(" sruds "," studs ",search_term),
                                 search_term = gsub("florescent","fluorescent",search_term),
                                 search_term = gsub("flourescent","fluorescent",search_term),
                                 search_term = gsub("electic","electric",search_term),
                                 search_term = gsub("greecianmarble","grecian marble",search_term),
                                 search_term = gsub("montagnia contina floor tile","montagna cortina floor tile",search_term),
                                 search_term = gsub("porcelin","porcelain",search_term),
                                 search_term = gsub("miricale","miracle",search_term),
                                 search_term = gsub("windos","windows",search_term),
                                 search_term = gsub("vaccum","vacuum",search_term),
                                 search_term = gsub("heather","heater",search_term),
                                 search_term = gsub("bagged cinder mulch","bagged cedar mulch",search_term),
                                 search_term = gsub("hindges","hinges",search_term),
                                 search_term = gsub("hieght","height",search_term),
                                 search_term = gsub("celling","ceiling",search_term),
                                 search_term = gsub("procelian","porcelain",search_term),
                                 search_term = gsub("dewalr","dewalt",search_term),
                                 search_term = gsub("cieling","ceiling",search_term),
                                 search_term = gsub("roybi","ryobi",search_term),
                                 search_term = gsub("cordlessrotary hammerss","cordless rotary hammers",search_term),
                                 search_term = gsub("bathro ","bathroom",search_term),
                                 search_term = gsub("barcello","bercello",search_term),
                                 search_term = gsub("aluminun","aluminum",search_term),
                                 search_term = gsub("lightening","lighting",search_term),
                                 search_term = gsub("fixtues","fixtures",search_term),
                                 search_term = gsub("swantone","swanstone",search_term),
                                 search_term = gsub("everblit","everbilt",search_term),
                                 search_term = gsub("alumanam","aluminum",search_term),
                                 search_term = gsub("fencde","fence",search_term),
                                 search_term = gsub("untility","utility",search_term),
                                 search_term = gsub("paito","patio",search_term),
                                 search_term = gsub("mowre","mower",search_term),
                                 search_term = gsub("ridding","riding",search_term),
                                 search_term = gsub("medicn","medicine",search_term),
                                 search_term = gsub("qtr","quarter",search_term),
                                 search_term = gsub("infered","infrared",search_term),
                                 search_term = gsub("blk pipe","black pipe",search_term),
                                 search_term = gsub("craft an lawn mower","craftsman lawn mower",search_term),
                                 search_term = gsub("tpoilet","toilet",search_term),
                                 search_term = gsub("dhower","shower",search_term),
                                 search_term = gsub("hindged","hinged",search_term),
                                 search_term = gsub("canopie","canopy",search_term),
                                 search_term = gsub("chanpayne","champagne",search_term),
                                 search_term = gsub("ceramick","ceramic",search_term),
                                 search_term = gsub("celing","ceiling",search_term),
                                 search_term = gsub("n9ickel","nickel",search_term),
                                 search_term = gsub("extention","extension",search_term),
                                 search_term = gsub(" gaint "," giant ",search_term),
                                 search_term = gsub("chainlink","chain link",search_term),
                                 search_term = gsub("linoliuml","linoleum",search_term),
                                 search_term = gsub("hagchet","hatchet",search_term),
                                 search_term = gsub("microwarve","microwave",search_term),
                                 search_term = gsub("backsplach","backsplash",search_term),
                                 search_term = gsub("rayoby","ryobi",search_term),
                                 search_term = gsub("naturlas","naturals",search_term),
                                 search_term = gsub("dog fence batt","dog fence battery",search_term),
                                 search_term = gsub("onda pressure washer","honda pressure washer",search_term),
                                 search_term = gsub("fridgdare","frigidaire",search_term),
                                 search_term = gsub("pain windows","pane windows",search_term),
                                 search_term = gsub("robi battery","ryobi battery",search_term),
                                 search_term = gsub("weewacker edger","weed wacker edger",search_term),
                                 search_term = gsub("forimca","formica",search_term),
                                 search_term = gsub("repir cuplings","repair couplings",search_term),
                                 search_term = gsub("hoursepower","horsepower",search_term),
                                 search_term = gsub("gauze","gauge",search_term),
                                 search_term = gsub("didger","dodger",search_term),
                                 search_term = gsub("accordian","accordion",search_term),
                                 search_term = gsub("4x6treaded wood","4x6 treated wood",search_term),
                                 search_term = gsub("preature treated lumber","pressure treated lumber",search_term),
                                 search_term = gsub("closetmade wood","closetmaid wood",search_term),
                                 search_term = gsub("cleanerm mop","cleaner mop",search_term),
                                 search_term = gsub(" shads "," shades ",search_term),
                                 search_term = gsub("contracor","contractor",search_term),
                                 search_term = gsub("rust oleum","rust-oleum",search_term),
                                 search_term = gsub("selves","shelves",search_term),
                                 search_term = gsub("hecurles","hercules",search_term),
                                 search_term = gsub("anderson","andersen",search_term),
                                 search_term = gsub("lasron","larson",search_term),
                                 search_term = gsub("refridgerator","refrigerator",search_term),
                                 search_term = gsub("structue","structure",search_term),
                                 search_term = gsub("cahnnel","channel",search_term),
                                 search_term = gsub("conner","corner",search_term),
                                 search_term = gsub("planel glue","panel glue",search_term),
                                 search_term = gsub("ventenatural","vented natural",search_term),
                                 search_term = gsub("swivrl","swivel",search_term),
                                 search_term = gsub("galvinized","galvanized",search_term),
                                 search_term = gsub("fictures","fixtures",search_term),
                                 search_term = gsub("plasticbathroom","plastic bathroom",search_term),
                                 search_term = gsub("jeffery","jeffrey",search_term),
                                 search_term = gsub("fireplacewater","fireplace water",search_term),
                                 search_term = gsub("waxhers","washers",search_term),
                                 search_term = gsub("genecrac","generac",search_term),
                                 search_term = gsub("acrtlic","acrylic",search_term),
                                 search_term = gsub("padio","patio",search_term),
                                 search_term = gsub("cyculer","circular",search_term),
                                 search_term = gsub("micro wave","microwave",search_term),
                                 search_term = gsub("utility traiter","utility trailer",search_term),
                                 search_term = gsub("consertrated","concentrate",search_term),
                                 search_term = gsub("vynik","vinyl",search_term),
                                 search_term = gsub("riobi","ryobi",search_term),
                                 search_term = gsub("recepicating","reciprocating",search_term),
                                 search_term = gsub("step latter","step ladder",search_term),
                                 search_term = gsub("meguire","meguiar",search_term),
                                 search_term = gsub("refrdgerators","refrigerators",search_term),
                                 search_term = gsub("colbolt","cobalt",search_term),
                                 search_term = gsub("falcet","faucet",search_term),
                                 search_term = gsub("mantle","mantel",search_term),
                                 search_term = gsub("rachet scret drivers","ratchet screwdrivers",search_term),
                                 search_term = gsub("ruotor table","router table",search_term),
                                 search_term = gsub("decoritive","decorative",search_term),
                                 search_term = gsub("threashold","threshold",search_term),
                                 search_term = gsub("horizantel","horizontal",search_term),
                                 search_term = gsub("attatchmens","attachments",search_term),
                                 search_term = gsub("wrachet","ratchet",search_term),
                                 search_term = gsub("waterroo","waterproof",search_term),
                                 search_term = gsub("ftawning","ft awning",search_term),
                                 search_term = gsub("florecent","fluorescent",search_term),
                                 search_term = gsub("hamptom bay cusion","hampton bay cushion",search_term),
                                 search_term = gsub("marrazi","marazzi",search_term),
                                 search_term = gsub("untique","antique",search_term),
                                 search_term = gsub("dinning","dining",search_term),
                                 search_term = gsub("colpay","clopay",search_term),
                                 search_term = gsub("cicular","circular",search_term),
                                 search_term = gsub("snowerblower","snowblower",search_term),
                                 search_term = gsub("birkmann","brinkmann",search_term),
                                 search_term = gsub("sofet","soffit",search_term),
                                 search_term = gsub("phillips","philips",search_term),
                                 search_term = gsub("residental","residential",search_term),
                                 search_term = gsub("luever","leuver",search_term),
                                 search_term = gsub("hamiltton","hamilton",search_term),
                                 search_term = gsub("andersor","andersen",search_term),
                                 search_term = gsub("florisant","fluorescent",search_term),
                                 search_term = gsub("pendent","pendant",search_term),
                                 search_term = gsub("kennal kit","kennel kit",search_term),
                                 search_term = gsub("chesapeke","chesapeake",search_term),
                                 search_term = gsub("raidos","radios",search_term),
                                 search_term = gsub("qucikie","quickie",search_term),
                                 search_term = gsub("napolian","napoleon",search_term),
                                 search_term = gsub("riverera","riviera",search_term),
                                 search_term = gsub("accesories","accessories",search_term),
                                 search_term = gsub("slyvanna","sylvania",search_term),
                                 search_term = gsub("beveragecooler","beverage cooler",search_term),
                                 search_term = gsub("ureka","eureka",search_term),
                                 search_term = gsub("unbralla","umbrella",search_term),
                                 search_term = gsub("asathbula","ashtabula",search_term),
                                 search_term = gsub("elerical","electrical",search_term),
                                 search_term = gsub("mansonry","radios",search_term),
                                 search_term = gsub("industreial","industrial",search_term),
                                 search_term = gsub("martha steward","martha stewart",search_term),
                                 search_term = gsub("canopie","canopy",search_term),
                                 search_term = gsub("flourescent","fluorescent",search_term),
                                 search_term = gsub("daucet","faucet",search_term),
                                 search_term = gsub("millwaukee","milwaukee",search_term),
                                 search_term = gsub("boscj bit","bosch bit",search_term),
                                 search_term = gsub("barbque","barbecue",search_term),
                                 search_term = gsub("brinkman","brinkmann",search_term),
                                 search_term = gsub("malbu","malibu",search_term),
                                 search_term = gsub("stone venner sequia","stone veneer sequoia",search_term),
                                 search_term = gsub("chrisymas","christmas",search_term),
                                 search_term = gsub("refigrator","refrigerator",search_term),
                                 search_term = gsub("cabinent","cabinet",search_term),
                                 search_term = gsub("hasmmock bed","hammock bed",search_term),
                                 search_term = gsub("idylus","idylis",search_term),
                                 search_term = gsub("parquee","parquet",search_term),
                                 search_term = gsub("dermal","dremel",search_term),
                                 search_term = gsub("gardinias","gardenias",search_term),
                                 search_term = gsub("942196brinkmann","942196 brinkmann",search_term),
                                 search_term = gsub("bouganvilla","bougainvillea",search_term),
                                 search_term = gsub("nozzel","nozzle",search_term),
                                 search_term = gsub("acrilic","acrylic",search_term),
                                 search_term = gsub("gibraltor","gibraltar",search_term),
                                 search_term = gsub("balist","ballast",search_term),
                                 search_term = gsub("john deer ","john deere ",search_term),
                                 search_term = gsub("walloven","wall oven",search_term),
                                 search_term = gsub("frigidare","frigidaire",search_term),
                                 search_term = gsub("asburn","ashburn",search_term),
                                 search_term = gsub("reheem","rheem",search_term),
                                 search_term = gsub("dewaqlt","dewalt",search_term),
                                 search_term = gsub("clyvus","clivus",search_term),
                                 search_term = gsub("cadelabra","candelabra",search_term),
                                 search_term = gsub("elvies ","elvis ",search_term),
                                 search_term = gsub("gentec ","gentex ",search_term),
                                 search_term = gsub("vogoro flower","vigoro flower",search_term),
                                 search_term = gsub("caroilne","caroline",search_term),
                                 search_term = gsub("martha suart","martha stewart",search_term),
                                 search_term = gsub("swiming","swimming",search_term),
                                 search_term = gsub("klien","klein",search_term),
                                 search_term = gsub("huskie","husky",search_term),
                                 search_term = gsub("levinton","leviton",search_term),
                                 search_term = gsub("milwaulkee","milwaukee",search_term),
                                 search_term = gsub("shelter logic","shelterlogic",search_term),
                                 search_term = gsub("fisher and penkel","fisher and paykel",search_term),
                                 search_term = gsub("simpon strong","simpson strong",search_term),
                                 search_term = gsub("tribecia","tribeca",search_term),
                                 search_term = gsub("conditionerriding","conditioner riding",search_term),
                                 search_term = gsub("veraluz","varaluz",search_term),
                                 search_term = gsub("kholer","kohler",search_term),
                                 search_term = gsub("shluter","schluter",search_term),
                                 search_term = gsub("greak","greek",search_term),
                                 search_term = gsub("gekko gauges","gecko gauges",search_term),
                                 search_term = gsub("pnematic","pneumatic",search_term),
                                 search_term = gsub("pegro xp","pergo xp",search_term),
                                 search_term = gsub("fiskers","fiskars",search_term),
                                 search_term = gsub("jimmyproof","jimmy proof",search_term),
                                 search_term = gsub("monococcon","monococcion",search_term),
                                 search_term = gsub("savavieh soho","safavieh soho",search_term),
                                 search_term = gsub("minala rope","manila rope",search_term),
                                 search_term = gsub("kraylon","krylon",search_term),
                                 search_term = gsub("mystick","mystik",search_term),
                                 search_term = gsub("kliz primers","kilz primers",search_term),
                                 search_term = gsub("rust oleam","rust-oleum",search_term),
                                 search_term = gsub("playststion","playstation",search_term),
                                 search_term = gsub("perferated","perforated",search_term),
                                 search_term = gsub("hamptonbay","hampton bay",search_term),
                                 search_term = gsub("chlorox","clorox",search_term),
                                 search_term = gsub("divonshire","devonshire",search_term),
                                 search_term = gsub("matage double","maytag double",search_term),
                                 search_term = gsub("tensil ties","tinsel ties",search_term),
                                 search_term = gsub("bonda body filler","bondo body filler",search_term),
                                 search_term = gsub("bousch","bosch",search_term),
                                 search_term = gsub("apriaire","aprilaire",search_term),
                                 search_term = gsub("hoovwe cordless","hoover cordless",search_term),
                                 search_term = gsub("perenial","perennial",search_term),
                                 search_term = gsub("baraar emy","bazaar emy",search_term),
                                 search_term = gsub("weelbarrow","wheelbarrow",search_term),
                                 search_term = gsub("zinser stain","zinsser stain",search_term),
                                 search_term = gsub("centipe grass seed","centipede grass seed",search_term),
                                 search_term = gsub("azelas","azaleas",search_term),
                                 search_term = gsub("suny citrus","sunny citrus",search_term),
                                 search_term = gsub("tuscon patio","tucson patio",search_term),
                                 search_term = gsub(" valae "," valve ",search_term),
                                 search_term = gsub("milwaukie","milwaukee",search_term),
                                 search_term = gsub(" gfi "," gfci ",search_term),
                                 search_term = gsub("emparador","emperador",search_term),
                                 search_term = gsub("moem faucet repair","moen faucet repair",search_term),
                                 search_term = gsub("acuria ","acurio ",search_term),
                                 search_term = gsub("arbourist ","arborist ",search_term),
                                 search_term = gsub("dealt portable ","dewalt portable ",search_term),
                                 search_term = gsub("taracota drain","terracotta drain",search_term),
                                 search_term = gsub("termini mosquito","terminix mosquito",search_term),
                                 search_term = gsub("bostic wood","bostik wood",search_term),
                                 search_term = gsub("surveilance","surveillance",search_term),
                                 search_term = gsub("lazer","laser",search_term),
                                 search_term = gsub("webber","weber",search_term),
                                 search_term = gsub("bbostitch","bostitch",search_term),
                                 search_term = gsub("vitarera","viatera",search_term),
                                 search_term = gsub("rostoluem","rust-oleum",search_term),
                                 search_term = gsub("gerber ","gerbera ",search_term),
                                 search_term = gsub("saw zall","sawzall",search_term),
                                 search_term = gsub("facia corner","fascia corner",search_term),
                                 search_term = gsub("ghroe shower","grohe shower",search_term),
                                 search_term = gsub("portercable","porter cable",search_term),
                                 search_term = gsub("rybi ","ryobi ",search_term),
                                 search_term = gsub("talsrar","talstar",search_term),
                                 search_term = gsub("cender block","cinder block",search_term),
                                 search_term = gsub("fridigidaire","frigidaire",search_term),
                                 search_term = gsub("maratha ","martha ",search_term),
                                 search_term = gsub("clacer bay","glacier bay",search_term),
                                 search_term = gsub("royobi","ryobi",search_term),
                                 search_term = gsub("armstroung ","armstrong ",search_term),
                                 search_term = gsub("hampden bay","hampton bay",search_term),
                                 search_term = gsub("facuet","faucet",search_term),
                                 search_term = gsub("mikita cordless","makita cordless",search_term),
                                 search_term = gsub("samsong stive","samsung stove",search_term),
                                 search_term = gsub("residentialsteel","residential steel",search_term),
                                 search_term = gsub("wirlpool","whirlpool",search_term),
                                 search_term = gsub("moehn kitchen","moen kitchen",search_term),
                                 search_term = gsub("powervent","power vent",search_term),
                                 search_term = gsub("stewartcabinet","stewart cabinet",search_term),
                                 search_term = gsub("samsungelectric","samsung electric",search_term),
                                 search_term = gsub("richman water","richmond water",search_term),
                                 search_term = gsub("oscilliat","oscillat",search_term),
                                 search_term = gsub("huskavarna","husqvarna",search_term),
                                 search_term = gsub("kholerhighland","kohler highline",search_term),
                                 search_term = gsub("huskey 18","husky 18",search_term),
                                 search_term = gsub("rustolem","rust-oleum",search_term),
                                 search_term = gsub("glazer bay","glacier bay",search_term),
                                 search_term = gsub("floorring","flooring",search_term),
                                 search_term = gsub("faucetskitchen","faucets kitchen",search_term),
                                 search_term = gsub("wiemans","weimans",search_term),
                                 search_term = gsub("hampton bat","hampton bay",search_term),
                                 search_term = gsub("diamondplate","diamond plate",search_term),
                                 search_term = gsub("boshe ","bosch ",search_term),
                                 search_term = gsub("g e micewave ","ge microwave ",search_term),
                                 search_term = gsub("golith","goliath",search_term),
                                 search_term = gsub("ralph laren","ralph lauren",search_term),
                                 search_term = gsub("n utdriver","nut driver",search_term),
                                 search_term = gsub("wesleyand","wesleyan",search_term),
                                 search_term = gsub("kaorik wine","kalorik wine",search_term),
                                 search_term = gsub("colaroo","coolaroo",search_term),
                                 search_term = gsub("rubberaid","rubbermaid",search_term),
                                 search_term = gsub("thermadore","thermador",search_term),
                                 search_term = gsub("milwalke","milwaukee",search_term),
                                 search_term = gsub("milwankee","milwaukee",search_term),
                                 search_term = gsub("almeda hickory","alameda hickory",search_term),
                                 search_term = gsub("seimens","siemens",search_term),
                                 search_term = gsub("whitesilicone","white silicone",search_term),
                                 search_term = gsub("kkohler","kohler",search_term),
                                 search_term = gsub("cerowire","cerro wire",search_term),
                                 search_term = gsub("rusteoulm","rust-oleum",search_term),
                                 search_term = gsub("quickrete","quikrete",search_term),
                                 search_term = gsub("lampost","lamp post",search_term),
                                 search_term = gsub("honolule","honolulu",search_term),
                                 search_term = gsub("vigaro","vigoro",search_term),
                                 search_term = gsub("fiet","feit",search_term),
                                 search_term = gsub("hardi lap","hardie lap",search_term),
                                 search_term = gsub("askley","ashley",search_term),
                                 search_term = gsub("maytab bravos","maytag bravos",search_term),
                                 search_term = gsub("milwoki","milwaukee",search_term),
                                 search_term = gsub("verathane","varathane",search_term),
                                 search_term = gsub("glaciar","glacier",search_term),
                                 search_term = gsub("morola tile","merola tile",search_term))

product_title_stem = tm_map(Corpus(VectorSource(test_data$product_title)),removeWords, stopwords('english'))
product_title_stem = tm_map(product_title_stem,replacePunctuation)
search_term_stem = tm_map(Corpus(VectorSource(test_data$search_term)),removeWords, stopwords('english'))
search_term_stem = tm_map(search_term_stem,replacePunctuation)
test_data$product_title_stem = as.character(tm_map(product_title_stem,stemDocument))
test_data$search_term_stem = as.character(tm_map(search_term_stem,stemDocument))

rm(product_title_stem)
rm(search_term_stem)

test_data = test_data %>% mutate(search_term_stem = gsub("  "," ",search_term_stem))

test_data = test_data %>% mutate(product_title_stem = gsub("  "," ",product_title_stem))

for(i in 1:nrow(test_data)){
  search = str_split(test_data$search_term_stem[i]," ")
  search = str_trim(setdiff(search[[1]],""))
  product_title = str_split(test_data$product_title_stem[i]," ")
  product_title = str_trim(setdiff(product_title[[1]],""))
  length_prod = str_length(product_title)
  search_len = length(search)
  length_search_tot = sum(str_length(search))
  match = 0
  temp=as.data.frame(matrix(rep(0,search_len),search_len,1))
  hamming_min = vector()
  lv_min = vector()
  osa_min = vector()
  dl_min = vector()
  lcs_min = vector()
  soundex_min = vector()
  search_left = ""
  string_length = 0
  for (j in 1:search_len){
    if(length(search[j] > 0)){
      if(search[j] %in% product_title)
      {
        match = match + 1
        search_left = paste("",search_left,sep=" ",collapse=NULL)
        string_length = string_length + str_length(str_trim(search[j]))
        if(length(which(product_title %in% search[j]))>0)
        {
          val_index = which(product_title %in% search[j])
          temp[j,"index"] = val_index
          temp[j,"len_str"] = sum(length_prod[1:val_index])
          temp[j,"len_char"] = length_prod[val_index]
        } 
      } else{
        search_left = paste(search_left,search[j],sep=" ",collapse=NULL)
      }
    }
    
    hamming_dist = lapply(product_title,function(x){stringdist(search[j],x,method="hamming")})
    lv_dist = lapply(product_title,function(x){stringdist(search[j],x,method="lv")})
    osa_dist = lapply(product_title,function(x){stringdist(search[j],x,method="osa")})
    dl_dist = lapply(product_title,function(x){stringdist(search[j],x,method="dl")})
    lcs_dist = lapply(product_title,function(x){stringdist(search[j],x,method="lcs")})
    soundex_dist = lapply(product_title,function(x){stringdist(search[j],x,method="soundex")})
    
    hamming_min[j] = min(unlist(hamming_dist))
    hamming_min[j] = ifelse(hamming_min[j] == Inf,str_length(str_trim(search[j])),hamming_min[j])
    lv_min[j] = min(unlist(lv_dist))
    lv_min[j] = ifelse(lv_min[j] == Inf,str_length(str_trim(search[j])),lv_min[j])
    osa_min[j] = min(unlist(osa_dist))
    osa_min[j] = ifelse(osa_min[j] == Inf,str_length(str_trim(search[j])),osa_min[j])
    dl_min[j] = min(unlist(dl_dist))
    dl_min[j] = ifelse(dl_min[j] == Inf,str_length(str_trim(search[j])),dl_min[j])
    lcs_min[j] = min(unlist(lcs_dist))
    lcs_min[j] = ifelse(lcs_min[j] == Inf,str_length(str_trim(search[j])),lcs_min[j])
    soundex_min[j] = min(unlist(soundex_dist))
    soundex_min[j] = ifelse(soundex_min[j] == Inf,1,soundex_min[j])
    
  }
  test_data[i,"search_left"] = search_left
  test_data$match_percent[i] = match/search_len
  test_data$search_word[i] = j
  
  test_data$hamming_match[i] = ifelse(length(hamming_min) == length(which(is.na(hamming_min))),0,(mean(hamming_min,na.rm=T)))
  
  test_data$lv_match[i] = ifelse(length(lv_min) == length(which(is.na(lv_min))),0,(mean(lv_min,na.rm=T)))
  
  test_data$osa_match[i] = ifelse(length(osa_min) == length(which(is.na(osa_min))),0,(mean(osa_min,na.rm=T)))
  
  test_data$dl_match[i] = ifelse(length(dl_min) == length(which(is.na(dl_min))),0,(mean(dl_min,na.rm=T)))
  
  test_data$lcs_match[i] = ifelse(length(lcs_min) == length(which(is.na(lcs_min))),0,(mean(lcs_min,na.rm=T)))
  
  test_data$soundex_match[i] = ifelse(length(soundex_min) == length(which(is.na(soundex_min))),0,(mean(soundex_min,na.rm=T)))
  
  
  test_data$hamming_ratio[i] = ifelse(length(hamming_min) == length(which(is.na(hamming_min))),0,sum(hamming_min,na.rm=T))/length_search_tot
  test_data$lv_ratio[i] = ifelse(length(lv_min) == length(which(is.na(lv_min))),0,sum(lv_min,na.rm=T))/length_search_tot
  test_data$osa_ratio[i] = ifelse(length(osa_min) == length(which(is.na(osa_min))),0,sum(osa_min,na.rm=T))/length_search_tot
  test_data$dl_ratio[i] = ifelse(length(dl_min) == length(which(is.na(dl_min))),0,sum(dl_min,na.rm=T))/length_search_tot
  test_data$lcs_ratio[i] = ifelse(length(lcs_min) == length(which(is.na(lcs_min))),0,sum(lcs_min,na.rm=T))/length_search_tot
  test_data$soundex_ratio[i] = ifelse(length(soundex_min) == length(which(is.na(soundex_min))),0,sum(soundex_min,na.rm=T))/search_len
  
  if(length_search_tot == 0){  
    test_data$hamming_ratio[i] = 99
    test_data$lv_ratio[i] = 99
    test_data$osa_ratio[i] = 99
    test_data$dl_ratio[i] = 99
    test_data$lcs_ratio[i] = 99
    test_data$soundex_ratio[i] = 1 
    
    test_data$hamming_match[i] = 99
    test_data$lv_match[i] = 99
    test_data$osa_match[i] = 99
    test_data$dl_match[i] = 99
    test_data$lcs_match[i] = 99
    test_data$soundex_match[i] = 1  
  }
  
  if(match >= 2)
  {
    new = lag(temp$index) * (-1)
    temp1 = cbind(temp,new)
    temp1 = temp1[,-1] 
    test_data[i,"dist_act_search"] = sum(rowSums(temp1[,c("index","new")],na.rm = F),na.rm = T)
    temp = temp %>% arrange(index)
    new = lag(temp$index) * (-1)
    temp1 = cbind(temp,new)
    temp1 = temp1[,-1]
    test_data[i,"string_dist"] = (abs(sum(rowSums(temp1[,c("index","new")],na.rm = F),na.rm = T))-match+1)/match
    
    temp = temp %>% arrange(len_str)
    new = lag(temp$len_str) * (-1)
    temp1 = cbind(temp,new)
    temp1 = temp1[,-1]
    test_data[i,"string_dist_bit"] = abs(abs(sum(rowSums(temp1[,c("len_str","new")],na.rm = F),na.rm = T)) - sum(temp1[2:nrow(temp1),"len_char"],na.rm = T))/match
  }else {
    if(search_len >= 2){
      
      full_data[i,"string_dist"] = search_len
      full_data[i,"string_dist_bit"] = length_search_tot
      full_data[i,"dist_act_search"] = search_len
      
    }else{
      full_data[i,"string_dist"] = 0
      full_data[i,"string_dist_bit"] = 0
      full_data[i,"dist_act_search"] = 0
    }
    
  }
  
  rm(temp)  
  rm(temp1)
  print(paste((i/nrow(test_data))*100, "% percent complete",sep="",collapse=NULL))
}

test_data = test_data %>% left_join(attr_data_1, by = "product_uid")

test_data = test_data %>% mutate(len_search_left = 
                                   str_length(sapply(search_left,
                                                     function(x){return(gsub("[[:space:]]","",x))})))

test_data = test_data %>% mutate(len_search = 
                                   str_length(sapply(search_term_stem,
                                                     function(x){return(gsub("[[:space:]]","",x))})))

saveRDS(attr_data_1, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/attr_data_new.rds")

rm(attr_data_1)

for(i in 1:nrow(test_data)){
  search = str_split(test_data$search_left[i]," ")
  search = str_trim(setdiff(search[[1]],""))
  desc_stem = str_split(test_data$desc_stem[i]," ")
  desc_stem = str_trim(setdiff(desc_stem[[1]],""))
  search_len = length(search)
  length_search_tot = sum(str_length(search))
  match = 0
  hamming_min = vector()
  lv_min = vector()
  osa_min = vector()
  dl_min = vector()
  lcs_min = vector()
  soundex_min = vector()
  temp=as.data.frame(matrix(rep(0,search_len),search_len,1))
  string_length = 0
  for (j in 1:search_len){
    if(length(search[j] > 0)){
      if(search[j] %in% desc_stem)
      {
        match = match + 1
        
        string_length = string_length + str_length(str_trim(search[j]))
      }
    }
    
    hamming_dist = lapply(desc_stem,function(x){stringdist(search[j],x,method="hamming")})
    lv_dist = lapply(desc_stem,function(x){stringdist(search[j],x,method="lv")})
    osa_dist = lapply(desc_stem,function(x){stringdist(search[j],x,method="osa")})
    dl_dist = lapply(desc_stem,function(x){stringdist(search[j],x,method="dl")})
    lcs_dist = lapply(desc_stem,function(x){stringdist(search[j],x,method="lcs")})
    soundex_dist = lapply(desc_stem,function(x){stringdist(search[j],x,method="soundex")})
    
    hamming_min[j] = min(unlist(hamming_dist))
    hamming_min[j] = ifelse(hamming_min[j] == Inf,str_length(str_trim(search[j])),hamming_min[j])
    lv_min[j] = min(unlist(lv_dist))
    lv_min[j] = ifelse(lv_min[j] == Inf,str_length(str_trim(search[j])),lv_min[j])
    osa_min[j] = min(unlist(osa_dist))
    osa_min[j] = ifelse(osa_min[j] == Inf,str_length(str_trim(search[j])),osa_min[j])
    dl_min[j] = min(unlist(dl_dist))
    dl_min[j] = ifelse(dl_min[j] == Inf,str_length(str_trim(search[j])),dl_min[j])
    lcs_min[j] = min(unlist(lcs_dist))
    lcs_min[j] = ifelse(lcs_min[j] == Inf,str_length(str_trim(search[j])),lcs_min[j])
    soundex_min[j] = min(unlist(soundex_dist))
    soundex_min[j] = ifelse(soundex_min[j] == Inf,1,soundex_min[j])
  }
  
  test_data$match_percent_attr[i] = match/search_len
  
  test_data$hamming_match_attr[i] = ifelse(length(hamming_min) == length(which(is.na(hamming_min))),0,(mean(hamming_min,na.rm=T)))
  test_data$lv_match_attr[i] = ifelse(length(lv_min) == length(which(is.na(lv_min))),0,(mean(lv_min,na.rm=T)))
  
  test_data$osa_match_attr[i] = ifelse(length(osa_min) == length(which(is.na(osa_min))),0,(mean(osa_min,na.rm=T)))
  
  test_data$dl_match_attr[i] = ifelse(length(dl_min) == length(which(is.na(dl_min))),0,(mean(dl_min,na.rm=T)))
  
  test_data$lcs_match_attr[i] = ifelse(length(lcs_min) == length(which(is.na(lcs_min))),0,(mean(lcs_min,na.rm=T)))
  
  test_data$soundex_match_attr[i] = ifelse(length(soundex_min) == length(which(is.na(soundex_min))),0,(mean(soundex_min,na.rm=T)))
  
  
  test_data$hamming_ratio_attr[i] = ifelse(length(hamming_min) == length(which(is.na(hamming_min))),0,sum(hamming_min,na.rm=T))/length_search_tot
  test_data$lv_ratio_attr[i] = ifelse(length(lv_min) == length(which(is.na(lv_min))),0,sum(lv_min,na.rm=T))/length_search_tot
  test_data$osa_ratio_attr[i] = ifelse(length(osa_min) == length(which(is.na(osa_min))),0,sum(osa_min,na.rm=T))/length_search_tot
  test_data$dl_ratio_attr[i] = ifelse(length(dl_min) == length(which(is.na(dl_min))),0,sum(dl_min,na.rm=T))/length_search_tot
  test_data$lcs_ratio_attr[i] = ifelse(length(lcs_min) == length(which(is.na(lcs_min))),0,sum(lcs_min,na.rm=T))/length_search_tot
  test_data$soundex_ratio_attr[i] = ifelse(length(soundex_min) == length(which(is.na(soundex_min))),0,sum(soundex_min,na.rm=T))/search_len
  
  if(is.infinite(test_data$match_percent_attr[i])){
    test_data$match_percent_attr[i] = 1
    
    test_data$hamming_match_attr[i] = 0
    
    test_data$lv_match_attr[i] = 0
    
    test_data$osa_match_attr[i] = 0
    
    test_data$dl_match_attr[i] = 0
    
    test_data$lcs_match_attr[i] = 0
    
    test_data$soundex_match_attr[i] = 0
    
    
    test_data$hamming_ratio_attr[i] = 0
    test_data$lv_ratio_attr[i] = 0
    test_data$osa_ratio_attr[i] = 0
    test_data$dl_ratio_attr[i] = 0
    test_data$lcs_ratio_attr[i] = 0
    test_data$soundex_ratio_attr[i] = 0    
    
  }
  print(paste((i/nrow(test_data))*100, "% percent complete",sep="",collapse=NULL))
  
}

for(i in which(is.na(test_data$match_percent_attr))){
  if(test_data$match_percent[i] == 1){
    test_data$match_percent_attr[i] = 1
    test_data$hamming_match_attr[i] = 0
    test_data$lv_match_attr[i] = 0
    test_data$osa_match_attr[i] = 0
    
    test_data$lcs_match_attr[i] = 0
    test_data$soundex_match_attr[i] = 0
    test_data$hamming_ratio_attr[i] = 0  
    test_data$lv_ratio_attr[i] = 0
    test_data$osa_ratio_attr[i] = 0
    test_data$dl_ratio_attr[i] = 0
    test_data$lcs_ratio_attr[i] = 0
    test_data$soundex_ratio_attr[i] = 0
  } 
}

test_data = test_data %>% mutate(ind_string_dist_NA = ifelse(is.na(string_dist),1,0))

test_data = test_data %>% mutate(ind_string_dist_bit_NA = ifelse(is.na(string_dist_bit),1,0))

test_data$match_percent[which(is.na(test_data$match_percent))] = 0

test_data = test_data %>% mutate(ratio_dist = ifelse(string_dist_bit == 0,0,string_dist/string_dist_bit))

test_data$ratio_dist = ifelse(is.na(test_data$ratio_dist),0,test_data$ratio_dist)

test_data$ind_ratio_dist_NA = ifelse(is.na(test_data$ratio_dist),1,0)

test_data$string_dist[which(is.na(test_data$string_dist))] = 0

test_data$string_dist_bit[which(is.na(test_data$string_dist_bit))] = 0

test_data$dist_act_search[which(is.na(test_data$dist_act_search))] = 0

test_data = test_data %>% left_join(brand_name, by = "product_uid")

saveRDS(brand_name, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/brand_name.rds")

rm(brand_name)

test_data = test_data %>% mutate(search_term_stem_rem = 
                                   sapply(search_term_stem,
                                          function(x){return(gsub("[[:space:]]","",x))}))

test_data$value_stem_rem[which(is.na(test_data$value_stem_rem))] = ""

for(i in 1:nrow(test_data)){
  if((test_data$value_stem_rem[i] != "")){
    if(!is.na(str_match(test_data$search_term_stem_rem[i],test_data$value_stem_rem[i]))){
      test_data$ind_brand_search[i] = 1
      test_data$ind_no_brand[i] = 0
    } else{
      test_data$ind_brand_search[i] = 0
      test_data$ind_no_brand[i] = 0
    }  
  }else{
    test_data$ind_brand_search[i] = 0
    test_data$ind_no_brand[i] = 1
  }
} 

test_data$ind_brand_term = as.numeric(test_data$ind_brand_search) + as.numeric(test_data$ind_no_brand)

test_data = test_data %>% mutate(title_len = 
                                   str_length(sapply(product_title_stem,
                                                     function(x){return(gsub("[[:space:]]","",x))})))                    

test_data$ratio_search = test_data$len_search/test_data$title_len

test_data = test_data %>% mutate(ratio_match = ifelse(match_percent_attr > 0, match_percent/match_percent_attr,0))

saveRDS(test_data, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data.rds")

################Adding more variables to the data ####################

distance_func = function(search_q,title){
      search = str_split(search_q," ")
      search = str_trim(setdiff(search[[1]],""))
      product_title = str_split(title," ")
      product_title = str_trim(setdiff(product_title[[1]],""))
      length_prod = str_length(product_title)
      length_search_tot = sum(str_length(search))
      search_len = length(search)
      match = 0
      temp=as.data.frame(matrix(rep(0,search_len),search_len,1))
      jacc_min = vector()
      cos_min = vector()
      search_left = ""
      string_length = 0
      for (j in 1:search_len){
        jacc_dist = lapply(product_title,function(x){stringdist(search[j],x,method="jaccard")})
        cos_dist = lapply(product_title,function(x){stringdist(search[j],x,method="cosine")})
    
        jacc_min[j] = min(unlist(jacc_dist))
        jacc_min[j] = ifelse(jacc_min[j] == Inf,1,jacc_min[j])
        cos_min[j] = min(unlist(cos_dist))
        cos_min[j] = ifelse(cos_min[j] == Inf,1,cos_min[j])
      }
      
      jacc_match = ifelse(length(jacc_min) == length(which(is.na(jacc_min))),0,(mean(jacc_min,na.rm=T)))
      
      cos_match = ifelse(length(cos_min) == length(which(is.na(cos_min))),0,(mean(cos_min,na.rm=T)))
      
      
      jacc_ratio = ifelse(length(jacc_min) == length(which(is.na(jacc_min))),0,sum(jacc_min,na.rm=T))/length_search_tot
      cos_ratio = ifelse(length(cos_min) == length(which(is.na(cos_min))),0,sum(cos_min,na.rm=T))/length_search_tot
      
      if(length_search_tot == 0){  
        jacc_ratio = 1
        cos_ratio = 1 
        jacc_match = 1
        cos_match = 1 
      }
      return(as.data.frame(cbind(jacc_match,cos_match,jacc_ratio,cos_ratio)))
}

more_dist_train = t(mapply(distance_func,full_data[,"search_term_stem"],full_data[,"product_title_stem"]))

more_dist_test = t(mapply(distance_func,test_data[,"search_term_stem"],test_data[,"product_title_stem"]))

rownames(more_dist_train) = NULL

rownames(more_dist_test) = NULL

full_data_fin = cbind(full_data,more_dist_train)

test_data_fin = cbind(test_data,more_dist_test)

rm(full_data)
rm(test_data)

saveRDS(more_dist_train, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/more_dist_train.rds")

saveRDS(more_dist_test, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/more_dist_test.rds")

rm(more_dist_train)
rm(more_dist_test)

################## Product Desc include in the model ############
product_desc = data.frame(fread("/mapr/projects/GLRM_test/HomeDepot/product_descriptions.csv"))

product_desc$product_description = sapply(product_desc$product_description,str_to_lower)

product_desc = product_desc %>% mutate(product_description = gsub("sq ft","sqft",product_description),
                                       product_description = gsub("sq feet","sqft",product_description),
                                       product_description = gsub("square feet","sqft",product_description),
                                       product_description = gsub("sq.ft.","sqft",product_description),
                                       product_description = gsub("gallon","gal.",product_description),
                                       product_description = gsub("ounce","oz",product_description),
                                       product_description = gsub("ounces","oz",product_description),
                                       product_description = gsub("centimeter","cm",product_description),
                                       product_description = gsub("milimeter","mm",product_description),
                                       product_description = gsub("pound","lb.",product_description),
                                       product_description = gsub("feet","ft.",product_description),
                                       product_description = gsub("inches","in.",product_description),
                                       product_description = gsub("inch","in.",product_description),
                                       product_description = gsub("a/c","air conditioner",product_description),
                                       product_description = gsub("cellphone","cell phone",product_description),
                                       product_description = gsub("otr","over the range",product_description))

desc = tm_map(Corpus(VectorSource(product_desc$product_description)),removeWords, stopwords('english'))

desc = tm_map(desc,replacePunctuation)

product_desc$product_description_stem = as.character(tm_map(desc,stemDocument))

rm(desc)
product_desc = product_desc[,-2]

product_desc = product_desc %>% mutate(product_description_stem = gsub("  "," ",product_description_stem))

saveRDS(product_desc, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/product_desc.rds")

full_data_fin = full_data_fin %>% left_join(product_desc, by = "product_uid")

test_data_fin = test_data_fin %>% left_join(product_desc, by = "product_uid")

rm(product_desc)

desc_match_func = function(search_q,prod_desc){
    search = str_split(search_q," ")
    search = str_trim(setdiff(search[[1]],""))
    product_title = str_split(prod_desc," ")
    product_title = str_trim(setdiff(product_title[[1]],""))
    search_len = length(search)
    match = 0
    for (j in 1:search_len){
      if(length(search[j] > 0)){
        if(search[j] %in% product_title)
        {
          match = match + 1
        } 
      }
    }
    return(match/search_len)
}

train_match_percent_desc = mapply(desc_match_func,full_data_fin[,"search_term_stem"],
                                  full_data_fin[,"product_description_stem"])

test_match_percent_desc = mapply(desc_match_func,test_data_fin[,"search_term_stem"],
                                 test_data_fin[,"product_description_stem"])

full_data_fin[,"match_percent_desc"] = train_match_percent_desc

test_data_fin[,"match_percent_desc"] = test_match_percent_desc

rm(test_match_percent_desc)
rm(train_match_percent_desc)

full_data_fin[,"ratio_title_desc"] = ifelse(full_data_fin$match_percent_desc == 0,0,
                                        full_data_fin$match_percent/full_data_fin$match_percent_desc)

full_data_fin[,"ratio_attr_desc"] = ifelse(full_data_fin$match_percent_desc == 0,0,
                                        full_data_fin$match_percent_attr/full_data_fin$match_percent_desc)

full_data_fin = full_data_fin %>% mutate(desc_len = 
                                   str_length(sapply(product_description_stem,
                                                     function(x){return(gsub("[[:space:]]","",x))})))

full_data_fin[,"ratio_search_desc"] = full_data_fin$len_search/full_data_fin$desc_len

test_data_fin[,"ratio_title_desc"] = ifelse(test_data_fin$match_percent_desc == 0,0,
                                        test_data_fin$match_percent/test_data_fin$match_percent_desc)

test_data_fin[,"ratio_attr_desc"] = ifelse(test_data_fin$match_percent_desc == 0,0,
                                       test_data_fin$match_percent_attr/test_data_fin$match_percent_desc)

test_data_fin = test_data_fin %>% mutate(desc_len = 
                                   str_length(sapply(product_description_stem,
                                                     function(x){return(gsub("[[:space:]]","",x))})))

test_data_fin[,"ratio_search_desc"] = test_data_fin$len_search/test_data_fin$desc_len

distance_func_full = function(search_q,title){
  search = str_split(search_q," ")
  search = str_trim(setdiff(search[[1]],""))
  product_title = str_split(title," ")
  product_title = str_trim(setdiff(product_title[[1]],""))
  length_prod = str_length(product_title)
  length_search_tot = sum(str_length(search))
  search_len = length(search)
  match = 0
  temp=as.data.frame(matrix(rep(0,search_len),search_len,1))
  jacc_min = vector()
  cos_min = vector()
  hamming_min = vector()
  string_length = 0
  for (j in 1:search_len){
    jacc_dist = lapply(product_title,function(x){stringdist(search[j],x,method="jaccard")})
    cos_dist = lapply(product_title,function(x){stringdist(search[j],x,method="cosine")})
    hamming_dist = lapply(product_title,function(x){stringdist(search[j],x,method="hamming")})
    
    jacc_min[j] = min(unlist(jacc_dist))
    jacc_min[j] = ifelse(jacc_min[j] == Inf,1,jacc_min[j])
    cos_min[j] = min(unlist(cos_dist))
    cos_min[j] = ifelse(cos_min[j] == Inf,1,cos_min[j])
    hamming_min[j] = min(unlist(hamming_dist))
    hamming_min[j] = ifelse(hamming_min[j] == Inf,str_length(str_trim(search[j])),hamming_min[j])
  }
  
  jacc_match_desc = ifelse(length(jacc_min) == length(which(is.na(jacc_min))),0,(mean(jacc_min,na.rm=T)))
  
  cos_match_desc = ifelse(length(cos_min) == length(which(is.na(cos_min))),0,(mean(cos_min,na.rm=T)))
  
  hamming_match_desc = ifelse(length(hamming_min) == length(which(is.na(hamming_min))),0,(mean(hamming_min,na.rm=T)))
  
  
  
  jacc_ratio_desc = ifelse(length(jacc_min) == length(which(is.na(jacc_min))),0,sum(jacc_min,na.rm=T))/length_search_tot
  cos_ratio_desc = ifelse(length(cos_min) == length(which(is.na(cos_min))),0,sum(cos_min,na.rm=T))/length_search_tot
  hamming_ratio_desc = ifelse(length(hamming_min) == length(which(is.na(hamming_min))),0,
                              sum(hamming_min,na.rm=T))/length_search_tot
  
  if(length_search_tot == 0){  
    jacc_ratio_desc = 1
    cos_ratio_desc = 1 
    hamming_ratio_desc = 99
    jacc_ratio_desc = 1
    cos_ratio_desc = 1 
    hamming_ratio_desc = 99
  }
  return(as.data.frame(cbind(jacc_ratio_desc,cos_ratio_desc,hamming_ratio_desc,
                             hamming_match_desc,jacc_match_desc,cos_match_desc)))
}

saveRDS(full_data_fin, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/HomeDepot/full_data_fin.rds")

saveRDS(test_data_fin, "//mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_fin.rds")

desc_dist_train = t(mapply(distance_func_full,full_data_fin[,"search_term_stem"],
                           full_data_fin[,"product_description_stem"]))

desc_dist_test = t(mapply(distance_func_full,test_data_fin[,"search_term_stem"],
                          test_data_fin[,"product_description_stem"]))

rownames(desc_dist_train) = NULL

rownames(desc_dist_test) = NULL

full_data_fin = cbind(full_data_fin,desc_dist_train)

test_data_fin = cbind(test_data_fin,desc_dist_test)

rm(desc_dist_train)
rm(desc_dist_test)

for(i in c("match_percent_desc","jacc_match","cos_match","jacc_ratio","cos_ratio",
           "jacc_ratio_desc","cos_ratio_desc","hamming_ratio_desc",
           "hamming_match_desc","jacc_match_desc","cos_match_desc")){
  test_data_fin[,i] = as.numeric(test_data_fin[,i])
  print(i)
}

for(i in c("match_percent_desc","jacc_match","cos_match","jacc_ratio","cos_ratio",
           "jacc_ratio_desc","cos_ratio_desc","hamming_ratio_desc",
           "hamming_match_desc","jacc_match_desc","cos_match_desc")){
  full_data_fin[,i] = as.numeric(full_data_fin[,i])
  print(i)
}


full_data_fin[,"hamming_desc_ratio"] = ifelse(full_data_fin$match_percent_desc == 0,0,
                                              full_data_fin$hamming_match/full_data_fin$match_percent_desc)


test_data_fin[,"hamming_desc_ratio"] = ifelse(test_data_fin$match_percent_desc == 0,0,
                                              test_data_fin$hamming_match/test_data_fin$match_percent_desc)

full_data_fin[,"log_desc_len"] = log(full_data_fin$desc_len)

test_data_fin[,"log_desc_len"] = log(test_data_fin$desc_len)

full_data_fin[,"log_title_len"] = log(full_data_fin$title_len)

test_data_fin[,"log_title_len"] = log(test_data_fin$title_len)


full_data_fin[,"ratio_hamdesc_matchdesc"] = ifelse(full_data_fin$match_percent_desc == 0,0,
                                                   full_data_fin$hamming_match_desc/full_data_fin$match_percent_desc)

test_data_fin[,"ratio_hamdesc_matchdesc"] = ifelse(test_data_fin$match_percent_desc == 0,0,
                                                   test_data_fin$hamming_match_desc/test_data_fin$match_percent_desc)

full_data_fin[,"ratio_cosdesc_matchdesc"] = ifelse(full_data_fin$match_percent_desc == 0,0,
                                                   full_data_fin$cos_match_desc/full_data_fin$match_percent_desc)

test_data_fin[,"ratio_cosdesc_matchdesc"] = ifelse(test_data_fin$match_percent_desc == 0,0,
                                               test_data_fin$cos_match_desc/test_data_fin$match_percent_desc)

full_data_fin[,"ratio_jaccdesc_matchdesc"] = ifelse(full_data_fin$match_percent_desc == 0,0,
                                                    full_data_fin$jacc_match_desc/full_data_fin$match_percent_desc)

test_data_fin[,"ratio_jaccdesc_matchdesc"] = ifelse(test_data_fin$match_percent_desc == 0,0,
                                                    test_data_fin$jacc_match_desc/test_data_fin$match_percent_desc)

full_data_fin[,"exp_hamming_match"] = exp(full_data_fin[,"hamming_match"])

test_data_fin[,"exp_hamming_match"] = exp(test_data_fin[,"hamming_match"])

full_data_fin[,"exp_cos_match"] = exp(full_data_fin[,"cos_match"])

test_data_fin[,"exp_cos_match"] = exp(test_data_fin[,"cos_match"])

full_data_fin[,"exp_lv_match"] = exp(full_data_fin[,"lv_match"])

test_data_fin[,"exp_lv_match"] = exp(test_data_fin[,"lv_match"])

full_data_fin[,"expratio_hamdesc_matchdesc"] = exp(full_data_fin[,"ratio_hamdesc_matchdesc"])

test_data_fin[,"expratio_hamdesc_matchdesc"] = exp(test_data_fin[,"ratio_hamdesc_matchdesc"])

full_data_fin[,"expratio_jaccdesc_matchdesc"] = exp(full_data_fin[,"ratio_jaccdesc_matchdesc"])

test_data_fin[,"expratio_jaccdesc_matchdesc"] = exp(test_data_fin[,"ratio_jaccdesc_matchdesc"])

full_data_fin[,"expratio_cosdesc_matchdesc"] = exp(full_data_fin[,"ratio_cosdesc_matchdesc"])

test_data_fin[,"expratio_cosdesc_matchdesc"] = exp(test_data_fin[,"ratio_cosdesc_matchdesc"])

full_data_fin[,"exp_hamming_match_desc"] = exp(full_data_fin[,"hamming_match_desc"])

test_data_fin[,"exp_hamming_match_desc"] = exp(test_data_fin[,"hamming_match_desc"])

full_data_fin[,"exp_cos_ratio_desc"] = exp(full_data_fin[,"cos_ratio_desc"])

test_data_fin[,"exp_cos_ratio_desc"] = exp(test_data_fin[,"cos_ratio_desc"])

temp = c("match_percent_desc","ratio_title_desc","ratio_attr_desc","hamming_desc_ratio","ratio_hamdesc_matchdesc",
         "ratio_cosdesc_matchdesc","ratio_jaccdesc_matchdesc","expratio_hamdesc_matchdesc","expratio_cosdesc_matchdesc",
         "expratio_jaccdesc_matchdesc")

for(i in temp){
  full_data_fin[which(is.na(full_data_fin[,i])),i] = 0
  print(i)
}

for(i in temp){
  test_data_fin[which(is.na(test_data_fin[,i])),i] = 0
  print(i)
}

saveRDS(full_data_fin, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/full_data_fin2.rds")

saveRDS(test_data_fin, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_fin2.rds")

################ More variables ##################

full_data_fin[,"len_brand"] = str_length(full_data_fin[,"value_stem_rem"])

test_data_fin[,"len_brand"] = str_length(test_data_fin[,"value_stem_rem"])

full_data_fin[,"ratlen_search_brand"] = ifelse(full_data_fin[,"len_search"] == 0,0,full_data_fin[,"len_brand"]/full_data_fin[,"len_search"])

test_data_fin[,"ratlen_search_brand"] = ifelse(test_data_fin[,"len_search"] == 0,0,test_data_fin[,"len_brand"]/test_data_fin[,"len_search"])

full_data_fin[,"expstring_dist"] = exp(full_data_fin[,"string_dist"])

test_data_fin[,"expstring_dist"] = exp(test_data_fin[,"string_dist"])

full_data_fin[,"expstring_distbit"] = exp(full_data_fin[,"string_dist_bit"])

test_data_fin[,"expstring_distbit"] = exp(test_data_fin[,"string_dist_bit"])

full_data_fin[,"rat_desc_title_len"] = ifelse(full_data_fin[,"title_len"] == 0,0,full_data_fin[,"desc_len"]/full_data_fin[,"title_len"])

test_data_fin[,"rat_desc_title_len"] = ifelse(test_data_fin[,"title_len"] == 0,0,test_data_fin[,"desc_len"]/test_data_fin[,"title_len"])

full_data_fin[,"exp_lcs_match"] = exp(full_data_fin[,"lcs_match"])

test_data_fin[,"exp_lcs_match"] = exp(test_data_fin[,"lcs_match"])

full_data_fin[,"exp_lcs_ratio"] = exp(full_data_fin[,"lcs_ratio"])

test_data_fin[,"exp_lcs_ratio"] = exp(test_data_fin[,"lcs_ratio"])

########################################################################################

distance_func1 = function(search_q,title){
  search = str_split(search_q," ")
  search = str_trim(setdiff(search[[1]],""))
  product_title = str_split(title," ")
  product_title = str_trim(setdiff(product_title[[1]],""))
  length_prod = str_length(product_title)
  length_search_tot = sum(str_length(search))
  search_len = length(search)
  match = 0
  temp=as.data.frame(matrix(rep(0,search_len),search_len,1))
  qgram_min = vector()
  jaro_min = vector()
  search_left = ""
  string_length = 0
  for (j in 1:search_len){
    qgram_dist = lapply(product_title,function(x){stringdist(search[j],x,method="qgram")})
    jaro_dist = lapply(product_title,function(x){stringdist(search[j],x,method="jw",p=0.1)})
    
    qgram_min[j] = min(unlist(qgram_dist))
    qgram_min[j] = ifelse(qgram_min[j] == Inf,str_length(str_trim(search[j])),qgram_min[j])
    jaro_min[j] = min(unlist(jaro_dist))
    jaro_min[j] = ifelse(jaro_min[j] == Inf,1,jaro_min[j])
  }
  
  qgram_match = ifelse(length(qgram_min) == length(which(is.na(qgram_min))),0,(mean(qgram_min,na.rm=T)))
  jaro_match = ifelse(length(jaro_min) == length(which(is.na(jaro_min))),0,(mean(jaro_min,na.rm=T)))
  
  qgram_ratio = ifelse(length(qgram_min) == length(which(is.na(qgram_min))),0,sum(qgram_min,na.rm=T))/length_search_tot
  jaro_ratio = ifelse(length(jaro_min) == length(which(is.na(jaro_min))),0,sum(jaro_min,na.rm=T))/length_search_tot
  
  if(length_search_tot == 0){  
    qgram_ratio = 99
    jaro_ratio = 1
    qgram_match = 99
    jaro_match = 1
  }
  return(as.data.frame(cbind(jaro_match,qgram_match,jaro_ratio,qgram_ratio)))
}

desc_dist_train1 = t(mapply(distance_func1,full_data_fin[,"search_term_stem"],full_data_fin[,"product_title_stem"]))

desc_dist_test1 = t(mapply(distance_func1,test_data_fin[,"search_term_stem"],test_data_fin[,"product_title_stem"]))

rownames(desc_dist_train1) = NULL

rownames(desc_dist_test1) = NULL

full_data_fin = cbind(full_data_fin,desc_dist_train1)

test_data_fin = cbind(test_data_fin,desc_dist_test1)

rm(desc_dist_train1)
rm(desc_dist_test1)

temp = c("jaro_match","qgram_match","jaro_ratio","qgram_ratio")

for(i in temp){
  full_data_fin[which(is.na(full_data_fin[,i])),i] = 0
  print(i)
}

for(i in temp){
  test_data_fin[which(is.na(test_data_fin[,i])),i] = 0
  print(i)
}


for(i in temp){
  test_data_fin[,i] = as.numeric(test_data_fin[,i])
  print(i)
}

for(i in temp){
  full_data_fin[,i] = as.numeric(full_data_fin[,i])
  print(i)
}

saveRDS(full_data_fin, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/full_data_fin3_new.rds")

saveRDS(test_data_fin, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_fin3_new.rds")

########### TFIDF finally ##############

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/full_data_fin3_new.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_fin3_new.rds")

BigramTokenizer = function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 2))}

search_txt = Corpus(VectorSource(rbind(full_data_fin[,-5],test_data_fin)[,"search_term_stem"]))

search_dtm = DocumentTermMatrix(search_txt,control=list(removeNumbers=TRUE, tokenize=BigramTokenizer,
                                                        weighting=function(x) weightTfIdf(x,normalize=T)))
search_dtm = removeSparseTerms(search_dtm,0.995)
df_search = as.data.frame(as.matrix(search_dtm))

colnames(df_search)=paste("pd_",colnames(df_search),sep="")

rm(search_txt)
rm(search_dtm)

options(mc.cores=1)

title_txt = Corpus(VectorSource(rbind(full_data_fin[,-5],test_data_fin)[,"product_title_stem"]))

title_dtm = DocumentTermMatrix(title_txt,control=list(removeNumbers=TRUE,tokenize=BigramTokenizer,
                                                      weighting=function(x) weightTfIdf(x,normalize=T)))
title_dtm = removeSparseTerms(title_dtm,0.995)
df_title = as.data.frame(as.matrix(title_dtm))
colnames(df_title)=paste("pd_",colnames(df_title),sep="")

rm(title_txt)
rm(title_dtm)

attr_txt = Corpus(VectorSource(rbind(full_data_fin[,-5],test_data_fin)[,"desc_stem"]))

attr_dtm = DocumentTermMatrix(attr_txt,control=list(removeNumbers=TRUE,
                                                    weighting=function(x) weightTfIdf(x,normalize=T)))
attr_dtm = removeSparseTerms(attr_dtm,0.99)
df_attr = as.data.frame(as.matrix(attr_dtm))
colnames(df_attr)=paste("pd_",colnames(df_attr),sep="")

rm(attr_txt)
rm(attr_dtm)

desc_txt = Corpus(VectorSource(rbind(full_data_fin[,-5],test_data_fin)[,"product_description_stem"]))

desc_dtm = DocumentTermMatrix(desc_txt,control=list(removeNumbers=TRUE,
                                                    weighting=function(x) weightTfIdf(x,normalize=T)))
desc_dtm = removeSparseTerms(desc_dtm,0.97)
df_desc = as.data.frame(as.matrix(desc_dtm))
colnames(df_desc)=paste("pd_",colnames(df_desc),sep="")

final_tfidf = as.matrix(cbind(df_desc,df_attr,df_title,df_search))

saveRDS(final_tfidf,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/full_data_tfidf_bigram.rds")

rm(desc_txt)
rm(desc_dtm)
rm(df_desc)
rm(df_attr)
rm(df_title)
rm(df_search)

final_tfidf_svd = svds(final_tfidf,k = 100,nv =100, nu=0);

saveRDS(final_tfidf_svd,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/full_data_tfidfsvd_final.rds")

final_tfidf_var = final_tfidf%*%final_tfidf_svd$v

saveRDS(final_tfidf_var,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_tfidf_svd_final.rds")

final_tfidf_var = as.data.frame(final_tfidf_var)

train_tfidf = final_tfidf_var[1:74067,]

test_tfidf = final_tfidf_var[74068:nrow(final_tfidf_var),]

full_data_fin2 = cbind(full_data_fin,train_tfidf)

test_data_fin2 = cbind(test_data_fin,test_tfidf)

saveRDS(full_data_fin2,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_inc_tfidf_final.rds")

saveRDS(test_data_fin2,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_inc_tfidf_final.rds")

################### Now load the data ########

options(mc.cores=1)

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_inc_tfidf_final.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_inc_tfidf_final.rds")

search_txt = Corpus(VectorSource(rbind(full_data_fin[,-5],test_data_fin)[,c("search_term_stem","product_title_stem")]))

txt = Corpus(VectorSource(paste(search_txt$search_term_stem,search_txt$product_title_stem)))

search_dtm = TermDocumentMatrix(txt,control=list(removeNumbers=TRUE, 
                                                 weighting=function(x) weightTfIdf(x,normalize=T)))
search_dtm = removeSparseTerms(search_dtm,0.995)

lsa_fit_fin = lsa(as.matrix(search_dtm))  # create LSA space

lsaMatrix = t(diag(lsa_fit_fin$sk) %*% t(lsa_fit_fin$dk))

lsaMatrix = as.data.frame(lsaMatrix)

colnames(lsaMatrix) = paste("lsa_",colnames(lsaMatrix),sep="")

rm(search_txt)
rm(search_dtm)

lsaMatrix = as.matrix(lsaMatrix)

final_lsa_svd = svds(lsaMatrix,k = 50,nv =50, nu=0);

final_lsa_var = lsaMatrix%*%final_lsa_svd$v

train_lsa = final_lsa_var[1:74067,]

test_lsa = final_lsa_var[74068:nrow(final_lsa_var),]

full_data_fin2 = cbind(full_data_fin,train_lsa)

test_data_fin2 = cbind(test_data_fin,test_lsa)

saveRDS(full_data_fin2,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/train_data_inc_tfidf_final.rds")

saveRDS(test_data_fin2,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/test_data_inc_tfidf_final.rds")

############### ghuam fira ke tfidf #########

options(mc.cores=1)

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/train_data_inc_tfidf_final.rds")
test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/test_data_inc_tfidf_final.rds")

BigramTokenizer = function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 2))}

search_txt = Corpus(VectorSource(rbind(full_data_fin[,-5],test_data_fin)[,"search_term_stem"]))

search_dtm = DocumentTermMatrix(search_txt,control=list(removeNumbers=TRUE, tokenize=BigramTokenizer,
                                                        weighting=function(x) weightSMART(x, spec = "ntc")))
search_dtm = removeSparseTerms(search_dtm,0.995)
df_search = as.data.frame(as.matrix(search_dtm))

colnames(df_search)=paste("pd_",colnames(df_search),sep="")

rm(search_txt)
rm(search_dtm)

title_txt = Corpus(VectorSource(rbind(full_data_fin[,-5],test_data_fin)[,"product_title_stem"]))

title_dtm = DocumentTermMatrix(title_txt,control=list(removeNumbers=TRUE,tokenize=BigramTokenizer,
                                                      weighting=function(x) weightSMART(x, spec = "ntc")))
title_dtm = removeSparseTerms(title_dtm,0.995)
df_title = as.data.frame(as.matrix(title_dtm))
colnames(df_title)=paste("pd_",colnames(df_title),sep="")

rm(title_txt)
rm(title_dtm)

final_tfidf = as.matrix(cbind(df_title,df_search))

rm(desc_txt)
rm(desc_dtm)
rm(df_desc)
rm(df_attr)
rm(df_title)
rm(df_search)

final_tfidf_svd = svds(final_tfidf,k = 25,nv =25, nu=0)

final_tfidf_var = final_tfidf%*%final_tfidf_svd$v

final_tfidf_var = as.data.frame(final_tfidf_var)

train_tfidf = final_tfidf_var[1:74067,]

test_tfidf = final_tfidf_var[74068:nrow(final_tfidf_var),]

full_data_fin2 = cbind(full_data_fin,train_tfidf)

test_data_fin2 = cbind(test_data_fin,test_tfidf)

colnames(full_data_fin2)[246:270] = paste("ntcV",seq(1,25),sep="")
colnames(test_data_fin2)[245:269] = paste("ntcV",seq(1,25),sep="")

saveRDS(full_data_fin2,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_inc_tfidf1_lsa.rds")

saveRDS(test_data_fin2,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_inc_tfidf1_lsas.rds")

############### Final Countdown ##############

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_inc_tfidf1_lsa.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_inc_tfidf1_lsas.rds")

########### Try splines fitting ###############

full_data_fin[,"srt_jacc"] = sqrt(full_data_fin[,"jacc_match"])

test_data_fin[,"srt_jacc"] = sqrt(test_data_fin[,"jacc_match"])

full_data_fin[,"srt_jaro"] = sqrt(full_data_fin[,"jaro_match"])

test_data_fin[,"srt_jaro"] = sqrt(test_data_fin[,"jaro_match"])

full_data_fin[,"ind_jaro"] = ifelse(full_data_fin[,"jaro_match"] >= 0.28,1,0)

test_data_fin[,"ind_jaro"] = ifelse(test_data_fin[,"jaro_match"] >= 0.28,1,0)

full_data_fin[,"ind_jacc"] = ifelse(full_data_fin[,"jacc_match"] >= 0.45,1,0)

test_data_fin[,"ind_jacc"] = ifelse(test_data_fin[,"jacc_match"] >= 0.45,1,0)

full_data_fin[,"ind_match"] = ifelse(full_data_fin[,"match_percent"] < 0.31,1,0)

test_data_fin[,"ind_match"] = ifelse(test_data_fin[,"match_percent"] < 0.31,1,0)

full_data_fin[,"ind_cos"] = ifelse(full_data_fin[,"cos_match"] >= 0.32,1,0)

test_data_fin[,"ind_cos"] = ifelse(test_data_fin[,"cos_match"] >= 0.32,1,0)

full_data_fin[,"cos_tfidf_inter"] = full_data_fin$cos_match*full_data_fin$V1

test_data_fin[,"cos_tfidf_inter"] = test_data_fin$cos_match*test_data_fin$V1

saveRDS(full_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final.rds")

saveRDS(test_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final.rds")

###########################other variable creation ##########

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final.rds")

desc_dist_train = t(mapply(distance_func_full,full_data_fin[,"search_term_stem"],
                           full_data_fin[,"desc_stem"]))

desc_dist_test = t(mapply(distance_func_full,test_data_fin[,"search_term_stem"],
                          test_data_fin[,"desc_stem"]))

rownames(desc_dist_train) = NULL

rownames(desc_dist_test) = NULL

full_data_fin = cbind(full_data_fin,desc_dist_train)

test_data_fin = cbind(test_data_fin,desc_dist_test)

colnames(full_data_fin)[278:283] = c("jacc_ratio_attr","cos_ratio_attr","hamming_ratio_attr","hamming_match_attr","jacc_match_attr","cos_match_attr")

colnames(test_data_fin)[277:282] = c("jacc_ratio_attr","cos_ratio_attr","hamming_ratio_attr","hamming_match_attr","jacc_match_attr","cos_match_attr")

saveRDS(full_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final1.rds")

saveRDS(test_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final1.rds")

################### Query Level timepass #################

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final1.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final1.rds")

full_data_fin$avg_string_dist = full_data_fin$string_dist/full_data_fin$title_len

test_data_fin$avg_string_dist = test_data_fin$string_dist/test_data_fin$title_len

full_data_fin$avg_string_distbit = full_data_fin$string_dist_bit/full_data_fin$title_len

test_data_fin$avg_string_distbit = test_data_fin$string_dist_bit/test_data_fin$title_len

full_data_fin$avg_dist_act = full_data_fin$dist_act_search/full_data_fin$title_len

test_data_fin$avg_dist_act = test_data_fin$dist_act_search/test_data_fin$title_len

full_data_fin = full_data_fin[,-c(280,281)]
test_data_fin = test_data_fin[,-c(279,280)]

for(i in c("jacc_match_attr","cos_match_attr","jacc_ratio_attr","cos_ratio_attr")){
  full_data_fin[,i] = as.numeric(full_data_fin[,i])
  test_data_fin[,i] = as.numeric(test_data_fin[,i])
  print(i)
}

full_data_fin_summ = full_data_fin %>% group_by(search_term_stem) %>% 
  summarise(avglv_match = mean(lv_match),
            sdlv_match = sd(lv_match),
            medlv_match = quantile(lv_match,0.5),
            avgosa_match = mean(osa_match),
            sdosa_match = sd(osa_match),
            medosa_match = quantile(osa_match,0.5),
            avglcs_match = mean(lcs_match),
            sdlcs_match = sd(lcs_match),
            medlcs_match = quantile(lcs_match,0.5),
            avgjacc_match = mean(jacc_match),
            sdjacc_match = sd(jacc_match),
            medjacc_match = quantile(jacc_match,0.5),
            avgjaro_match = mean(jaro_match),
            sdjaro_match = sd(jaro_match),
            medjaro_match = quantile(jaro_match,0.5),
            avgcos_match = mean(cos_match),
            sdcos_match = sd(cos_match),
            medcos_match = quantile(cos_match,0.5),
            avgmatch_percent = mean(match_percent),
            sdmatch_percent = sd(match_percent),
            medmatch_percent = quantile(match_percent,0.5),
            avgmatch_percentattr = mean(match_percent_attr),
            sdmatch_percentattr = sd(match_percent_attr),
            medmatch_percentattr = quantile(match_percent_attr,0.5),
            avgstring_dist = mean(string_dist),
            sdstring_dist = sd(string_dist),
            medstring_dist = quantile(string_dist,0.5),
            avgstring_distbit = mean(string_dist_bit),
            sdstring_distbit = sd(string_dist_bit),
            medstring_distbit = quantile(string_dist_bit,0.5),
            avgdist_act_search = mean(dist_act_search),
            sddist_act_search = sd(dist_act_search),
            meddist_act_search = quantile(dist_act_search,0.5),
            avgavg_string_dist = mean(avg_string_dist),
            sdavg_string_dist = sd(avg_string_dist),
            medavg_string_dist = quantile(avg_string_dist,0.5),
            avgavg_string_distbit = mean(avg_string_distbit),
            sdavg_string_distbit = sd(avg_string_distbit),
            medavg_string_distbit = quantile(avg_string_distbit,0.5),
            avgavg_dist_act = mean(avg_dist_act),
            sdavg_dist_act = sd(avg_dist_act),
            medavg_dist_act = quantile(avg_dist_act,0.5),
            avgmatch_percent_desc = mean(match_percent_desc),
            sdmatch_percent_desc = sd(match_percent_desc),
            medmatch_percent_desc = quantile(match_percent_desc,0.5),
            avgjacc_match_attr = mean(jacc_match_attr),
            sdjacc_match_attr = sd(jacc_match_attr),
            medjacc_match_attr = quantile(jacc_match_attr,0.5),
            avgcos_match_attr = mean(cos_match_attr),
            sdcos_match_attr = sd(cos_match_attr),
            medcos_match_attr = quantile(cos_match_attr,0.5),
            count_query = n(),
            count_title = n_distinct(product_uid))%>% 
            ungroup()

test_data_fin_summ = test_data_fin %>% group_by(search_term_stem) %>% 
  summarise(avglv_match = mean(lv_match),
            sdlv_match = sd(lv_match),
            medlv_match = quantile(lv_match,0.5),
            avgosa_match = mean(osa_match),
            sdosa_match = sd(osa_match),
            medosa_match = quantile(osa_match,0.5),
            avglcs_match = mean(lcs_match),
            sdlcs_match = sd(lcs_match),
            medlcs_match = quantile(lcs_match,0.5),
            avgjacc_match = mean(jacc_match),
            sdjacc_match = sd(jacc_match),
            medjacc_match = quantile(jacc_match,0.5),
            avgjaro_match = mean(jaro_match),
            sdjaro_match = sd(jaro_match),
            medjaro_match = quantile(jaro_match,0.5),
            avgcos_match = mean(cos_match),
            sdcos_match = sd(cos_match),
            medcos_match = quantile(cos_match,0.5),
            avgmatch_percent = mean(match_percent),
            sdmatch_percent = sd(match_percent),
            medmatch_percent = quantile(match_percent,0.5),
            avgmatch_percentattr = mean(match_percent_attr),
            sdmatch_percentattr = sd(match_percent_attr),
            medmatch_percentattr = quantile(match_percent_attr,0.5),
            avgstring_dist = mean(string_dist),
            sdstring_dist = sd(string_dist),
            medstring_dist = quantile(string_dist,0.5),
            avgstring_distbit = mean(string_dist_bit),
            sdstring_distbit = sd(string_dist_bit),
            medstring_distbit = quantile(string_dist_bit,0.5),
            avgdist_act_search = mean(dist_act_search),
            sddist_act_search = sd(dist_act_search),
            meddist_act_search = quantile(dist_act_search,0.5),
            avgavg_string_dist = mean(avg_string_dist),
            sdavg_string_dist = sd(avg_string_dist),
            medavg_string_dist = quantile(avg_string_dist,0.5),
            avgavg_string_distbit = mean(avg_string_distbit),
            sdavg_string_distbit = sd(avg_string_distbit),
            medavg_string_distbit = quantile(avg_string_distbit,0.5),
            avgavg_dist_act = mean(avg_dist_act),
            sdavg_dist_act = sd(avg_dist_act),
            medavg_dist_act = quantile(avg_dist_act,0.5),
            avgmatch_percent_desc = mean(match_percent_desc),
            sdmatch_percent_desc = sd(match_percent_desc),
            medmatch_percent_desc = quantile(match_percent_desc,0.5),
            avgjacc_match_attr = mean(jacc_match_attr),
            sdjacc_match_attr = sd(jacc_match_attr),
            medjacc_match_attr = quantile(jacc_match_attr,0.5),
            avgcos_match_attr = mean(cos_match_attr),
            sdcos_match_attr = sd(cos_match_attr),
            medcos_match_attr = quantile(cos_match_attr,0.5),
            count_query = n(),
            count_title = n_distinct(product_uid))%>% 
  ungroup()
            
full_data_fin = full_data_fin %>% inner_join(full_data_fin_summ,by="search_term_stem")

test_data_fin = test_data_fin %>% inner_join(test_data_fin_summ,by="search_term_stem")

saveRDS(full_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final2.rds")

saveRDS(test_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final2.rds")

##########################Bi gram similarity #################

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final2.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final2.rds")

BigramTokenizer = function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}

bigram_match = function(search_q,title){
  search = BigramTokenizer(search_q)
  product_title = BigramTokenizer(title)
  search_len = length(search)
  match = 0
  jacc_min = vector()
  jaro_min = vector()
  cos_min = vector()
  for (j in 1:search_len){
    jacc_dist = lapply(product_title,function(x){stringdist(search[j],x,method="jaccard")})
    jaro_dist = lapply(product_title,function(x){stringdist(search[j],x,method="jw",p=0.1)})
    cos_dist = lapply(product_title,function(x){stringdist(search[j],x,method="cosine")})
    
    jacc_min[j] = min(unlist(jacc_dist))
    jacc_min[j] = ifelse(jacc_min[j] == Inf,str_length(str_trim(search[j])),jacc_min[j])
    jaro_min[j] = min(unlist(jaro_dist))
    jaro_min[j] = ifelse(jaro_min[j] == Inf,1,jaro_min[j])
    cos_min[j] = min(unlist(cos_dist))
    cos_min[j] = ifelse(cos_min[j] == Inf,1,cos_min[j])
  }
  
  jaro_match_bi = ifelse(length(jaro_min) == length(which(is.na(jaro_min))),0,(mean(jaro_min,na.rm=T)))
  jacc_match_bi = ifelse(length(jacc_min) == length(which(is.na(jacc_min))),0,(mean(jacc_min,na.rm=T)))
  cos_match_bi = ifelse(length(cos_min) == length(which(is.na(cos_min))),0,(mean(cos_min,na.rm=T)))  
  jaro_match_minbi = ifelse(length(jaro_min) == length(which(is.na(jaro_min))),0,(min(jaro_min,na.rm=T)))
  jacc_match_minbi = ifelse(length(jacc_min) == length(which(is.na(jacc_min))),0,(min(jacc_min,na.rm=T)))
  cos_match_minbi = ifelse(length(cos_min) == length(which(is.na(cos_min))),0,(min(cos_min,na.rm=T)))  
  
  if(search_len == 0){  
    jacc_match_bi = 1
    cos_match_bi = 1
    jaro_match_bi = 1
    jaro_match_minbi = 1
    jacc_match_minbi = 1
    cos_match_minbi = 1
  }
  return(as.data.frame(cbind(jaro_match_bi,jacc_match_bi,cos_match_bi,jaro_match_minbi,jacc_match_minbi,cos_match_minbi)))
}  

bigram_sim_tr = mapply(bigram_match,full_data_fin[,"search_term_stem"],full_data_fin[,"product_title_stem"])

bigram_sim_te = mapply(bigram_match,test_data_fin[,"search_term_stem"],test_data_fin[,"product_title_stem"])

saveRDS(bigram_sim_tr,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/bigram_sim_tr.rds")

saveRDS(bigram_sim_te,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/bigram_sim_te.rds")

full_data_fin = cbind(full_data_fin,t(bigram_sim_tr))
test_data_fin = cbind(test_data_fin,t(bigram_sim_te))

for(i in c("jaro_match_bi","jacc_match_bi","cos_match_bi","jaro_match_minbi","jacc_match_minbi",
           "cos_match_minbi")){
  full_data_fin[,i] = as.numeric(full_data_fin[,i])
  test_data_fin[,i] = as.numeric(test_data_fin[,i])
  print(i)
}

abigram_sim_tr = mapply(bigram_match,full_data_fin[,"search_term_stem"],full_data_fin[,"desc_stem"])

abigram_sim_te = mapply(bigram_match,test_data_fin[,"search_term_stem"],test_data_fin[,"desc_stem"])

saveRDS(abigram_sim_tr,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/abigram_sim_tr.rds")

saveRDS(abigram_sim_te,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/abigram_sim_te.rds")

full_data_fin = cbind(full_data_fin,t(abigram_sim_tr))
test_data_fin = cbind(test_data_fin,t(abigram_sim_te))

colnames(full_data_fin)[344:349] = c("jaro_match_bi_att","jacc_match_bi_att","cos_match_bi_att","jaro_match_minbi_att",
                                    "jacc_match_minbi_att", "cos_match_minbi_att")
colnames(test_data_fin)[343:348] = c("jaro_match_bi_att","jacc_match_bi_att","cos_match_bi_att","jaro_match_minbi_att",
                                    "jacc_match_minbi_att", "cos_match_minbi_att")

for(i in c("jaro_match_bi_att","jacc_match_bi_att","cos_match_bi_att","jaro_match_minbi_att",
           "jacc_match_minbi_att", "cos_match_minbi_att")){
  full_data_fin[,i] = as.numeric(full_data_fin[,i])
  test_data_fin[,i] = as.numeric(test_data_fin[,i])
  print(i)
}

full_data_fin_summ = full_data_fin %>% group_by(search_term_stem) %>% 
  summarise(avgjaro_match_bi = mean(jaro_match_bi),
            sdjaro_match_bi = sd(jaro_match_bi),
            medjaro_match_bi = quantile(jaro_match_bi,0.5),
            avgjacc_match_bi = mean(jacc_match_bi),
            sdjacc_match_bi = sd(jacc_match_bi),
            medjacc_match_bi = quantile(jacc_match_bi,0.5),
            avgcos_match_bi = mean(cos_match_bi),
            sdcos_match_bi = sd(cos_match_bi),
            medcos_match_bi = quantile(cos_match_bi,0.5),
            avgjaro_match_bi_att = mean(jaro_match_bi_att),
            sdjaro_match_bi_att = sd(jaro_match_bi_att),
            medjaro_match_bi_att = quantile(jaro_match_bi_att,0.5),
            avgjacc_match_bi_att = mean(jacc_match_bi_att),
            sdjacc_match_bi_att = sd(jacc_match_bi_att),
            medjacc_match_bi_att = quantile(jacc_match_bi_att,0.5),
            avgcos_match_bi_att = mean(cos_match_bi_att),
            sdcos_match_bi_att = sd(cos_match_bi_att),
            medcos_match_bi_att = quantile(cos_match_bi_att,0.5)) %>% 
            ungroup()

test_data_fin_summ = test_data_fin %>% group_by(search_term_stem) %>% 
  summarise(avgjaro_match_bi = mean(jaro_match_bi),
            sdjaro_match_bi = sd(jaro_match_bi),
            medjaro_match_bi = quantile(jaro_match_bi,0.5),
            avgjacc_match_bi = mean(jacc_match_bi),
            sdjacc_match_bi = sd(jacc_match_bi),
            medjacc_match_bi = quantile(jacc_match_bi,0.5),
            avgcos_match_bi = mean(cos_match_bi),
            sdcos_match_bi = sd(cos_match_bi),
            medcos_match_bi = quantile(cos_match_bi,0.5),
            avgjaro_match_bi_att = mean(jaro_match_bi_att),
            sdjaro_match_bi_att = sd(jaro_match_bi_att),
            medjaro_match_bi_att = quantile(jaro_match_bi_att,0.5),
            avgjacc_match_bi_att = mean(jacc_match_bi_att),
            sdjacc_match_bi_att = sd(jacc_match_bi_att),
            medjacc_match_bi_att = quantile(jacc_match_bi_att,0.5),
            avgcos_match_bi_att = mean(cos_match_bi_att),
            sdcos_match_bi_att = sd(cos_match_bi_att),
            medcos_match_bi_att = quantile(cos_match_bi_att,0.5)) %>% 
            ungroup()

full_data_fin = full_data_fin %>% inner_join(full_data_fin_summ,by="search_term_stem")

test_data_fin = test_data_fin %>% inner_join(test_data_fin_summ,by="search_term_stem")

for(i in colnames(full_data_fin)[270:367]){
  full_data_fin[which(is.na(full_data_fin[,i])),i] = 0
}

for(i in colnames(test_data_fin)[270:366]){
  test_data_fin[which(is.na(test_data_fin[,i])),i] = 0
}


saveRDS(full_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final3.rds")

saveRDS(test_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final3.rds")

######################## PoS tag kar denge hum ##################

options(mc.cores=1)

library(pbapply)

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final3.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final3.rds")

full = as.data.frame(rbind(full_data_fin[,-5],test_data_fin)[,"search_term"])

colnames(full) = "search"

full[,"search"] = sapply(full[,"search"],function(x) {return(as.character(x))})

library(openNLP)
library(NLP)
library(rJava)

options(java.parameters = "-Xmx8000m")
.jinit(parameters="-Xmx4g")

jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
} 

corpus.tagged.pos = function(x){
  x = as.String(x)
  search_annotate = annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  search_pos_annotate = annotate(x, Maxent_POS_Tag_Annotator(), search_annotate)
  pos_words = subset(search_pos_annotate, type == "word")
  tag_search = lapply(pos_words$features, '[[', "POS")
  pos_index = grep("NN", tag_search)
  if(x == ""){
    noun_strings = x
  } else{
    if(length(pos_index) == 0){
      noun_strings = ""
    } else if(length(pos_index) == 1){
      append_pos_tag = paste(x,tag_search[pos_index],sep="/")
      jgc()
      split_nn = sapply(append_pos_tag, function(i) {strsplit(unlist(i),'/NN(P)?(S)?')})  
      names(split_nn) = NULL
      noun_strings = sapply(split_nn, function(i) paste(i, collapse = " "))    
    } else {
      append_pos_tag_ind = sprintf("%s/%s", x[pos_words][pos_index], tag_search[pos_index])
      append_pos_tag = paste(append_pos_tag_ind, collapse = " ")
      jgc()
      split_nn = sapply(append_pos_tag, function(i) {strsplit(unlist(i),'/NN(P)?(S)?')})  
      names(split_nn) = NULL
      noun_strings = sapply(split_nn, function(i) paste(i, collapse = " "))
    }
  }
  return(noun_strings)
}

noun_only_search = pbsapply(full[,"search"],corpus.tagged.pos)

noun_only_search = gsub("  "," ",noun_only_search)

saveRDS(noun_only_search,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/noun_only_search.rds")

############### Noun search term #############

tr_search = pbsapply(full_data_fin[,"search_term_stem"],BigramTokenizer)

tr_search_len = pbsapply(tr_search,length)
  
te_search = pbsapply(test_data_fin[,"search_term_stem"],BigramTokenizer)

te_search_len = pbsapply(te_search,length)

n_search_term_stem = tm_map(Corpus(VectorSource(noun_only_search)),removeWords, stopwords('english'))
n_search_term_stem = tm_map(n_search_term_stem,replacePunctuation)

n_search_term_stem = as.character(tm_map(n_search_term_stem,stemDocument))

product_title = as.data.frame(rbind(full_data_fin[,-5],test_data_fin)[,"product_title_stem"])

final_match = cbind(n_search_term_stem,product_title)

colnames(final_match) = c("search","title")

BigramTokenizer = function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}

bigram_match_update = function(search_q,title){
  search = BigramTokenizer(search_q)
  product_title = BigramTokenizer(title)
  search_len = length(search)
  match = 0
  jacc_min = vector()
  jaro_min = vector()
  cos_min = vector()
  for (j in 1:search_len){
    jacc_dist = lapply(product_title,function(x){stringdist(search[j],x,method="jaccard")})
    jaro_dist = lapply(product_title,function(x){stringdist(search[j],x,method="jw",p=0.1)})
    cos_dist = lapply(product_title,function(x){stringdist(search[j],x,method="cosine")})
    
    jacc_min[j] = min(unlist(jacc_dist))
    jacc_min[j] = ifelse(jacc_min[j] == Inf,str_length(str_trim(search[j])),jacc_min[j])
    jaro_min[j] = min(unlist(jaro_dist))
    jaro_min[j] = ifelse(jaro_min[j] == Inf,1,jaro_min[j])
    cos_min[j] = min(unlist(cos_dist))
    cos_min[j] = ifelse(cos_min[j] == Inf,1,cos_min[j])
  }
  
  jaro_match_bi = ifelse(length(jaro_min) == length(which(is.na(jaro_min))),0,(mean(jaro_min,na.rm=T)))
  jacc_match_bi = ifelse(length(jacc_min) == length(which(is.na(jacc_min))),0,(mean(jacc_min,na.rm=T)))
  cos_match_bi = ifelse(length(cos_min) == length(which(is.na(cos_min))),0,(mean(cos_min,na.rm=T)))  
  jaro_match_maxbi = ifelse(length(jaro_min) == length(which(is.na(jaro_min))),0,(max(jaro_min,na.rm=T)))
  jacc_match_maxbi = ifelse(length(jacc_min) == length(which(is.na(jacc_min))),0,(max(jacc_min,na.rm=T)))
  cos_match_maxbi = ifelse(length(cos_min) == length(which(is.na(cos_min))),0,(max(cos_min,na.rm=T)))  
  
  if(search_len == 0){  
    jacc_match_bi = 1
    cos_match_bi = 1
    jaro_match_bi = 1
    jaro_match_maxbi = 1
    jacc_match_maxbi = 1
    cos_match_maxbi = 1
  }
  return(as.data.frame(cbind(jaro_match_bi,jacc_match_bi,cos_match_bi,jaro_match_maxbi,jacc_match_maxbi,cos_match_maxbi)))
}  

UnigramTokenizer = function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))}

unigram_match_update = function(search_q,title){
  search = UnigramTokenizer(search_q)
  product_title = UnigramTokenizer(title)
  search_len = length(search)
  match = 0
  jacc_min = vector()
  jaro_min = vector()
  cos_min = vector()
  for (j in 1:search_len){
    jacc_dist = lapply(product_title,function(x){stringdist(search[j],x,method="jaccard")})
    jaro_dist = lapply(product_title,function(x){stringdist(search[j],x,method="jw",p=0.1)})
    cos_dist = lapply(product_title,function(x){stringdist(search[j],x,method="cosine")})
    
    jacc_min[j] = min(unlist(jacc_dist))
    jacc_min[j] = ifelse(jacc_min[j] == Inf,str_length(str_trim(search[j])),jacc_min[j])
    jaro_min[j] = min(unlist(jaro_dist))
    jaro_min[j] = ifelse(jaro_min[j] == Inf,1,jaro_min[j])
    cos_min[j] = min(unlist(cos_dist))
    cos_min[j] = ifelse(cos_min[j] == Inf,1,cos_min[j])
  }
  
  jaro_match_ui = ifelse(length(jaro_min) == length(which(is.na(jaro_min))),0,(mean(jaro_min,na.rm=T)))
  jacc_match_ui = ifelse(length(jacc_min) == length(which(is.na(jacc_min))),0,(mean(jacc_min,na.rm=T)))
  cos_match_ui = ifelse(length(cos_min) == length(which(is.na(cos_min))),0,(mean(cos_min,na.rm=T)))  
  jaro_match_maxui = ifelse(length(jaro_min) == length(which(is.na(jaro_min))),0,(max(jaro_min,na.rm=T)))
  jacc_match_maxui = ifelse(length(jacc_min) == length(which(is.na(jacc_min))),0,(max(jacc_min,na.rm=T)))
  cos_match_maxui = ifelse(length(cos_min) == length(which(is.na(cos_min))),0,(max(cos_min,na.rm=T)))  
  
  if(search_len == 0){  
    jacc_match_ui = 1
    cos_match_ui = 1
    jaro_match_ui = 1
    jaro_match_maxui = 1
    jacc_match_maxui = 1
    cos_match_maxui = 1
  }
  return(as.data.frame(cbind(jaro_match_ui,jacc_match_ui,cos_match_ui,jaro_match_maxui,jacc_match_maxui,cos_match_maxui)))
}  

n_search_bi = pbsapply(n_search_term_stem,BigramTokenizer)

n_search_len_bi = pbsapply(n_search_bi,length)

n_search_uni = pbsapply(n_search_term_stem,UnigramTokenizer)

n_search_len_ui = pbsapply(n_search_uni,length)

uni_match_n = mapply(unigram_match_update,final_match[,"search"],final_match[,"title"])

bi_match_n = mapply(bigram_match_update,final_match[,"search"],final_match[,"title"])

uni_match_nt = t(uni_match_n)

rownames(uni_match_nt) = NULL

bi_match_nt = t(bi_match_n)

rownames(bi_match_nt) = NULL

colnames(uni_match_nt) = paste("noun", colnames(uni_match_nt),sep="_", collapse=NULL)

colnames(bi_match_nt) = paste("noun", colnames(bi_match_nt),sep="_", collapse=NULL)

full_data_fin = cbind(full_data_fin,tr_search_len)

test_data_fin = cbind(test_data_fin,te_search_len)

colnames(full_data_fin)[ncol(full_data_fin)] = "bigram_cnt"

colnames(test_data_fin)[ncol(test_data_fin)] = "bigram_cnt"

full_data_fin = cbind(full_data_fin,n_search_len_bi[1:74067])

test_data_fin = cbind(test_data_fin,n_search_len_bi[74068:240760])

full_data_fin = cbind(full_data_fin,n_search_len_ui[1:74067])

test_data_fin = cbind(test_data_fin,n_search_len_ui[74068:240760])

colnames(full_data_fin)[369:370] = c("bigram_cnt_noun","unigram_cnt_noun")

colnames(test_data_fin)[368:369] = c("bigram_cnt_noun","unigram_cnt_noun")

saveRDS(uni_match_nt,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/uni_match_nt.rds")

saveRDS(bi_match_nt,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/bi_match_nt.rds")

uni_match_nt1 = as.data.frame(uni_match_nt)

bi_match_nt1 = as.data.frame(bi_match_nt)

full_data_fin = cbind(full_data_fin,uni_match_nt1[1:74067,])

test_data_fin = cbind(test_data_fin,uni_match_nt1[74068:240760,])

full_data_fin = cbind(full_data_fin,bi_match_nt1[1:74067,])

test_data_fin = cbind(test_data_fin,bi_match_nt1[74068:240760,])

saveRDS(full_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final4.rds")

saveRDS(test_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final4.rds")

#################### read data again ##############

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final4.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final4.rds")

full_data_fin$min_cos_uni_bi = pmin(full_data_fin$cos_match,full_data_fin$cos_match_bi)

test_data_fin$min_cos_uni_bi = pmin(test_data_fin$cos_match,test_data_fin$cos_match_bi)

full_data_fin$min_jacc_uni_bi = pmin(full_data_fin$jacc_match,full_data_fin$jacc_match_bi)

test_data_fin$min_jacc_uni_bi = pmin(test_data_fin$jacc_match,test_data_fin$jacc_match_bi)

full_data_fin$min_jaro_uni_bi = pmin(full_data_fin$jaro_match,full_data_fin$jaro_match_bi)

test_data_fin$min_jaro_uni_bi = pmin(test_data_fin$jaro_match,test_data_fin$jaro_match_bi)

################# Query level again ################

full_data_fin_summ = full_data_fin %>% group_by(search_term_stem) %>% 
  summarise(max_cos_match = max(cos_match),
            max_jacc_match = max(jacc_match),
            max_jaro_match = max(jaro_match),
            max_jaro_match_bi = max(jaro_match_bi),
            max_jacc_match_bi = max(jacc_match_bi),
            max_jaro_match_bi = max(jaro_match_bi)) %>% 
  ungroup()

test_data_fin_summ = test_data_fin %>% group_by(search_term_stem) %>% 
  summarise(max_cos_match = max(cos_match),
            max_jacc_match = max(jacc_match),
            max_jaro_match = max(jaro_match),
            max_jaro_match_bi = max(jaro_match_bi),
            max_jacc_match_bi = max(jacc_match_bi),
            max_jaro_match_bi = max(jaro_match_bi)) %>% 
  ungroup()

full_data_fin = full_data_fin %>% inner_join(full_data_fin_summ,by="search_term_stem")

test_data_fin = test_data_fin %>% inner_join(test_data_fin_summ,by="search_term_stem")

for(i in c("noun_jaro_match_ui","noun_jaro_match_ui","noun_cos_match_ui","noun_jaro_match_bi",
           "noun_jacc_match_maxbi","noun_jacc_match_ui","noun_jaro_match_maxui","noun_jacc_match_bi",
           "noun_cos_match_maxbi","noun_jacc_match_maxui","noun_cos_match_bi","noun_cos_match_maxui",
           "noun_jaro_match_maxbi")){
  full_data_fin[,i] = as.numeric(full_data_fin[,i])
  test_data_fin[,i] = as.numeric(test_data_fin[,i])
  print(i)
}

saveRDS(full_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final5.rds")

saveRDS(test_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final5.rds")

##################alternative query##################

trigramTokenizer = function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))}

full = as.data.frame(rbind(full_data_fin[,-5],test_data_fin)[,c("search_term","product_title_stem")])

colnames(full) = c("search","title")

full[,"search"] = sapply(full[,"search"],function(x) {return(as.character(x))})
full[,"title"] = sapply(full[,"title"],function(x) {return(as.character(x))})

queries = unique(full[,1])

alt_queries = character()

i = 0

for (q in queries) {
  titles = full$title[full$search==q]
  if(length(str_split(titles," ")[[1]]) >= 3){
        grams = trigramTokenizer(titles)
        wd = as.data.frame(table(grams))
        wd = wd[order(-wd$Freq),]
        alt_queries = c(alt_queries, as.character(wd$grams[1]))
        i = i+1
        print(100*i/length(queries))
    } else {
      
      alt_queries = c(alt_queries, as.character(q))
      
    }
}

qmap = data.frame(queries, alt_queries)

full$row.seq = 1:nrow(full)

full_1 = merge(x=full, y=qmap, by.x="search", by.y="queries")

full_1 = full_1[order(full_1$row.seq),]

uni_match_n = mapply(unigram_match_update,full_1[,"alt_queries"],full_1[,"title"])

bi_match_n = mapply(bigram_match_update,full_1[,"alt_queries"],full_1[,"title"])

uni_match_nt = t(uni_match_n)

rownames(uni_match_nt) = NULL

bi_match_nt = t(bi_match_n)

rownames(bi_match_nt) = NULL

colnames(uni_match_nt) = paste("alt", colnames(uni_match_nt),sep="_", collapse=NULL)

colnames(bi_match_nt) = paste("alt", colnames(bi_match_nt),sep="_", collapse=NULL)

uni_match_nt1 = as.data.frame(uni_match_nt)

bi_match_nt1 = as.data.frame(bi_match_nt)

full_data_fin = cbind(full_data_fin,uni_match_nt1[1:74067,])

test_data_fin = cbind(test_data_fin,uni_match_nt1[74068:240760,])

full_data_fin = cbind(full_data_fin,bi_match_nt1[1:74067,])

test_data_fin = cbind(test_data_fin,bi_match_nt1[74068:240760,])

for(i in c("alt_jaro_match_ui","alt_jaro_match_ui","alt_cos_match_ui","alt_jaro_match_bi",
           "alt_jacc_match_maxbi","alt_jacc_match_ui","alt_jaro_match_maxui","alt_jacc_match_bi",
           "alt_cos_match_maxbi","alt_jacc_match_maxui","alt_cos_match_bi","alt_cos_match_maxui",
           "alt_jaro_match_maxbi")){
  full_data_fin[,i] = as.numeric(full_data_fin[,i])
  test_data_fin[,i] = as.numeric(test_data_fin[,i])
  print(i)
}

saveRDS(full_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final6.rds")

saveRDS(test_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final6.rds")

########### Remove numbers from product title and search and perform some matching again ########

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final6.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final6.rds")

full = as.data.frame(rbind(full_data_fin[,-5],test_data_fin)[,c("search_term_stem","product_title_stem")])

colnames(full) = c("search","title")

remove_num = function(x) {return(gsub("[0-9]","",x))}

single_ltr_rem = function(x) {return(gsub('\\b\\w{1}\\b','',x))}

full[,"search"] = gsub("[^a-z][0-9.]+[ /-]?(mm|ft|ml|oz|ounce|qt|cu ft|inch|ms|in|pk|ct|ea|pack|cup|pound|fl oz|floz|ghz|pc|gr|g|count|w|v|a|gb|tb)\\b", "", full[,"search"], ignore.case=T)

full[,"title"] = gsub("[^a-z][0-9.]+[ /-]?(mm|ft|ml|oz|ounce|qt|cu ft|inch|ms|in|pk|ct|ea|pack|cup|pound|fl oz|floz|ghz|pc|gr|g|count|w|v|a|gb|tb)\\b", "", full[,"title"], ignore.case=T)

full[,"search"] = sapply(full[,"search"], remove_num)
full[,"title"] = sapply(full[,"title"], remove_num)

full[,"search"] = sapply(full[,"search"], single_ltr_rem)
full[,"title"] = sapply(full[,"title"], single_ltr_rem)

full[,"search"] = gsub("\\s+", " ", str_trim(full[,"search"]))

full[,"title"] = gsub("\\s+", " ", str_trim(full[,"title"]))

full[,"search"] = sapply(full[,"search"],function(x) {return(as.character(x))})
full[,"title"] = sapply(full[,"title"],function(x) {return(as.character(x))})

uni_match_n = mapply(unigram_match_update,full[,"search"],full[,"title"])

bi_match_n = mapply(bigram_match_update,full[,"search"],full[,"title"])

uni_match_nt = t(uni_match_n)

rownames(uni_match_nt) = NULL

bi_match_nt = t(bi_match_n)

rownames(bi_match_nt) = NULL

colnames(uni_match_nt) = paste("no.num", colnames(uni_match_nt),sep="_", collapse=NULL)

colnames(bi_match_nt) = paste("no.num", colnames(bi_match_nt),sep="_", collapse=NULL)

uni_match_nt1 = as.data.frame(uni_match_nt)

bi_match_nt1 = as.data.frame(bi_match_nt)

full_data_fin = cbind(full_data_fin,uni_match_nt1[1:74067,])

test_data_fin = cbind(test_data_fin,uni_match_nt1[74068:240760,])

full_data_fin = cbind(full_data_fin,bi_match_nt1[1:74067,])

test_data_fin = cbind(test_data_fin,bi_match_nt1[74068:240760,])

for(i in c("no.num_jaro_match_ui","no.num_jaro_match_ui","no.num_cos_match_ui","no.num_jaro_match_bi",
           "no.num_jacc_match_maxbi","no.num_jacc_match_ui","no.num_jaro_match_maxui","no.num_jacc_match_bi",
           "no.num_cos_match_maxbi","no.num_jacc_match_maxui","no.num_cos_match_bi","no.num_cos_match_maxui",
           "no.num_jaro_match_maxbi")){
  full_data_fin[,i] = as.numeric(full_data_fin[,i])
  test_data_fin[,i] = as.numeric(test_data_fin[,i])
  print(i)
}

saveRDS(full_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final7.rds")

saveRDS(test_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final7.rds")

####################### Load again ##########################

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final7.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final7.rds")

trigramTokenizer = function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))}

full = as.data.frame(rbind(full_data_fin[,-5],test_data_fin)[,c("search_term","product_title_stem")])

colnames(full) = c("search","title")

remove_num = function(x) {return(gsub("[0-9]","",x))}

single_ltr_rem = function(x) {return(gsub('\\b\\w{1}\\b','',x))}

full[,"search"] = gsub("[^a-z][0-9.]+[ /-]?(mm|ft|ml|oz|ounce|qt|cu ft|inch|ms|in|pk|ct|ea|pack|cup|pound|fl oz|floz|ghz|pc|gr|g|count|w|v|a|gb|tb)\\b", "", full[,"search"], ignore.case=T)

full[,"title"] = gsub("[^a-z][0-9.]+[ /-]?(mm|ft|ml|oz|ounce|qt|cu ft|inch|ms|in|pk|ct|ea|pack|cup|pound|fl oz|floz|ghz|pc|gr|g|count|w|v|a|gb|tb)\\b", "", full[,"title"], ignore.case=T)

full[,"search"] = sapply(full[,"search"], remove_num)
full[,"title"] = sapply(full[,"title"], remove_num)

full[,"search"] = sapply(full[,"search"], single_ltr_rem)
full[,"title"] = sapply(full[,"title"], single_ltr_rem)

full[,"search"] = gsub("\\s+", " ", str_trim(full[,"search"]))

full[,"title"] = gsub("\\s+", " ", str_trim(full[,"title"]))

full[,"search"] = sapply(full[,"search"],function(x) {return(as.character(x))})
full[,"title"] = sapply(full[,"title"],function(x) {return(as.character(x))})

queries = unique(full[,1])

alt_queries = character()

i = 0

for (q in queries) {
  titles = full$title[full$search==q]
  t = str_split(titles," ")[[1]]
  if(length(str_trim(setdiff(t,""),side = c("both"))) >= 3){
    grams = trigramTokenizer(titles)
    wd = as.data.frame(table(grams))
    wd = wd[order(-wd$Freq),]
    alt_queries = c(alt_queries, as.character(wd$grams[1]))
    i = i+1
    print(100*i/length(queries))
  } else {
    
    alt_queries = c(alt_queries, as.character(q))
    
  }
}

qmap = data.frame(queries, alt_queries)

full$row.seq = 1:nrow(full)

full_1 = merge(x=full, y=qmap, by.x="search", by.y="queries")

full_1 = full_1[order(full_1$row.seq),]

uni_match_n = mapply(unigram_match_update,full_1[,"alt_queries"],full_1[,"title"])

bi_match_n = mapply(bigram_match_update,full_1[,"alt_queries"],full_1[,"title"])

uni_match_nt = t(uni_match_n)

rownames(uni_match_nt) = NULL

bi_match_nt = t(bi_match_n)

rownames(bi_match_nt) = NULL

colnames(uni_match_nt) = paste("alt.no.num", colnames(uni_match_nt),sep="_", collapse=NULL)

colnames(bi_match_nt) = paste("alt.no.num", colnames(bi_match_nt),sep="_", collapse=NULL)

uni_match_nt1 = as.data.frame(uni_match_nt)

bi_match_nt1 = as.data.frame(bi_match_nt)

full_data_fin = cbind(full_data_fin,uni_match_nt1[1:74067,])

test_data_fin = cbind(test_data_fin,uni_match_nt1[74068:240760,])

full_data_fin = cbind(full_data_fin,bi_match_nt1[1:74067,])

test_data_fin = cbind(test_data_fin,bi_match_nt1[74068:240760,])

for(i in c("alt.no.num_jaro_match_ui","alt.no.num_jaro_match_ui","alt.no.num_cos_match_ui",
           "alt.no.num_jaro_match_bi","alt.no.num_jacc_match_maxbi","alt.no.num_jacc_match_ui",
           "alt.no.num_jaro_match_maxui", "alt.no.num_jacc_match_bi", "alt.no.num_cos_match_maxbi",
           "alt.no.num_jacc_match_maxui","alt.no.num_cos_match_bi","alt.no.num_cos_match_maxui",
           "alt.no.num_jaro_match_maxbi")){
  full_data_fin[,i] = as.numeric(full_data_fin[,i])
  test_data_fin[,i] = as.numeric(test_data_fin[,i])
  print(i)
}

saveRDS(full_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final8.rds")

saveRDS(test_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final8.rds")

####################### more distances same data ######################

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final8.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final8.rds")

full = as.data.frame(rbind(full_data_fin[,-5],test_data_fin)[,c("search_term_stem","product_title_stem")])

colnames(full) = c("search","title")

remove_num = function(x) {return(gsub("[0-9]","",x))}

single_ltr_rem = function(x) {return(gsub('\\b\\w{1}\\b','',x))}

full[,"search"] = gsub("[^a-z][0-9.]+[ /-]?(mm|ft|ml|oz|ounce|qt|cu ft|inch|ms|in|pk|ct|ea|pack|cup|pound|fl oz|floz|ghz|pc|gr|g|count|w|v|a|gb|tb)\\b", "", full[,"search"], ignore.case=T)

full[,"title"] = gsub("[^a-z][0-9.]+[ /-]?(mm|ft|ml|oz|ounce|qt|cu ft|inch|ms|in|pk|ct|ea|pack|cup|pound|fl oz|floz|ghz|pc|gr|g|count|w|v|a|gb|tb)\\b", "", full[,"title"], ignore.case=T)

full[,"search"] = sapply(full[,"search"], remove_num)
full[,"title"] = sapply(full[,"title"], remove_num)

full[,"search"] = sapply(full[,"search"], single_ltr_rem)
full[,"title"] = sapply(full[,"title"], single_ltr_rem)

full[,"search"] = gsub("\\s+", " ", str_trim(full[,"search"]))

full[,"title"] = gsub("\\s+", " ", str_trim(full[,"title"]))

full[,"search"] = sapply(full[,"search"],function(x) {return(as.character(x))})
full[,"title"] = sapply(full[,"title"],function(x) {return(as.character(x))})

BigramTokenizer = function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}

bigram_match_update = function(search_q,title){
  search = BigramTokenizer(search_q)
  product_title = BigramTokenizer(title)
  search_len = length(search)
  match = 0
  lv_min = vector()
  osa_min = vector()
  dl_min = vector()
  lcs_min = vector()
  for (j in 1:search_len){
    lv_dist = lapply(product_title,function(x){stringdist(search[j],x,method="lv")})
    osa_dist = lapply(product_title,function(x){stringdist(search[j],x,method="osa")})
    dl_dist = lapply(product_title,function(x){stringdist(search[j],x,method="dl")})
    lcs_dist = lapply(product_title,function(x){stringdist(search[j],x,method="lcs")})
    
    lv_min[j] = min(unlist(lv_dist))
    lv_min[j] = ifelse(lv_min[j] == Inf,str_length(str_trim(search[j])),lv_min[j])
    osa_min[j] = min(unlist(osa_dist))
    osa_min[j] = ifelse(osa_min[j] == Inf,str_length(str_trim(search[j])),osa_min[j])
    dl_min[j] = min(unlist(dl_dist))
    dl_min[j] = ifelse(dl_min[j] == Inf,str_length(str_trim(search[j])),dl_min[j])
    lcs_min[j] = min(unlist(lcs_dist))
    lcs_min[j] = ifelse(lcs_min[j] == Inf,str_length(str_trim(search[j])),lcs_min[j])
  }
  
  lv_match_bi = ifelse(length(lv_min) == length(which(is.na(lv_min))),0,(mean(lv_min,na.rm=T)))
  osa_match_bi = ifelse(length(osa_min) == length(which(is.na(osa_min))),0,(mean(osa_min,na.rm=T)))
  dl_match_bi = ifelse(length(dl_min) == length(which(is.na(dl_min))),0,(mean(dl_min,na.rm=T)))  
  lcs_match_bi = ifelse(length(lcs_min) == length(which(is.na(lcs_min))),0,(mean(lcs_min,na.rm=T))) 
  lv_match_maxbi = ifelse(length(lv_min) == length(which(is.na(lv_min))),0,(max(lv_min,na.rm=T)))
  osa_match_maxbi = ifelse(length(osa_min) == length(which(is.na(osa_min))),0,(max(osa_min,na.rm=T)))
  dl_match_maxbi = ifelse(length(dl_min) == length(which(is.na(dl_min))),0,(max(dl_min,na.rm=T))) 
  lcs_match_maxbi = ifelse(length(lcs_min) == length(which(is.na(lcs_min))),0,(max(lcs_min,na.rm=T)))
  
  if(search_len == 0){  
    lv_match_bi = 99
    osa_match_bi = 99
    dl_match_bi = 99
    lcs_match_bi = 99
    lv_match_maxbi = 99
    osa_match_maxbi = 99
    dl_match_maxbi = 99
    lcs_match_maxbi = 99
  }
  return(as.data.frame(cbind(lv_match_bi,osa_match_bi,dl_match_bi,lcs_match_bi,lv_match_maxbi,osa_match_maxbi,
                             dl_match_maxbi,lcs_match_maxbi)))
}  

UnigramTokenizer = function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))}

unigram_match_update = function(search_q,title){
  search = UnigramTokenizer(search_q)
  product_title = UnigramTokenizer(title)
  search_len = length(search)
  match = 0
  lv_min = vector()
  osa_min = vector()
  dl_min = vector()
  lcs_min = vector()
  for (j in 1:search_len){
    lv_dist = lapply(product_title,function(x){stringdist(search[j],x,method="lv")})
    osa_dist = lapply(product_title,function(x){stringdist(search[j],x,method="osa")})
    dl_dist = lapply(product_title,function(x){stringdist(search[j],x,method="dl")})
    lcs_dist = lapply(product_title,function(x){stringdist(search[j],x,method="lcs")})
    
    lv_min[j] = min(unlist(lv_dist))
    lv_min[j] = ifelse(lv_min[j] == Inf,str_length(str_trim(search[j])),lv_min[j])
    osa_min[j] = min(unlist(osa_dist))
    osa_min[j] = ifelse(osa_min[j] == Inf,str_length(str_trim(search[j])),osa_min[j])
    dl_min[j] = min(unlist(dl_dist))
    dl_min[j] = ifelse(dl_min[j] == Inf,str_length(str_trim(search[j])),dl_min[j])
    lcs_min[j] = min(unlist(lcs_dist))
    lcs_min[j] = ifelse(lcs_min[j] == Inf,str_length(str_trim(search[j])),lcs_min[j])
  }
  
  lv_match_uni = ifelse(length(lv_min) == length(which(is.na(lv_min))),0,(mean(lv_min,na.rm=T)))
  osa_match_uni = ifelse(length(osa_min) == length(which(is.na(osa_min))),0,(mean(osa_min,na.rm=T)))
  dl_match_uni = ifelse(length(dl_min) == length(which(is.na(dl_min))),0,(mean(dl_min,na.rm=T)))  
  lcs_match_uni = ifelse(length(lcs_min) == length(which(is.na(lcs_min))),0,(mean(lcs_min,na.rm=T))) 
  lv_match_maxuni = ifelse(length(lv_min) == length(which(is.na(lv_min))),0,(max(lv_min,na.rm=T)))
  osa_match_maxuni = ifelse(length(osa_min) == length(which(is.na(osa_min))),0,(max(osa_min,na.rm=T)))
  dl_match_maxuni = ifelse(length(dl_min) == length(which(is.na(dl_min))),0,(max(dl_min,na.rm=T))) 
  lcs_match_maxuni = ifelse(length(lcs_min) == length(which(is.na(lcs_min))),0,(max(lcs_min,na.rm=T)))
  
  if(search_len == 0){  
    lv_match_uni = 99
    osa_match_uni = 99
    dl_match_uni = 99
    lcs_match_uni = 99
    lv_match_maxuni = 99
    osa_match_maxuni = 99
    dl_match_maxuni = 99
    lcs_match_maxuni = 99
  }
  return(as.data.frame(cbind(lv_match_uni,osa_match_uni,dl_match_uni,lcs_match_uni,lv_match_maxuni,osa_match_maxuni,
                             dl_match_maxuni,lcs_match_maxuni)))
}  

uni_match_n = mapply(unigram_match_update,full[,"search"],full[,"title"])

bi_match_n = mapply(bigram_match_update,full[,"search"],full[,"title"])

uni_match_nt = t(uni_match_n)

rownames(uni_match_nt) = NULL

bi_match_nt = t(bi_match_n)

rownames(bi_match_nt) = NULL

colnames(uni_match_nt) = paste("no.num", colnames(uni_match_nt),sep="_", collapse=NULL)

colnames(bi_match_nt) = paste("no.num", colnames(bi_match_nt),sep="_", collapse=NULL)

uni_match_nt1 = as.data.frame(uni_match_nt)

bi_match_nt1 = as.data.frame(bi_match_nt)

full_data_fin = cbind(full_data_fin,uni_match_nt1[1:74067,])

test_data_fin = cbind(test_data_fin,uni_match_nt1[74068:240760,])

full_data_fin = cbind(full_data_fin,bi_match_nt1[1:74067,])

test_data_fin = cbind(test_data_fin,bi_match_nt1[74068:240760,])

for(i in c("no.num_lv_match_uni","no.num_lv_match_uni","no.num_osa_match_uni","no.num_dl_match_uni",
           "no.num_dl_match_maxuni","no.num_dl_match_bi","no.num_dl_match_maxbi","no.num_lcs_match_uni",
           "no.num_lcs_match_maxuni","no.num_lcs_match_bi","no.num_lcs_match_maxbi","no.num_lv_match_maxuni",
           "no.num_lv_match_bi","no.num_lv_match_maxbi","no.num_osa_match_maxuni","no.num_osa_match_bi",
           "no.num_osa_match_maxbi")){
  full_data_fin[,i] = as.numeric(full_data_fin[,i])
  test_data_fin[,i] = as.numeric(test_data_fin[,i])
  print(i)
}

saveRDS(full_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final9.rds")

saveRDS(test_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final9.rds")

################### Brand string match other ###############

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final9.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final9.rds")

full = as.data.frame(rbind(full_data_fin[,-5],test_data_fin)[,c("search_term_stem","value_stem_rem")])

colnames(full) = c("search","brand")

remove_num = function(x) {return(gsub("[0-9]","",x))}

single_ltr_rem = function(x) {return(gsub('\\b\\w{1}\\b','',x))}

full[,"search"] = gsub("[^a-z][0-9.]+[ /-]?(mm|ft|ml|oz|ounce|qt|cu ft|inch|ms|in|pk|ct|ea|pack|cup|pound|fl oz|floz|ghz|pc|gr|g|count|w|v|a|gb|tb)\\b", "", full[,"search"], ignore.case=T)

full[,"brand"] = gsub("[^a-z][0-9.]+[ /-]?(mm|ft|ml|oz|ounce|qt|cu ft|inch|ms|in|pk|ct|ea|pack|cup|pound|fl oz|floz|ghz|pc|gr|g|count|w|v|a|gb|tb)\\b", "", full[,"brand"], ignore.case=T)

full[,"search"] = sapply(full[,"search"], remove_num)
full[,"brand"] = sapply(full[,"brand"], remove_num)

full[,"search"] = sapply(full[,"search"], single_ltr_rem)
full[,"brand"] = sapply(full[,"brand"], single_ltr_rem)

full[,"search"] = gsub("\\s+", " ", str_trim(full[,"search"]))

full[,"brand"] = gsub("\\s+", " ", str_trim(full[,"brand"]))

full[,"search"] = sapply(full[,"search"],function(x) {return(as.character(x))})
full[,"brand"] = sapply(full[,"brand"],function(x) {return(as.character(x))})

uni_match_n = mapply(unigram_match_update,full[,"brand"],full[,"search"])

uni_match_nt = t(uni_match_n)

rownames(uni_match_nt) = NULL

colnames(uni_match_nt) = paste("brand", colnames(uni_match_nt),sep="_", collapse=NULL)

uni_match_nt1 = as.data.frame(uni_match_nt)

full_data_fin = cbind(full_data_fin,uni_match_nt1[1:74067,])

test_data_fin = cbind(test_data_fin,uni_match_nt1[74068:240760,])

colnames(full_data_fin)[443:448] = c("brand_jaro_match_ui","brand_jacc_match_ui","brand_jacc_match_maxui",
                                     "brand_cos_match_ui","brand_cos_match_maxui","brand_jaro_match_maxui")

colnames(test_data_fin)[442:447] = c("brand_jaro_match_ui","brand_jacc_match_ui","brand_jacc_match_maxui",
                                     "brand_cos_match_ui","brand_cos_match_maxui","brand_jaro_match_maxui")

for(i in c("brand_jaro_match_ui","brand_jacc_match_ui","brand_jacc_match_maxui",
           "brand_cos_match_ui","brand_cos_match_maxui","brand_jaro_match_maxui")){
  full_data_fin[,i] = as.numeric(full_data_fin[,i])
  test_data_fin[,i] = as.numeric(test_data_fin[,i])
  print(i)
}

saveRDS(full_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final10.rds")

saveRDS(test_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final10.rds")

##################### Attribute level no num n gram similarity #############

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final10.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final10.rds")

full_data_fin_summ = full_data_fin %>% group_by(search_term) %>% 
  summarise(avgnoun_jaro_match_ui = mean(noun_jaro_match_ui),     
            minnoun_jaro_match_ui = min(noun_jaro_match_ui),  
            maxnoun_jaro_match_ui = max(noun_jaro_match_ui), 
            avgnoun_jacc_match_ui = mean(noun_jacc_match_ui),     
            minnoun_jacc_match_ui = min(noun_jacc_match_ui),  
            maxnoun_jacc_match_ui = max(noun_jacc_match_ui), 
            avgnoun_cos_match_ui = mean(noun_cos_match_ui),     
            minnoun_cos_match_ui = min(noun_cos_match_ui),  
            maxnoun_cos_match_ui = max(noun_cos_match_ui), 
            avgnoun_jaro_match_bi = mean(noun_jaro_match_bi),    
            minnoun_jaro_match_bi = min(noun_jaro_match_bi), 
            maxnoun_jaro_match_bi = max(noun_jaro_match_bi), 
            avgnoun_jacc_match_bi = mean(noun_jacc_match_bi),    
            minnoun_jacc_match_bi = min(noun_jacc_match_bi),  
            maxnoun_jacc_match_bi = max(noun_jacc_match_bi), 
            avgnoun_cos_match_bi = mean(noun_cos_match_bi),   
            minnoun_cos_match_bi = min(noun_cos_match_bi), 
            maxnoun_cos_match_bi = max(noun_cos_match_bi), 
            avgalt_jaro_match_ui = mean(alt_jaro_match_ui),   
            minalt_jaro_match_ui = min(alt_jaro_match_ui), 
            maxalt_jaro_match_ui = max(alt_jaro_match_ui), 
            avgalt_jacc_match_ui = mean(alt_jacc_match_ui),  
            minalt_jacc_match_ui = min(alt_jacc_match_ui), 
            maxalt_jacc_match_ui = max(alt_jacc_match_ui), 
            avgalt_cos_match_ui = mean(alt_cos_match_ui),   
            minalt_cos_match_ui = min(alt_cos_match_ui), 
            maxalt_cos_match_ui = max(alt_cos_match_ui), 
            avgalt_jaro_match_bi = mean(alt_jaro_match_bi),  
            minalt_jaro_match_bi = min(alt_jaro_match_bi), 
            maxalt_jaro_match_bi = max(alt_jaro_match_bi), 
            avgalt_jacc_match_bi = mean(alt_jacc_match_bi), 
            minalt_jacc_match_bi = min(alt_jacc_match_bi), 
            maxalt_jacc_match_bi = max(alt_jacc_match_bi), 
            avgalt_cos_match_bi = mean(alt_cos_match_bi),  
            minalt_cos_match_bi = min(alt_cos_match_bi), 
            maxalt_cos_match_bi = max(alt_cos_match_bi), 
            avgno.num_jaro_match_ui = mean(no.num_jaro_match_ui), 
            minno.num_jaro_match_ui = min(no.num_jaro_match_ui), 
            maxno.num_jaro_match_ui = max(no.num_jaro_match_ui), 
            avgno.num_jacc_match_ui = mean(no.num_jacc_match_ui), 
            minno.num_jacc_match_ui = min(no.num_jacc_match_ui), 
            maxno.num_jacc_match_ui = max(no.num_jacc_match_ui), 
            avgno.num_cos_match_ui = mean(no.num_cos_match_ui),   
            minno.num_cos_match_ui = min(no.num_cos_match_ui), 
            maxno.num_cos_match_ui = max(no.num_cos_match_ui), 
            avgno.num_jaro_match_bi = mean(no.num_jaro_match_bi), 
            minno.num_jaro_match_bi = min(no.num_jaro_match_bi), 
            maxno.num_jaro_match_bi = max(no.num_jaro_match_bi), 
            avgno.num_jacc_match_bi = mean(no.num_jacc_match_bi), 
            minno.num_jacc_match_bi = min(no.num_jacc_match_bi), 
            maxno.num_jacc_match_bi = max(no.num_jacc_match_bi), 
            avgno.num_cos_match_bi = mean(no.num_cos_match_bi),  
            minno.num_cos_match_bi = min(no.num_cos_match_bi), 
            maxno.num_cos_match_bi = max(no.num_cos_match_bi), 
            avgalt.no.num_jaro_match_ui = mean(alt.no.num_jaro_match_ui),  
            minalt.no.num_jaro_match_ui = min(alt.no.num_jaro_match_ui), 
            maxalt.no.num_jaro_match_ui = max(alt.no.num_jaro_match_ui), 
            avgalt.no.num_jacc_match_ui = mean(alt.no.num_jacc_match_ui), 
            minalt.no.num_jacc_match_ui = min(alt.no.num_jacc_match_ui), 
            maxalt.no.num_jacc_match_ui = max(alt.no.num_jacc_match_ui), 
            avgalt.no.num_cos_match_ui = mean(alt.no.num_cos_match_ui),   
            minalt.no.num_cos_match_ui = min(alt.no.num_cos_match_ui),
            maxalt.no.num_cos_match_ui = max(alt.no.num_cos_match_ui), 
            avgalt.no.num_jaro_match_bi = mean(alt.no.num_jaro_match_bi), 
            minalt.no.num_jaro_match_bi = min(alt.no.num_jaro_match_bi), 
            maxalt.no.num_jaro_match_bi = max(alt.no.num_jaro_match_bi), 
            avgalt.no.num_jacc_match_bi = mean(alt.no.num_jacc_match_bi), 
            minalt.no.num_jacc_match_bi = min(alt.no.num_jacc_match_bi), 
            maxalt.no.num_jacc_match_bi = max(alt.no.num_jacc_match_bi), 
            avgalt.no.num_cos_match_bi = mean(alt.no.num_cos_match_bi),    
            minalt.no.num_cos_match_bi = min(alt.no.num_cos_match_bi), 
            maxalt.no.num_cos_match_bi = max(alt.no.num_cos_match_bi), 
            avgno.num_lv_match_uni = mean(no.num_lv_match_uni),    
            minno.num_lv_match_uni = min(no.num_lv_match_uni), 
            maxno.num_lv_match_uni = max(no.num_lv_match_uni), 
            avgno.num_osa_match_uni = mean(no.num_osa_match_uni), 
            minno.num_osa_match_uni = min(no.num_osa_match_uni), 
            maxno.num_osa_match_uni = max(no.num_osa_match_uni), 
            avgno.num_dl_match_uni = mean(no.num_dl_match_uni),    
            minno.num_dl_match_uni = min(no.num_dl_match_uni),  
            maxno.num_dl_match_uni = max(no.num_dl_match_uni), 
            avgno.num_lcs_match_uni = mean(no.num_lcs_match_uni), 
            minno.num_lcs_match_uni = min(no.num_lcs_match_uni), 
            maxno.num_lcs_match_uni = max(no.num_lcs_match_uni), 
            avgno.num_lv_match_bi = mean(no.num_lv_match_bi),   
            minno.num_lv_match_bi = min(no.num_lv_match_bi),  
            maxno.num_lv_match_bi = max(no.num_lv_match_bi), 
            avgno.num_osa_match_bi = mean(no.num_osa_match_bi),  
            minno.num_osa_match_bi = min(no.num_osa_match_bi), 
            maxno.num_osa_match_bi = max(no.num_osa_match_bi), 
            avgno.num_dl_match_bi = mean(no.num_dl_match_bi),   
            minno.num_dl_match_bi = min(no.num_dl_match_bi), 
            maxno.num_dl_match_bi = max(no.num_dl_match_bi), 
            avgno.num_lcs_match_bi = mean(no.num_lcs_match_bi), 
            minno.num_lcs_match_bi = min(no.num_lcs_match_bi), 
            maxno.num_lcs_match_bi = max(no.num_lcs_match_bi)) %>% ungroup()

test_data_fin_summ = test_data_fin %>% group_by(search_term) %>% 
  summarise(avgnoun_jaro_match_ui = mean(noun_jaro_match_ui),     
            minnoun_jaro_match_ui = min(noun_jaro_match_ui),  
            maxnoun_jaro_match_ui = max(noun_jaro_match_ui), 
            avgnoun_jacc_match_ui = mean(noun_jacc_match_ui),     
            minnoun_jacc_match_ui = min(noun_jacc_match_ui),  
            maxnoun_jacc_match_ui = max(noun_jacc_match_ui), 
            avgnoun_cos_match_ui = mean(noun_cos_match_ui),     
            minnoun_cos_match_ui = min(noun_cos_match_ui),  
            maxnoun_cos_match_ui = max(noun_cos_match_ui), 
            avgnoun_jaro_match_bi = mean(noun_jaro_match_bi),    
            minnoun_jaro_match_bi = min(noun_jaro_match_bi), 
            maxnoun_jaro_match_bi = max(noun_jaro_match_bi), 
            avgnoun_jacc_match_bi = mean(noun_jacc_match_bi),    
            minnoun_jacc_match_bi = min(noun_jacc_match_bi),  
            maxnoun_jacc_match_bi = max(noun_jacc_match_bi), 
            avgnoun_cos_match_bi = mean(noun_cos_match_bi),   
            minnoun_cos_match_bi = min(noun_cos_match_bi), 
            maxnoun_cos_match_bi = max(noun_cos_match_bi), 
            avgalt_jaro_match_ui = mean(alt_jaro_match_ui),   
            minalt_jaro_match_ui = min(alt_jaro_match_ui), 
            maxalt_jaro_match_ui = max(alt_jaro_match_ui), 
            avgalt_jacc_match_ui = mean(alt_jacc_match_ui),  
            minalt_jacc_match_ui = min(alt_jacc_match_ui), 
            maxalt_jacc_match_ui = max(alt_jacc_match_ui), 
            avgalt_cos_match_ui = mean(alt_cos_match_ui),   
            minalt_cos_match_ui = min(alt_cos_match_ui), 
            maxalt_cos_match_ui = max(alt_cos_match_ui), 
            avgalt_jaro_match_bi = mean(alt_jaro_match_bi),  
            minalt_jaro_match_bi = min(alt_jaro_match_bi), 
            maxalt_jaro_match_bi = max(alt_jaro_match_bi), 
            avgalt_jacc_match_bi = mean(alt_jacc_match_bi), 
            minalt_jacc_match_bi = min(alt_jacc_match_bi), 
            maxalt_jacc_match_bi = max(alt_jacc_match_bi), 
            avgalt_cos_match_bi = mean(alt_cos_match_bi),  
            minalt_cos_match_bi = min(alt_cos_match_bi), 
            maxalt_cos_match_bi = max(alt_cos_match_bi), 
            avgno.num_jaro_match_ui = mean(no.num_jaro_match_ui), 
            minno.num_jaro_match_ui = min(no.num_jaro_match_ui), 
            maxno.num_jaro_match_ui = max(no.num_jaro_match_ui), 
            avgno.num_jacc_match_ui = mean(no.num_jacc_match_ui), 
            minno.num_jacc_match_ui = min(no.num_jacc_match_ui), 
            maxno.num_jacc_match_ui = max(no.num_jacc_match_ui), 
            avgno.num_cos_match_ui = mean(no.num_cos_match_ui),   
            minno.num_cos_match_ui = min(no.num_cos_match_ui), 
            maxno.num_cos_match_ui = max(no.num_cos_match_ui), 
            avgno.num_jaro_match_bi = mean(no.num_jaro_match_bi), 
            minno.num_jaro_match_bi = min(no.num_jaro_match_bi), 
            maxno.num_jaro_match_bi = max(no.num_jaro_match_bi), 
            avgno.num_jacc_match_bi = mean(no.num_jacc_match_bi), 
            minno.num_jacc_match_bi = min(no.num_jacc_match_bi), 
            maxno.num_jacc_match_bi = max(no.num_jacc_match_bi), 
            avgno.num_cos_match_bi = mean(no.num_cos_match_bi),  
            minno.num_cos_match_bi = min(no.num_cos_match_bi), 
            maxno.num_cos_match_bi = max(no.num_cos_match_bi), 
            avgalt.no.num_jaro_match_ui = mean(alt.no.num_jaro_match_ui),  
            minalt.no.num_jaro_match_ui = min(alt.no.num_jaro_match_ui), 
            maxalt.no.num_jaro_match_ui = max(alt.no.num_jaro_match_ui), 
            avgalt.no.num_jacc_match_ui = mean(alt.no.num_jacc_match_ui), 
            minalt.no.num_jacc_match_ui = min(alt.no.num_jacc_match_ui), 
            maxalt.no.num_jacc_match_ui = max(alt.no.num_jacc_match_ui), 
            avgalt.no.num_cos_match_ui = mean(alt.no.num_cos_match_ui),   
            minalt.no.num_cos_match_ui = min(alt.no.num_cos_match_ui),
            maxalt.no.num_cos_match_ui = max(alt.no.num_cos_match_ui), 
            avgalt.no.num_jaro_match_bi = mean(alt.no.num_jaro_match_bi), 
            minalt.no.num_jaro_match_bi = min(alt.no.num_jaro_match_bi), 
            maxalt.no.num_jaro_match_bi = max(alt.no.num_jaro_match_bi), 
            avgalt.no.num_jacc_match_bi = mean(alt.no.num_jacc_match_bi), 
            minalt.no.num_jacc_match_bi = min(alt.no.num_jacc_match_bi), 
            maxalt.no.num_jacc_match_bi = max(alt.no.num_jacc_match_bi), 
            avgalt.no.num_cos_match_bi = mean(alt.no.num_cos_match_bi),    
            minalt.no.num_cos_match_bi = min(alt.no.num_cos_match_bi), 
            maxalt.no.num_cos_match_bi = max(alt.no.num_cos_match_bi), 
            avgno.num_lv_match_uni = mean(no.num_lv_match_uni),    
            minno.num_lv_match_uni = min(no.num_lv_match_uni), 
            maxno.num_lv_match_uni = max(no.num_lv_match_uni), 
            avgno.num_osa_match_uni = mean(no.num_osa_match_uni), 
            minno.num_osa_match_uni = min(no.num_osa_match_uni), 
            maxno.num_osa_match_uni = max(no.num_osa_match_uni), 
            avgno.num_dl_match_uni = mean(no.num_dl_match_uni),    
            minno.num_dl_match_uni = min(no.num_dl_match_uni),  
            maxno.num_dl_match_uni = max(no.num_dl_match_uni), 
            avgno.num_lcs_match_uni = mean(no.num_lcs_match_uni), 
            minno.num_lcs_match_uni = min(no.num_lcs_match_uni), 
            maxno.num_lcs_match_uni = max(no.num_lcs_match_uni), 
            avgno.num_lv_match_bi = mean(no.num_lv_match_bi),   
            minno.num_lv_match_bi = min(no.num_lv_match_bi),  
            maxno.num_lv_match_bi = max(no.num_lv_match_bi), 
            avgno.num_osa_match_bi = mean(no.num_osa_match_bi),  
            minno.num_osa_match_bi = min(no.num_osa_match_bi), 
            maxno.num_osa_match_bi = max(no.num_osa_match_bi), 
            avgno.num_dl_match_bi = mean(no.num_dl_match_bi),   
            minno.num_dl_match_bi = min(no.num_dl_match_bi), 
            maxno.num_dl_match_bi = max(no.num_dl_match_bi), 
            avgno.num_lcs_match_bi = mean(no.num_lcs_match_bi), 
            minno.num_lcs_match_bi = min(no.num_lcs_match_bi), 
            maxno.num_lcs_match_bi = max(no.num_lcs_match_bi)) %>% ungroup()

full_data_fin = full_data_fin %>% inner_join(full_data_fin_summ,by="search_term")

test_data_fin = test_data_fin %>% inner_join(test_data_fin_summ,by="search_term")

saveRDS(full_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final11.rds")

saveRDS(test_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final11.rds")

#################### Read the data again #####################

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final11.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final11.rds")

trigramTokenizer = function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))}

full = as.data.frame(rbind(full_data_fin[,-5],test_data_fin)[,c("search_term","product_title_stem","desc_stem")])

colnames(full) = c("search","title","attr")

remove_num = function(x) {return(gsub("[0-9]","",x))}

single_ltr_rem = function(x) {return(gsub('\\b\\w{1}\\b','',x))}

full[,"search"] = gsub("[^a-z][0-9.]+[ /-]?(mm|ft|ml|oz|ounce|qt|cu ft|inch|ms|in|pk|ct|ea|pack|cup|pound|fl oz|floz|ghz|pc|gr|g|count|w|v|a|gb|tb)\\b", "", full[,"search"], ignore.case=T)

full[,"title"] = gsub("[^a-z][0-9.]+[ /-]?(mm|ft|ml|oz|ounce|qt|cu ft|inch|ms|in|pk|ct|ea|pack|cup|pound|fl oz|floz|ghz|pc|gr|g|count|w|v|a|gb|tb)\\b", "", full[,"title"], ignore.case=T)

full[,"attr"] = gsub("[^a-z][0-9.]+[ /-]?(mm|ft|ml|oz|ounce|qt|cu ft|inch|ms|in|pk|ct|ea|pack|cup|pound|fl oz|floz|ghz|pc|gr|g|count|w|v|a|gb|tb)\\b", "", full[,"attr"], ignore.case=T)

full[,"search"] = sapply(full[,"search"], remove_num)
full[,"title"] = sapply(full[,"title"], remove_num)
full[,"attr"] = sapply(full[,"attr"], remove_num)

full[,"search"] = sapply(full[,"search"], single_ltr_rem)
full[,"title"] = sapply(full[,"title"], single_ltr_rem)
full[,"attr"] = sapply(full[,"attr"], single_ltr_rem)


full[,"search"] = gsub("\\s+", " ", str_trim(full[,"search"]))

full[,"title"] = gsub("\\s+", " ", str_trim(full[,"title"]))

full[,"attr"] = gsub("\\s+", " ", str_trim(full[,"attr"]))

full[,"search"] = sapply(full[,"search"],function(x) {return(as.character(x))})
full[,"title"] = sapply(full[,"title"],function(x) {return(as.character(x))})
full[,"attr"] = sapply(full[,"attr"],function(x) {return(as.character(x))})

queries = unique(full[,1])

alt_queries = character()

i = 0

for (q in queries) {
  titles = full$title[full$search==q]
  t = str_split(titles," ")[[1]]
  if(length(str_trim(setdiff(t,""),side = c("both"))) >= 3){
    grams = trigramTokenizer(titles)
    wd = as.data.frame(table(grams))
    wd = wd[order(-wd$Freq),]
    alt_queries = c(alt_queries, as.character(wd$grams[1]))
    i = i+1
    print(100*i/length(queries))
  } else {
    
    alt_queries = c(alt_queries, as.character(q))
    
  }
}

qmap = data.frame(queries, alt_queries)

full$row.seq = 1:nrow(full)

full_1 = merge(x=full, y=qmap, by.x="search", by.y="queries")

full_1 = full_1[order(full_1$row.seq),]

uni_match_n = mapply(unigram_match_update,full_1[,"alt_queries"],full_1[,"attr"])

bi_match_n = mapply(bigram_match_update,full_1[,"alt_queries"],full_1[,"attr"])

uni_match_nt = t(uni_match_n)

rownames(uni_match_nt) = NULL

bi_match_nt = t(bi_match_n)

rownames(bi_match_nt) = NULL

colnames(uni_match_nt) = paste("alt.no.num_attr", colnames(uni_match_nt),sep="_", collapse=NULL)

colnames(bi_match_nt) = paste("alt.no.num_attr", colnames(bi_match_nt),sep="_", collapse=NULL)

uni_match_nt1 = as.data.frame(uni_match_nt)

bi_match_nt1 = as.data.frame(bi_match_nt)

full_data_fin = cbind(full_data_fin,uni_match_nt1[1:74067,])

test_data_fin = cbind(test_data_fin,uni_match_nt1[74068:240760,])

full_data_fin = cbind(full_data_fin,bi_match_nt1[1:74067,])

test_data_fin = cbind(test_data_fin,bi_match_nt1[74068:240760,])

for(i in c("alt.no.num_attr_jaro_match_ui","alt.no.num_attr_jaro_match_ui","alt.no.num_attr_cos_match_ui",
           "alt.no.num_attr_jaro_match_bi","alt.no.num_attr_jacc_match_maxbi","alt.no.num_attr_jacc_match_ui",
           "alt.no.num_attr_jaro_match_maxui", "alt.no.num_attr_jacc_match_bi", "alt.no.num_attr_cos_match_maxbi",
           "alt.no.num_attr_jacc_match_maxui","alt.no.num_attr_cos_match_bi","alt.no.num_attr_cos_match_maxui",
           "alt.no.num_attr_jaro_match_maxbi")){
  full_data_fin[,i] = as.numeric(full_data_fin[,i])
  test_data_fin[,i] = as.numeric(test_data_fin[,i])
  print(i)
}

saveRDS(full_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final12.rds")

saveRDS(test_data_fin,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final12.rds")

#################### Load data again ####################

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final12.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final12.rds")

full = as.data.frame(rbind(full_data_fin[,-5],test_data_fin)[,c("search_term","product_title_stem")])

colnames(full) = c("search","title")

remove_num = function(x) {return(gsub("[0-9]","",x))}

single_ltr_rem = function(x) {return(gsub('\\b\\w{1}\\b','',x))}

full[,"search"] = gsub("[^a-z][0-9.]+[ /-]?(mm|ft|ml|oz|ounce|qt|cu ft|inch|ms|in|pk|ct|ea|pack|cup|pound|fl oz|floz|ghz|pc|gr|g|count|w|v|a|gb|tb)\\b", "", full[,"search"], ignore.case=T)

full[,"title"] = gsub("[^a-z][0-9.]+[ /-]?(mm|ft|ml|oz|ounce|qt|cu ft|inch|ms|in|pk|ct|ea|pack|cup|pound|fl oz|floz|ghz|pc|gr|g|count|w|v|a|gb|tb)\\b", "", full[,"title"], ignore.case=T)

full[,"search"] = sapply(full[,"search"], remove_num)
full[,"title"] = sapply(full[,"title"], remove_num)

full[,"search"] = sapply(full[,"search"], single_ltr_rem)
full[,"title"] = sapply(full[,"title"], single_ltr_rem)

full[,"search"] = gsub("\\s+", " ", str_trim(full[,"search"]))

full[,"title"] = gsub("\\s+", " ", str_trim(full[,"title"]))

BigramTokenizer = function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}

bigram_match_update = function(search_q,title){
  search = BigramTokenizer(search_q)
  product_title = BigramTokenizer(title)
  search_len = length(search)
  match = 0
  jacc_min = vector()
  loc = vector()
  for (j in 1:search_len){
    jacc_dist = lapply(product_title,function(x){stringdist(search[j],x,method="jaccard")})
    
    loc[j] = which.min(unlist(jacc_dist))
  }
  
  location_title_bi = (sum(loc,na.rm=T)/length(loc[!is.na(loc)]))/length(product_title)
  
  if(search_len == 0){  
    location_title_bi = 0
  }
  return(location_title_bi)
}  

UnigramTokenizer = function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))}

unigram_match_update = function(search_q,title){
  search = UnigramTokenizer(search_q)
  product_title = UnigramTokenizer(title)
  search_len = length(search)
  match = 0
  jacc_min = vector()
  loc = vector()
  for (j in 1:search_len){
    jacc_dist = lapply(product_title,function(x){stringdist(search[j],x,method="jaccard")})
    
    loc[j] = which.min(unlist(jacc_dist))
  }
  
  location_title_ui = (sum(loc,na.rm=T)/length(loc[!is.na(loc)]))/length(product_title)
  
  if(search_len == 0){  
    location_title_ui = 0
  }
  return(location_title_ui)
}  

uni_match_n = mapply(unigram_match_update,full[,"search"],full[,"title"])

bi_match_n = mapply(bigram_match_update,full[,"search"],full[,"title"])

saveRDS(uni_match_n,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/uni_match_n.rds")

saveRDS(bi_match_n,"/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/bi_match_n.rds")

###################################################################################

uni_match_n = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/uni_match_n.rds")

bi_match_n = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/bi_match_n.rds")

rownames(uni_match_n) = NULL

rownames(bi_match_n) = NULL

uni_match_nt1 = as.data.frame(uni_match_n)

bi_match_nt1 = as.data.frame(bi_match_n)

colnames(uni_match_nt1) = paste("no.num", colnames(uni_match_nt1),sep="_", collapse=NULL)

colnames(bi_match_nt1) = paste("no.num", colnames(bi_match_nt1),sep="_", collapse=NULL)

full_data_fin = cbind(full_data_fin,uni_match_nt1[1:74067,])

test_data_fin = cbind(test_data_fin,uni_match_nt1[74068:240760,])

full_data_fin = cbind(full_data_fin,bi_match_nt1[1:74067,])

test_data_fin = cbind(test_data_fin,bi_match_nt1[74068:240760,])

colnames(full_data_fin)[551:552] = c("location_uni","location_bi")
colnames(test_data_fin)[550:551] = c("location_uni","location_bi")

full_data_fin[which(is.na(full_data_fin$location_bi)),"location_bi"] = 0
full_data_fin[which(is.na(full_data_fin$location_uni)),"location_uni"] = 0

test_data_fin[which(is.na(test_data_fin$location_bi)),"location_bi"] = 0
test_data_fin[which(is.na(test_data_fin$location_uni)),"location_uni"] = 0

full_data_fin[,"ratio_noun_norm_jacc"] = full_data_fin[,"jacc_match"]/(full_data_fin[,"noun_jacc_match_ui"]+1)

test_data_fin[,"ratio_noun_norm_jacc"] = test_data_fin[,"jacc_match"]/(test_data_fin[,"noun_jacc_match_ui"]+1)

full_data_fin = saveRDS(full_data_fin, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final13.rds")

test_data_fin = saveRDS(test_data_fin, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final13.rds")

########################

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final13.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final13.rds")

full_data_fin_summ = full_data_fin %>% group_by(search_term_stem) %>% 
  summarise(avgalt.no.num_attr_jaro_match_ui = mean(alt.no.num_attr_jaro_match_ui),     
            minalt.no.num_attr_jaro_match_ui = min(alt.no.num_attr_jaro_match_ui),  
            maxalt.no.num_attr_jaro_match_ui = max(alt.no.num_attr_jaro_match_ui), 
            avgalt.no.num_attr_jacc_match_ui = mean(alt.no.num_attr_jacc_match_ui),    
            minalt.no.num_attr_jacc_match_ui = min(alt.no.num_attr_jacc_match_ui),  
            maxalt.no.num_attr_jacc_match_ui = max(alt.no.num_attr_jacc_match_ui), 
            avgalt.no.num_attr_cos_match_ui = mean(alt.no.num_attr_cos_match_ui),     
            minalt.no.num_attr_cos_match_ui = min(alt.no.num_attr_cos_match_ui),  
            maxalt.no.num_attr_cos_match_ui = max(alt.no.num_attr_cos_match_ui), 
            avgalt.no.num_attr_cos_match_maxui = mean(alt.no.num_attr_cos_match_maxui),     
            minalt.no.num_attr_cos_match_maxui = min(alt.no.num_attr_cos_match_maxui),  
            maxalt.no.num_attr_cos_match_maxui = max(alt.no.num_attr_cos_match_maxui), 
            avgalt.no.num_attr_cos_match_bi = mean(alt.no.num_attr_cos_match_bi),    
            minalt.no.num_attr_cos_match_bi = min(alt.no.num_attr_cos_match_bi),  
            maxalt.no.num_attr_cos_match_bi = max(alt.no.num_attr_cos_match_bi), 
            avgalt.no.num_attr_cos_match_maxbi = mean(alt.no.num_attr_cos_match_maxbi),     
            minalt.no.num_attr_cos_match_maxbi = min(alt.no.num_attr_cos_match_maxbi),  
            maxalt.no.num_attr_cos_match_maxbi = max(alt.no.num_attr_cos_match_maxbi), 
            avgratio_noun_norm_jacc = mean(ratio_noun_norm_jacc),     
            minratio_noun_norm_jacc = min(ratio_noun_norm_jacc),  
            maxratio_noun_norm_jacc = max(ratio_noun_norm_jacc), 
            avgalt.no.num_attr_jaro_match_maxui = mean(alt.no.num_attr_jaro_match_maxui),    
            minalt.no.num_attr_jaro_match_maxui = min(alt.no.num_attr_jaro_match_maxui), 
            maxalt.no.num_attr_jaro_match_maxui = max(alt.no.num_attr_jaro_match_maxui), 
            avgalt.no.num_attr_jaro_match_bi = mean(alt.no.num_attr_jaro_match_bi),    
            minalt.no.num_attr_jaro_match_bi = min(alt.no.num_attr_jaro_match_bi),  
            maxalt.no.num_attr_jaro_match_bi = max(alt.no.num_attr_jaro_match_bi), 
            avgalt.no.num_attr_jaro_match_maxbi = mean(alt.no.num_attr_jaro_match_maxbi),    
            minalt.no.num_attr_jaro_match_maxbi = min(alt.no.num_attr_jaro_match_maxbi),  
            maxalt.no.num_attr_jaro_match_maxbi = max(alt.no.num_attr_jaro_match_maxbi), 
            avglocation_uni = mean(location_uni),     minlocation_uni = min(location_uni), 
            maxlocation_uni = max(location_uni), 
            avgalt.no.num_attr_jacc_match_maxui = mean(alt.no.num_attr_jacc_match_maxui),   
            minalt.no.num_attr_jacc_match_maxui = min(alt.no.num_attr_jacc_match_maxui),  
            maxalt.no.num_attr_jacc_match_maxui = max(alt.no.num_attr_jacc_match_maxui), 
            avgalt.no.num_attr_jacc_match_bi = mean(alt.no.num_attr_jacc_match_bi),   
            minalt.no.num_attr_jacc_match_bi = min(alt.no.num_attr_jacc_match_bi),
            maxalt.no.num_attr_jacc_match_bi = max(alt.no.num_attr_jacc_match_bi), 
            avgalt.no.num_attr_jacc_match_maxbi = mean(alt.no.num_attr_jacc_match_maxbi),  
            minalt.no.num_attr_jacc_match_maxbi = min(alt.no.num_attr_jacc_match_maxbi), 
            maxalt.no.num_attr_jacc_match_maxbi = max(alt.no.num_attr_jacc_match_maxbi), 
            avglocation_bi = mean(location_bi),    
            minlocation_bi = min(location_bi), 
            maxlocation_bi = max(location_bi)) %>% 
  ungroup()

test_data_fin_summ = test_data_fin %>% group_by(search_term_stem) %>% 
  summarise(avgalt.no.num_attr_jaro_match_ui = mean(alt.no.num_attr_jaro_match_ui),     
            minalt.no.num_attr_jaro_match_ui = min(alt.no.num_attr_jaro_match_ui),  
            maxalt.no.num_attr_jaro_match_ui = max(alt.no.num_attr_jaro_match_ui), 
            avgalt.no.num_attr_jacc_match_ui = mean(alt.no.num_attr_jacc_match_ui),    
            minalt.no.num_attr_jacc_match_ui = min(alt.no.num_attr_jacc_match_ui),  
            maxalt.no.num_attr_jacc_match_ui = max(alt.no.num_attr_jacc_match_ui), 
            avgalt.no.num_attr_cos_match_ui = mean(alt.no.num_attr_cos_match_ui),     
            minalt.no.num_attr_cos_match_ui = min(alt.no.num_attr_cos_match_ui),  
            maxalt.no.num_attr_cos_match_ui = max(alt.no.num_attr_cos_match_ui), 
            avgalt.no.num_attr_cos_match_maxui = mean(alt.no.num_attr_cos_match_maxui),     
            minalt.no.num_attr_cos_match_maxui = min(alt.no.num_attr_cos_match_maxui),  
            maxalt.no.num_attr_cos_match_maxui = max(alt.no.num_attr_cos_match_maxui), 
            avgalt.no.num_attr_cos_match_bi = mean(alt.no.num_attr_cos_match_bi),    
            minalt.no.num_attr_cos_match_bi = min(alt.no.num_attr_cos_match_bi),  
            maxalt.no.num_attr_cos_match_bi = max(alt.no.num_attr_cos_match_bi), 
            avgalt.no.num_attr_cos_match_maxbi = mean(alt.no.num_attr_cos_match_maxbi),     
            minalt.no.num_attr_cos_match_maxbi = min(alt.no.num_attr_cos_match_maxbi),  
            maxalt.no.num_attr_cos_match_maxbi = max(alt.no.num_attr_cos_match_maxbi), 
            avgratio_noun_norm_jacc = mean(ratio_noun_norm_jacc),     
            minratio_noun_norm_jacc = min(ratio_noun_norm_jacc),  
            maxratio_noun_norm_jacc = max(ratio_noun_norm_jacc), 
            avgalt.no.num_attr_jaro_match_maxui = mean(alt.no.num_attr_jaro_match_maxui),    
            minalt.no.num_attr_jaro_match_maxui = min(alt.no.num_attr_jaro_match_maxui), 
            maxalt.no.num_attr_jaro_match_maxui = max(alt.no.num_attr_jaro_match_maxui), 
            avgalt.no.num_attr_jaro_match_bi = mean(alt.no.num_attr_jaro_match_bi),    
            minalt.no.num_attr_jaro_match_bi = min(alt.no.num_attr_jaro_match_bi),  
            maxalt.no.num_attr_jaro_match_bi = max(alt.no.num_attr_jaro_match_bi), 
            avgalt.no.num_attr_jaro_match_maxbi = mean(alt.no.num_attr_jaro_match_maxbi),    
            minalt.no.num_attr_jaro_match_maxbi = min(alt.no.num_attr_jaro_match_maxbi),  
            maxalt.no.num_attr_jaro_match_maxbi = max(alt.no.num_attr_jaro_match_maxbi), 
            avglocation_uni = mean(location_uni),     minlocation_uni = min(location_uni), 
            maxlocation_uni = max(location_uni), 
            avgalt.no.num_attr_jacc_match_maxui = mean(alt.no.num_attr_jacc_match_maxui),   
            minalt.no.num_attr_jacc_match_maxui = min(alt.no.num_attr_jacc_match_maxui),  
            maxalt.no.num_attr_jacc_match_maxui = max(alt.no.num_attr_jacc_match_maxui), 
            avgalt.no.num_attr_jacc_match_bi = mean(alt.no.num_attr_jacc_match_bi),   
            minalt.no.num_attr_jacc_match_bi = min(alt.no.num_attr_jacc_match_bi),
            maxalt.no.num_attr_jacc_match_bi = max(alt.no.num_attr_jacc_match_bi), 
            avgalt.no.num_attr_jacc_match_maxbi = mean(alt.no.num_attr_jacc_match_maxbi),  
            minalt.no.num_attr_jacc_match_maxbi = min(alt.no.num_attr_jacc_match_maxbi), 
            maxalt.no.num_attr_jacc_match_maxbi = max(alt.no.num_attr_jacc_match_maxbi), 
            avglocation_bi = mean(location_bi),    
            minlocation_bi = min(location_bi), 
            maxlocation_bi = max(location_bi)) %>% 
  ungroup()

full_data_fin = full_data_fin %>% inner_join(full_data_fin_summ,by="search_term_stem")

test_data_fin = test_data_fin %>% inner_join(test_data_fin_summ,by="search_term_stem")

full_data_fin$sqrt_osa_match = sqrt(full_data_fin$osa_match)

test_data_fin$sqrt_osa_match = sqrt(test_data_fin$osa_match)

full_data_fin$sqrt_jacc_match_attr = sqrt(full_data_fin$jacc_match_attr)

test_data_fin$sqrt_jacc_match_attr = sqrt(test_data_fin$jacc_match_attr)

full_data_fin$sqrt_jaro_match_attr = sqrt(full_data_fin$jaro_match_attr)

test_data_fin$sqrt_jaro_match_attr = sqrt(test_data_fin$jaro_match_attr)

full_data_fin$sqr_match_percent = (full_data_fin$match_percent)^2

test_data_fin$sqr_match_percent = (test_data_fin$match_percent)^2

full_data_fin$sqr_match_percent_desc = (full_data_fin$match_percent_desc)^2

test_data_fin$sqr_match_percent_desc = (test_data_fin$match_percent_desc)^2

full_data_fin$sqrt_cos_match_bi = sqrt(full_data_fin$cos_match_bi)

test_data_fin$sqrt_cos_match_bi = sqrt(test_data_fin$cos_match_bi)

full_data_fin$sqrt_jaro_match_bi = sqrt(full_data_fin$jaro_match_bi)

test_data_fin$sqrt_jaro_match_bi = sqrt(test_data_fin$jaro_match_bi)

full_data_fin$sqrt_jacc_match_bi = sqrt(full_data_fin$jacc_match_bi)

test_data_fin$sqrt_jacc_match_bi = sqrt(test_data_fin$jacc_match_bi)

full_data_fin$sqrt_noun_jaro_match_ui = sqrt(full_data_fin$noun_jaro_match_ui)

test_data_fin$sqrt_noun_jaro_match_ui = sqrt(test_data_fin$noun_jaro_match_ui)

full_data_fin$sqrt_min_jaro_uni_bi = sqrt(full_data_fin$min_jaro_uni_bi)

test_data_fin$sqrt_min_jaro_uni_bi = sqrt(test_data_fin$min_jaro_uni_bi)

full_data_fin$alt.no.num_attr_jacc_match_ui4 = (full_data_fin$alt.no.num_attr_jacc_match_ui)^4

test_data_fin$alt.no.num_attr_jacc_match_ui4 = (test_data_fin$alt.no.num_attr_jacc_match_ui)^4

full_data_fin$sqrt_cos_ratio = sqrt(full_data_fin$cos_ratio)

test_data_fin$sqrt_cos_ratio = sqrt(test_data_fin$cos_ratio)

full_data_fin$sqrt_jaro_ratio = sqrt(full_data_fin$jaro_ratio)

test_data_fin$sqrt_jaro_ratio = sqrt(test_data_fin$jaro_ratio)

full_data_fin$sqrt_jacc_ratio = sqrt(full_data_fin$jacc_ratio)

test_data_fin$sqrt_jacc_ratio = sqrt(test_data_fin$jacc_ratio)

full_data_fin$sqrt_no.num_lv_match_uni = sqrt(full_data_fin$no.num_lv_match_uni)

test_data_fin$sqrt_no.num_lv_match_uni = sqrt(test_data_fin$no.num_lv_match_uni)

full_data_fin$sqrt_no.num_osa_match_uni = sqrt(full_data_fin$no.num_osa_match_uni)

test_data_fin$sqrt_no.num_osa_match_uni = sqrt(test_data_fin$no.num_osa_match_uni)

full_data_fin$sqrt_no.num_lv_match_uni = sqrt(full_data_fin$no.num_lv_match_uni)

test_data_fin$sqrt_no.num_lv_match_uni = sqrt(test_data_fin$no.num_lv_match_uni)

full_data_fin$sqrt_no.num_lcs_match_uni = sqrt(full_data_fin$no.num_lcs_match_uni)

test_data_fin$sqrt_no.num_lcs_match_uni = sqrt(test_data_fin$no.num_lcs_match_uni)

full_data_fin$sqrt_no.num_dl_match_uni = sqrt(full_data_fin$no.num_dl_match_uni)

test_data_fin$sqrt_no.num_dl_match_uni = sqrt(test_data_fin$no.num_dl_match_uni)

full_data_fin$no.num_lcs_match_bi_5pow = (full_data_fin$no.num_lcs_match_bi)^.2

test_data_fin$no.num_lcs_match_bi_5pow = (test_data_fin$no.num_lcs_match_bi)^.2

full_data_fin$no.num_osa_match_bi_5pow = (full_data_fin$no.num_osa_match_bi)^.2

test_data_fin$no.num_osa_match_bi_5pow = (test_data_fin$no.num_osa_match_bi)^.2

full_data_fin$sqrt_ratlen_search_brand = sqrt(full_data_fin$ratlen_search_brand)

test_data_fin$sqrt_ratlen_search_brand = sqrt(test_data_fin$ratlen_search_brand)

full_data_fin$sqrt_qgram_match = sqrt(full_data_fin$qgram_match)

test_data_fin$sqrt_qgram_match = sqrt(test_data_fin$qgram_match)

full_data_fin$sqrt_no.num_lv_match_bi = sqrt(full_data_fin$no.num_lv_match_bi)

test_data_fin$sqrt_no.num_lv_match_bi = sqrt(test_data_fin$no.num_lv_match_bi)

full_data_fin$sqrt_no.num_osa_match_bi = sqrt(full_data_fin$no.num_osa_match_bi)

test_data_fin$sqrt_no.num_osa_match_bi = sqrt(test_data_fin$no.num_osa_match_bi)

saveRDS(full_data_fin, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final14.rds")

saveRDS(test_data_fin, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final14.rds")

####################

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final_last1.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final_last1.rds")

########################

mat_name = attr_data %>% filter(name == "Material")

rm(attr_data)

mat_name$value = sapply(mat_name$value,str_to_lower)

desc1 = tm_map(Corpus(VectorSource(mat_name$value)),removeWords, stopwords('english'))
desc1 = tm_map(desc1,replacePunctuation)

mat_name$value_stem = as.character(tm_map(desc1,stemDocument))

rm(desc1)

mat_name = mat_name %>% mutate(value_stem = gsub("  "," ",value_stem))

UnigramTokenizer = function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))}

unigram_match_update = function(search_q,title){
  search = UnigramTokenizer(search_q)
  product_title = UnigramTokenizer(title)
  search_len = length(search)
  match = 0
  jacc_min = vector()
  jaro_min = vector()
  cos_min = vector()
  for (j in 1:search_len){
    jacc_dist = lapply(product_title,function(x){stringdist(search[j],x,method="jaccard")})
    jaro_dist = lapply(product_title,function(x){stringdist(search[j],x,method="jw",p=0.1)})
    cos_dist = lapply(product_title,function(x){stringdist(search[j],x,method="cosine")})
    
    jacc_min[j] = min(unlist(jacc_dist))
    jacc_min[j] = ifelse(jacc_min[j] == Inf,str_length(str_trim(search[j])),jacc_min[j])
    jaro_min[j] = min(unlist(jaro_dist))
    jaro_min[j] = ifelse(jaro_min[j] == Inf,1,jaro_min[j])
    cos_min[j] = min(unlist(cos_dist))
    cos_min[j] = ifelse(cos_min[j] == Inf,1,cos_min[j])
  }
  
  jaro_match_ui = ifelse(length(jaro_min) == length(which(is.na(jaro_min))),0,(mean(jaro_min,na.rm=T)))
  jacc_match_ui = ifelse(length(jacc_min) == length(which(is.na(jacc_min))),0,(mean(jacc_min,na.rm=T)))
  cos_match_ui = ifelse(length(cos_min) == length(which(is.na(cos_min))),0,(mean(cos_min,na.rm=T)))  
  
  if(search_len == 0){  
    jacc_match_ui = 1
    cos_match_ui = 1
    jaro_match_ui = 1
  }
  return(as.data.frame(cbind(jaro_match_ui,jacc_match_ui,cos_match_ui)))
}  

mat_name = mat_name[,c(1,4)]

colnames(mat_name)[2] = "material" 

mat_name = mat_name %>% group_by(product_uid) %>% summarise(material = paste(material,collapse = " "))

full_data_fin = full_data_fin %>% left_join(mat_name,by="product_uid")
test_data_fin = test_data_fin %>% left_join(mat_name,by="product_uid")

train_mat = mapply(unigram_match_update,full_data_fin[,"material"],full_data_fin[,"search_term_stem"])

test_mat = mapply(unigram_match_update,test_data_fin[,"material"],test_data_fin[,"search_term_stem"])

saveRDS(train_mat, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_mat.rds")

saveRDS(test_mat, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_mat.rds")

train_mat = as.data.frame(t(train_mat))
test_mat = as.data.frame(t(test_mat))

colnames(train_mat) = c("jaro_mat","jacc_mat","cos_mat")
colnames(test_mat) = c("jaro_mat","jacc_mat","cos_mat")

full_data_fin = cbind(full_data_fin,train_mat)
test_data_fin = cbind(test_data_fin,test_mat)

mat_name = attr_data %>% filter(str_to_upper(name) == "COLOR FAMILY")

rm(attr_data)

mat_name$value = sapply(mat_name$value,str_to_lower)

desc1 = tm_map(Corpus(VectorSource(mat_name$value)),removeWords, stopwords('english'))
desc1 = tm_map(desc1,replacePunctuation)

mat_name$value_stem = as.character(tm_map(desc1,stemDocument))

rm(desc1)

mat_name = mat_name %>% mutate(value_stem = gsub("  "," ",value_stem))

mat_name = mat_name[,c(1,4)]

colnames(mat_name)[2] = "color" 

mat_name = mat_name %>% group_by(product_uid) %>% summarise(color = paste(color,collapse = " "))

full_data_fin = full_data_fin %>% left_join(mat_name,by="product_uid")
test_data_fin = test_data_fin %>% left_join(mat_name,by="product_uid")

train_mat = mapply(unigram_match_update,full_data_fin[,"color"],full_data_fin[,"search_term_stem"])

test_mat = mapply(unigram_match_update,test_data_fin[,"color"],test_data_fin[,"search_term_stem"])

saveRDS(train_mat, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_col.rds")

saveRDS(test_mat, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_col.rds")

train_mat = as.data.frame(t(train_mat))
test_mat = as.data.frame(t(test_mat))

colnames(train_mat) = c("jaro_col","jacc_col","cos_col")
colnames(test_mat) = c("jaro_col","jacc_col","cos_col")

full_data_fin = cbind(full_data_fin,train_mat)
test_data_fin = cbind(test_data_fin,test_mat)

full_data_fin$color = NULL
test_data_fin$color = NULL

full_data_fin$material = NULL
test_data_fin$material = NULL

for(i in c("jaro_col","jacc_col","cos_col","jaro_mat","jacc_mat","cos_mat")){
  full_data_fin[,i] = as.numeric(full_data_fin[,i])
  test_data_fin[,i] = as.numeric(test_data_fin[,i])
  print(i)
}

saveRDS(full_data_fin, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final_last2.rds")

saveRDS(test_data_fin, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final_last2.rds")

full_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final_last2.rds")

test_data_fin = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final_last2.rds")

###############Remove correlated vars

for(i in c("sqrt_jaro_ratio","jaro_match","sqrt_min_jaro_uni_bi","sd_rel_med","sqr_match_percent_desc",
"srt_jacc","ratio_noun_norm_jacc","jacc_match","cos_match","match_percent","sqrt_cos_ratio","min_jaro_uni_bi",
"cos_col","cos_mat","jacc_col","jaro_mat","mean_rel_high","min_cos_uni_bi","jaro_match_bi","jacc_match_bi")){
  full_data_fin[,i]=NULL
  test_data_fin[,i]=NULL
}

saveRDS(full_data_fin, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final_last3.rds")

saveRDS(test_data_fin, "/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final_last3.rds")

##############

full_data = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/train_data_final_last3.rds")

test_data = readRDS("/mapr/projects/GLRM_test/HomeDepot/HomeDepot2/Home_3/test_data_final_last3.rds")

