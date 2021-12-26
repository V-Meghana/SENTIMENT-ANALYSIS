#attach the required packages

packages <- c("tidyverse","caret","data.table","dslabs","dplyr","stringr","tidyr","kableExtra","knitr","knitLatex",
              "readxl","readr","tm","wordcloud","tidytext","e1071","gmodels","reshape2","RTextTools" ,"crosstable" ,
              "SnowballC","RColorBrewer")

lapply(packages, FUN = function(X) {
  do.call("require", list(X)) 
})

# set working directory
setwd("C:/Users/MAG/Documents/CYO")

# import the file containing reviews
dat <- read_csv("5000 REVIEWS.csv")

# pre-processing
# removing unwanted columns
# we want to retain the original. so we create a duplicate that is cleaned.
# we remove brand name (among others) since they've all been bought on Amazon
new_dat <- select(dat, -c(id, asins, categories, dateAdded, dateUpdated, brand, imageURLs, keys, 
                          manufacturer, manufacturerNumber, reviews.date, 
                          reviews.dateAdded, reviews.dateSeen, reviews.id, reviews.numHelpful, reviews.rating,
                          reviews.sourceURLs, reviews.username, reviews.title, sourceURLs, primaryCategories ))

write.csv(new_dat, file = "Reviews.csv")

# partially cleansed data
head(new_dat)

# product with more number of reviews
# 'Amazon - Echo Plus w/ Built-In Hub - Silver' has 590 reviews. so it was chosen among all products
dat_final <- new_dat %>% filter(name == "Amazon - Echo Plus w/ Built-In Hub - Silver") 

# extracting the reviews
para <- dat_final %>% group_by(name, reviews.text) %>% 
  summarise_all(funs(trimws(paste(., collapse = ''))))

para1 <- para %>% select(reviews.text)

write.csv(para1, file = "Reviews2 - Copy.csv") # this was used to create 'text'(below) manually

# creating a vector with all the reviews (590 reviews)  << excuse the clutter :) >>
# this is done so that we can create tokens.
text <- c("A fun little device on the cutting edge of consumer artificial intelligence Speaker sounds-",
          "pretty good Alexa does have some problems understanding how I ask questions a fair -",
          "amount of the time A great value and I love it sounds great and works A must have Everyone-", 
          "should get one A perfect gift for someone who has everything and a busy schedule A wonderful-",
          "use of creativity and design where installation was a breeze and use was pure enjoyment Added-",
          "Alexa hub and expanded the lighting capabilities Easy setup and it works like a charm Looking-",
          "for more devices to add to the mix Added another echo to the family This one came with a free-",
          "Philips HUE bulb on black friday which was great Alexa a great product the light too They are-",
          "working fine Alexa Exceed expectations in it ability to control Alexa in itself needs lots of-",
          "improvement but with philips hue and smart plugs you can have a lot of fun settings Alexa Is-",
          "amazin I love the features their great Just love it Alexa is amazing and the Phillips bulb-",
          "works seamlessly with it Alexa is awesome to have at home makes things fun and easy Alexa is-",
          "easily confused and fun to mess with but also informative when we are being serious She-",
          "disappoints when she loses connection with the wifi but its not often We have six different-",
          "Alexa products and after a few hiccups all are working well Impressed Alexa is fun to play-",
          "with for the whole family and it fills the house with music when not telling us a joke Alexa-",
          "is the greatest device to have around the house She basically runs my house for me Alexa items-",
          "are easy to use and set up Works easily with the smart plug and bulb Alexa makes my life easy-",
          "by turning my lights on and off playing my favorite music and so much more It really can -",
          "control anything Alexa really amazes me She learns new skills everyday Alexa ricks the plus is-",
          "the best ever buy it you will love it Alexa rocks The Echo Plus is super easy to set up I works-",
          "great Love everything about it Adding more devices Alexa turns on the lights and when I say Alexa-",
          "good night she turns them off Love her Alexa usually works well but it has turned on things when I-",
          "did not tell it to Alexa walks you through every step which I found helpful Alexa works pretty -",
          "good music sounds good Just exploring all her skills Alexis is the new favorite name Great for the-",
          "college student Already have the echo and dot Has good sound quality and with the hub feature you -",
          "can control the hue lights with Alexa which is convenient when walking in the house with a bunch-",
          "of groceries Amazon and Philips Hue work great together very easy setup The only drawback is -",
          "using these bulbs on a non Philips dimmer switch The bulbs flicker a bit even when its set to -",
          "full on and practical easy to use just the start of my IOT home As a newbie in smart home -",
          "technology I was attracted to the Echo Plus because of the builtin Zigbee hub which means it -",
          "can connect directly to products like the Philips Hue line of smart bulbs without the need for -",
          "a separate smart hub Combine that with the power of Alexa voice commands and I was able to get -",
          "my smart home up and running in a matter of minutes As advertised Easy set up and integration -",
          "Havent tried to use the hub yet but it is limited in the number of products it works with -",
          "compared to Samsung Smartthing or Wink two As I made my home a smart home I know I know I made a-",
          "smart choice with Amazon Echo and Philips light bulbs Im looking forward to adding more smart -",
          "gadgets to my collection Asking trivia questions of Alexa is a hoot but being able to turn off -",
          "the ceiling fan from bed is golden Awesome love them had to buy two more for the family Awesome -",
          "product and cool design and a great color I would recommend to a friend awesome product my wife -",
          "and I love it makes home automation simple Best Buy associate explained product very well Easy -",
          "to use Bought two as Cristmas presents for my nephews Both ages had them up and running within -",
          "minutes Had fun asking questions and adjusting the Philips light bulb to different brightness -",
          "levels All in all it was a hit Bought four devices for my kids they report its simple to start-",
          "up easy to use and fun Bought an echo plus and it has been better than I expected Primarily use -",
          "it for music in the LR but we are starting to use it for other functions such as the weather -",
          "and playing jeopardy Bought as a Christmas gift Works good with no problems Bought eh Echo Plus -",
          "on a whim it was on sale and I figured why not I am really enjoying it Apart form the basic -",
          "news and weather reporting I enjoy my morning trivia I also use the music from Amazon a lot Its-", 
          "been nice having a  holiday  station at my disposal The Echo Plus is easy to set up and use and -",
          "definitely has a lot of features many of which I am still learning Bought for a family memeber-",
          "and he was so happy with the game Bought for mother she loves it Good price in stock and an easy-",
          "purchase Bought for my husband for Christmas We keep it in our kitchen and mainly just use it -",
          "for music But our kids love that they can tell Alexa to play a song they like and it plays -",
          "Bought one for Living Room and the other for the bedroom Home wifi needs to be boosted but -",
          "Alexa would connect through out which caused complications along with Bluetooth speakers Once -",
          "straightened out works great Bought the Amazon Echo Plus Philips Hue Bulb Black for my kitchen -",
          "it is awesome also came with free smart bulb bought this as a present for the wife but as I -",
          "relaxed for the holiday requested six hours of different song play The wife enjoyed the music -",
          "as the music emanated through the open areas of our house The wife received information on -",
          "measures for recipes weather updates etc this will be one of those classic gifts Bought this-",
          "Echo for my wife She loves how just by speaking she can listen to any type of music It sounds -",
          "pretty good too Bought this for my mom that is electronically challenged and she loves it Makes -",
          "her feel like she is a tech wiz Makes me feel better about her ability to turn lights on and -",
          "off as she comes and goes More importantly she is in her and Iives alone she can make a phone-",
          "call using Alexa from anywhere she has an echo I bought her three to cover her whole house -",
          "This means if she falls down and cant get to her phone she can dial me with her voice a-",
          "must have Bought this for my parents home and its awesome The quality of the sound is -",
          "amazing Its also helpful to get quick questions answered when they are running around or -",
          "out of the house I need to now purchase one for my own apartment Also great when hosting-",
          "guests Bought this for my parents to use with their Amazon prime account Sure they will -",
          "love it Bought this for my son and one for my husband and me So easy to set up Very helpful-",
          "Doesn ttake up a lot of space Bought this for our son for Christmas He uses it in his room-",
          "and loves it Alexa also wakes him up for school in the morning Builtin hub only lets your -",
          "control Philips Hue with your voice You still need the Philips Hue hub to be able to use -",
          "the Hue app I ended up buying the Hue hub as well Has better speaker than the regular Echo -",
          "Can not ell you the price of merchandise when asked Christmas gift for my son and my-",
          "husband Can twait to hook it up Decent sounding music device easy to connect and use the-", 
          "app Getting custom routines set up with smart home vivint hue etc takes some trial and -",
          "error figuring out commands Would buy again Decided to go for the model just below this -",
          "one I didnt need the integrated  hub  features Phillips Hue does require this but I -",
          "decided to stick with my current Insteon hub Definitely recommend to anyone that wants a-",
          "exciting product Definitely worth the money The sound is awesome Easy hook up great for -",
          "lights and my Sony tv I use all the time Easy set up and very user friendly for my children-",
          "Easy setup They are adding new commands every week it seems Still has some work till it is-",
          "fully functional Easy to set up and pair other compatible items by enabling skills through-",
          "the Amazon app Ive been to best buy twice for information and how to questions and all -",
          "seems to be going well Its only been a week and still have a lot to learn about Elexas -",
          "capabilities Easy to set up easy to use apps are awesome and sounds great Easy to set up -",
          "Performs well Nice Product For The Money Easy to set up We really enjoy getting real time -",
          "local weather It translates Spanish very efficiently We also enjoy the music we can easily-",
          "select but would appreciate a higher quality sound Linking the Echo to lighting is a snap -",
          "Highly recommended Easy to use no Complains at all Added all the smart plugs to the skills-",
          "and works perfect Easy to use Should have bought this a long time ago Echo is nice but it-",
          "could be better allowing you to connect to anything that is on your WIFI network without-",
          "having to buy several different hubs Echo Plus has everything the old Echo had plus better -",
          "sound and builtin capability for Smart Home addons like Hue bulbs and Smart Things plugs-",
          "without needing a Hue hub or ST hub Love it Echo plus is able to play music and response -",
          "whatever answer by itself It also equip with bluetooth so I can pair to another more powerful-", 
          "speaker Its so convenient to ask request without touch anything The response time is acceptable-",
          "but you need to speak out your question right after calling Alex or it wont wait Echo Plus is -",
          "everything I expected and more Plus easy to set up Echo plus is great for home use to ask -",
          "questions and turn the lights on when you away ECHO PLUS OFFERS SO MUCH MORE FOR THE SMART HOME-", 
          "HOWEVER I HAVE HAD THE DOT FOR A LONG TIME AND YESTERDAY I BOUGHT 5 ECHO DOTS FOR MY CHILDREN-",
          "GREAT PRODUCT Echo plus seems to work as they say I have it running two lights so far Plan on -",
          "setting up seven more Nice product Eco plus is an amazing product it integrates with most of -",
          "the products U can set the music reminders buy products what not definitely a worthy purchase -",
          "Enjoying using Alexa and love the smart home features Excellent combination and easy to install -",
          "and setup Excellent item above and beyond my expectations Excellent product Turns on and off -",
          "supported devices by asking it Live walking into a room and asking Alexa to turn on a light -",
          "Poof its done Or locking and unlocking the front door Arming and disarming my security system -",
          "If youre looking to turn your house into a smarthome I strongly recommend the Amazon Echo Plus-",
          "Exchanged the google Google home for this model Fun for the kids Finding new uses for it every -",
          "day Had a problem when best buy forgot to include the free light bulb Extra features don treally -",
          "seem worth it I would just buy the basic one instead First time user of the Alexa plus Using it -",
          "more and more every day Love it Friend received Amazon Echo as gift & is enjoying it I researched -",
          "decided on Echo Plus as I wanted capability to use to control home lighting Absolutely love this -",
          "product It controls lights in several rooms as well as outdoor lighting Other features weather news -",
          "briefs traffic are fun but the lighting control did it for me From her jokes rapping and everyday-",
          "new fact Alexa is awesome Its super easy to pull out your phone to check the weather but i honestly -",
          "never did and would always be surprised when i walked outside with whatever it is that day But-",
          "every morning i ask alexa whats the weather and she gives me an in depth review for that day and -",
          "the rest of the week There are tons of skills for Alexa most useful some random But shes a must -",
          "try Also plus about the echo plus is if you want to start a smart home you dont need a hub like -",
          "the other echo pieces You can just buy some compatible smart bulbs Fun gadget Plays songs various -",
          "entertainment still learning all it can do Fun item and helpful to do things around the home Gave -",
          "it as a gift to provide a sense of safety and security for someone living in the country Turning-",
          "the lights on before entering the house is a big deal when you live alone Good product and wasy to-",
          "set up Works well with smart lights Good stereo sound but will not control some devices without-",
          "the use of an external hub Got it as a Christmas gift and I love it I would recommend this product-",
          "Got one for my daughter last year and she loves it Got this as a present for my fiance for Christmas-",
          "will update Got this for my Dad for Christmas Awesome gift he was super excited Got this model -",
          "only because of the sale price and the built in hub that controls my hue strip lights And I did nnot -",
          "twant to say google all the time Sounds like baby talk to me Great addition to all the other -",
          "electronics in our home Great and easy even for someone that is not tech savvy Great and fun -",
          "almost like having someone in the room with you Great Christmas gift The hubby loved it Just -",
          "read the manual before setting up Great device but I noticed based on the WiFi connection -",
          "that Alexa can react slower at times Especially when asking the device to turn off bulbs-",
          "or any smart devices Great device Good speaker very solid feeling Microphones listen well-",
          "Great Echo Friend really liked it It really does what he wants it to do and he connected -",
          "his lights to the device Everything works well Great entertainment when friends are over -",
          "The knowledge and jokes are tons of fun great for the elderly makes life a lot easier for -",
          "everyone Great gifts I have the Alexa dot in each room now this is better with the speaker-",
          "base ask Alexa to play music find me recipes read the news Got this & few other dot-",
          "devices to give for Xmas Great improved sound and built in smart hub is so useful Great-", 
          "item and easy to use Very helpful and entertaining Great item for controling your lights -",
          "in my home even when Im away Great item we love our Echo If only she could mow the yard-",
          "for the kids Great little speaker I use it every day and I have it about for three days-",
          "already GREAT PRICE FOR HOLIDAYS COMBO WORKS WELL NO NEED FOR EXTERNAL HUB Great price -",
          "large selection Plenty to choose from Great product easy to use and would recommend to -",
          "anyone Great product answers all your questions and controls compatible devices Great -",
          "product easier set up than anticipated Actually bought for my Mother she loves it Great -",
          "product Didnt realize its compatible with different items Great product I use it everyday -",
          "Its a great way to automate your home Great product Loving alexa Was so happy with it that-",
          "know its a big part of all that i do Great product Works as advertised and fun to use -",
          "Great product Works well and is even easy for kids to use Great purchase We bought this -",
          "to make it easier to turn off lights etc Its perfect for connecting the whole house great -",
          "sound easy to use wife love it connect with wifi and all my phones Great sound and -",
          "function Alexa The color and style of the silver plus looks great with our newly finished-",
          "basement Great sound and voice recognition easy to set up and fun to use Great sound for -",
          "the money Easy to set up also have a dot that I like Great sound quality Easy to set up-",
          "Easy to use Looks good too great sound Love the music and the electrical devices you can-",
          "work with it Great way to introduce family and friends to A I Technology Had the Echo Plus-",
          "over a week and this thing is amazing Great sound and easy to move around A little tricky -",
          "with the set up but works great Hard to review as I purchased for my son as a present -",
          "Has built in smart hub unlike other echos Works with other smart home devices with no -",
          "additional purchases Have first gen Echo and Dot and a second gen Dot Wanted this over-",
          "regular Echo because I prefer the look and better speakers Have just started using Alexa-",
          "and am looking forward to all the other things that are available to use with the unit-",
          "Have yet to explore all the aspects Enjoy using it daily Having a great experience using-",
          "the Alexa Echo plus product having an assistant like Echo makes life so much easier -",
          "Having fun with all the things we can learn from Alexa It is almost like having another-", 
          "person in the house HUB is almost useless Ended up using spare Philips HUB The HUB built-",
          "in would only discover two out of twelve bulbs three days of trying HUE bridge twelve out -",
          "of twelve first try I am not a techie but after days of trying to hook up I gave up I -",
          "dont know if it was the Echo Plus or my carrier ATT Uverse causing the issue but it would-", 
          "NOT connect So back to the store it went Even with checking out blogs on the internet for -",
          "assistance none of it worked Ive had an Amazon Echo and Dot for over a year now I really -",
          "wanted to develop the Smart Device usage around my house and when the upgraded Echo i -",
          "just had to have it It is a great device I use the drop in feature to communicate with -",
          "others in the house especially when they are upstairs without having to yell The smart -",
          "hub is amazing Attaching to smart devices is much more seamless than with the first -",
          "gen echo Just got it so time will tell but the second gen Echo devices are worth it I -",
          "absolutely love Alexa This device is loud easy to install and does exactly what I tell her-",
          "to do I absolutely love this product It does so many things not the least of which is-",
          "music Ask it anything and it will answer.The Dot worked fine so I returned the full-",
          "featured Echo Plus I bought this for controlling most of my Smart device-",
          "I bought this for my fourteen year old daughter and she loves it I bought this for my brother -",
          "and his wife They love it Not only does it control their Nest thermostat it works with their-",
          "expanding environment of Philips Hue bulbs I bought this for my finance i bought it for Christmas-",
          "he left bed it I bought this for my mom and dad they loved it Thank you I bought this for -",
          "my Mom for Christmas and ended up she got me one too Great for everyone speaker sounds great easy-",
          "to set up and my Mom ended up getting the dot so she could have her light turned on from downstairs-",
          "and then tell Alexa to turn it off while in bed No regrets on this one I bought this for my moms-",
          "birthday and she loves it I bought this for my wife and absolutely loves it Alexa is so smart and -",
          "helps my wife with everything Like a personal assistant I bought this for myself and another one for-",
          "a gift you must have WiFi to use as well as a smart phone I brought the Echo Plus for the louder sound quality-" ,
          "quality I already have an Echo Dot which I use in my bedroom to control my lights and heater via a Wemo Smart-" ,
          "Switch I brought the Echo Plus which I use in my family room for music and to control my smart devices I didnt-" , 
          "know what I was missing out on until I got an Echo Plus I enjoy Alexa she reminds me of appointment and alerts-" ,
          "me when my packages arrive I enjoy my new smart speaker It is a good device that I can get my pandora radio on -" ,
          "it especially in the app Learning what the device offers for smart home besides that It is good speaker to work -" ,
          "with different apps I enjoy the amazon Alexa It is an lot of fun It will be more helpful once I have completely -" ,
          "set up I enjoy the premium sound with this Amazon Echo Plus speaker Of course it also connects to Alexa so I can -" ,
          "do everything from making calls to listening to music and finding recipes for cooking meals is compatible with -" ,
          "smart home technology for handsfree control of lighting and other functions I like the Amazon Echo Plus speaker -" ,
          "because it adapts to my speech patterns I enjoyed and I bought an echo too To make all my family take the pleasure-" ,
          "of it I gave as a gift to my kids for Christmas they are enjoying Alexa I gave the Echo five stars but I like -" ,
          "Amazon products and ultimately the product is good however I use Cree smart light bulbs in my house and bought -" ,
          "this Echo for the built in Hub to try and eliminate the extra hub space in my house but the built in hub of the -" ,
          "Echo Plus isn tcompatible at this time with Cree light bulbs so I had to return it and wait for now and stick -" ,
          "with my regular Echo and Echo Dots I got this for my husband He likes it and the setup is easy I had three dots -" ,
          "and decided to get the echo plus Great product so far Controls my hue lights Grandkids love asking it questions -" ,
          "I had my doubts when I first purchased it But after just a day I had synced it with my thermostat lights TV Bose -" ,
          "sound system and many more Amazing product couldn tbe happier I have an Amazon Prime account so I started -" ,
          "downloading apps and playing all kinds of music on this I also can control my Hue hub you need one for certain-" , 
          "timers and things and Wemo plus outlets for my non wifi lights and LEDs in the kitchen cabinets I have an Echo -" ,
          "that I bought from Best Buy several months ago So when it came to buying one for my son & his family for Christmas-" ,
          "I knew where to come Its an excellent information hub I have an original Gen one Echo that has been relocated to-" ,
          "My Bedroom by simply unplugging and Plugging back in Now that I have the Gen two Echo Plus in my Living Room and -" ,
          "set up to work seamlessly with my Gen one I could not be happier Along with the built in Hue it makes it that-" ,
          "much better Everything works just like I want and the sound on both devices are perfect for me I have been -" ,
          "contemplating purchasing an Echo product for some time now I finally did it and I am so pleased I did It was -" ,
          "extremely easy to set up I add my lights to the process and now it is so easy to turn lights on before I get -" ,
          "to the room For me I know this will be the tip of the iceberg I will now start looking at what else I can add -" ,
          "to the Echo Love the product Great Buy I have been greatly impressed with the Alexa A friend loves calling me -" ,
          "and asking her Alexa questions We share alot of laughs just discovering all the things she can do So I just had-" ,
          "to purchase an Alexa for myself Im just beginning set my Alexa up I hope its easy because Im not tech salve -" ,
          "Ive got to go and get started with the setup I have both a first gen Echo and now the new Echo Plus I am-" ,
          "impressed with the sound quality improvement especially the bass Dont get me wrong its no Bose system but -" ,
          "it sounds great for every day use I have enjoyed learning about home automation using Echo plus and consumers-" ,
          "need to know that this comes with having to buy additional equipment to get the most out of Alexa I have had -" ,
          "the competitive products and nothing compares I have had this for around a month and i love it It makes it -" ,
          "simple to set timers and control my lights I have no idea how my family ever lived without these products-" ,
          "Since purchasing the Alexa Hub and Hue bulb weve purchased Echo Dots for our kitchen and sons room We absolutely-" ,
          "love the convenience of the bulbs and Alexa is a life saver on so many levels I have not had a chance to use -" ,
          "the HUB function on this device but it was easy to set up and get running still learning how to make it work-" ,
          "with the other echo devices but overall it operates as advertised i have really enjoyed learning about the-" ,
          "product and its many uses i thought initially music but now to get weather updates news receipies sports scores-" ,
          "and ordering kitchen supplies i have really enjoyed my new echo plus i enjoy music every morning with the news -" ,
          "I have several Amazon Echos throughout my house Home automation is great and these work with verything that I -" ,
          "have I have several home automation devices and the Echo Plus controls all of them I was so happy with automation-" ,
          "I bought an Echo Dot and Echo Tap I have the Echo plus and i love it the speakers sounds loud and clear I have -" ,
          "the original Echo and just purchase the Echo Plus Better speakers and can act as a hub for the Philips Hue Bulbs-" ,
          "without having to purchase another hub Very satisfied with its performance I have two dots and this and the -" ,
          "functionality between them is awesome I can twait to see how it improves through time Great quality speaker and-" ,
          "build I know this makes us lazy But comes in handy when your hands are tied up with groceries or holding a baby-" ,
          "Love that my kids ask her questions I like being able to ask about the weather set an alarm on my phone or create-" ,
          "a shopping list all by just asking Elexia I like being able to turn on lights music and the thermostat in our -" ,
          "home I like it for controlling my lights I like that we can play anyones music playlist I like the looks of the -" ,
          "Echo Plus and the sound from the speakers is something I didn texpect to be as nice as it is Im using it to tune-" ,
          "into my favorite radio station answer any question I have to look up on the internet and assist me with lighting-" ,
          "I am considering another for a different room I like this smart machine and will recommend to my friends I looked-" ,
          "at Google Home And the Echo I went with the Echo after I spoke to a Best Buy employee They helped me sort out the-" ,
          "pros and cons I do love all the skills that are possible Its like another person in my apt sometimes I live alone-" ,
          "the Echo adjusts my lights and plays my Pandora Im pretty much loving it I love Alexa although sometimes she has -" ,
          "a hard time understanding so I find myself repeating commands such as Alexa turn off lights in bedroom Overall -" ,
          "shes amazing and she tells jokes and very up to date with current events I LOVE Alexa and my Echo device and -" ,
          "Smart Bulb so much I bought four more Echos and eight more bulbs with more to come soon I love Alexa Great sound-" ,
          "unlimited music used the bulb and synchronized it Simple to use love Alexa Has a pretty good speaker for -" ,
          "listening toMusic and helps with my shopping lists I love Alexa She has become my best friend Thanks I love all-" ,
          "of the things you can do with the Echo I am still learning everything I can do and look forward to more content -" ,
          "going forward I love amazon products easy to use and install I recommend them hundred very good sound and above -" ,
          "all price I love being able to turn my lights on or off by voice It makes life a lot easier I am looking forward-" ,
          "to finish making my home a smart home by buying other products I love being able to use Alexa She is an amazing -" ,
          "AI Until the robots rebel against the human kind I think its worth it I love having Alexa in my home Alexa has -" ,
          "helped from recipes to relaxing sleep sounds all night I love how Alexa can control almost all smart things -" ,
          "and help control lights and power on and off devices to help us get ready for the day The music is connected -" ,
          "directly to Amazon Prime Music and I love how the sound is clear and crisp You can call out to Alexa from far-" ,
          "away and shell add items to your shopping list and the Alexa app is so simple to view the lists and control your-" ,
          "smart things I love it the sound is great I use it everyday for my dogs to listen to when I am at work I love my -" ,
          "amazon echo plus as bad as it sounds I don thave to get out of bed to turn the lights off my fans on or off and -" ,
          "even my tv We love alexa I love my Amazon Echo Alexa is compatible with all of my technology and super easy to -" ,
          "use My favorite is the ease of making a grocery list and having it in the app instead of forgetting my list at -" ,
          "home I love my Echo Plus The hub makes it easy to add devices to my Smarthome and Alexa is a great addition I -" ,
          "love my echo so easy to hook up and great sound quality I love my Echo so much I have my lights and front door-" ,
          "deadbolt hooked up to it I play music from it all the time and ask Alexa about the traffic and weather I would-" ,
          "recommend the Echo to everyone I love our Amazon echo plus My boyfriend says this was the best present for -" ,
          "Christmas daily I love that I have the ability to catch up on news and have this crazy thing turn on and off my-" ,
          "lights Love it I love the Alexa It has encouraged me to automate my home Now I have lighting that comes on when-" ,
          "I ask My heat is controlled wherever I am and its fun asking her Alexa questions I love this product and it was-" ,
          "one of my Best Buys from best buy I love this product and plan to expand my Alexa and Echo network I love this-" ,
          "product just wish it was as smart as the Google home I love this product would really recommend to any one I love -" ,
          "this product I love everything about it I love this Has great sound I thought i bought in silver THe packaging was-" ,
          "a little deceptive But Im loving it Wanting to get a second one I loved my echo so much I bought my sister one I-" ,
          "can now turn on my lights when I get home just by asking Alexa I m so happy to get this amazon echo plus Great to-" ,
          "have it I mainly bought the Echo plus for my kids but Im using it more than they do Excellent product I need to-" ,
          "buy all the additional parts but its great I never thought that buying this product was such a great step up-" ,
          "Really makes watching a movie and turning on the TV so easy really works well with my harmony hub Love the way I-" ,
          "can control all the lights Looking forward to all the add ons I can purchase I originally purchased this to use -" ,
          "as a Bluetooth speaker for my wifes music but soon realized that we would use this much more than we thought Ya-" ,
          "turning on and off lights is good and being able to dim with the included bulb is pretty great but there are-" ,
          "hundreds of smart home type of apps that work with this including my Sony Google TV and more show up regularly-" ,
          "Makes it almost too easy to be lazy I purchase one for my children and us is a great way to stay connected and -" ,
          "smart to have it at home I purchase the Echo Plus as a Christmas gift but set it up early so that it could be -" ,
          "used right out of the box I was impressed how easy the setup was Ill find out later how it sounds and works I -" ,
          "purchase this at the store and I was not given any bulb till I decide to do review I purchased it to control-" ,
          "my lights & introduce myself to Alexa and AI but I find myself using it for all sorts of things and enjoy using -" ,
          "it I can now control my front door Arlo camera by asking Alexa to show the live feed in to my TV and get my-" ,
          "Robots vacuum cleaner to start work plus listen to the latest weather conditions and do my normal google search-" , 
          "via Alexa I am seriously hooked in to my Alexa -")

# removing unnecessary objects
rm(new_dat, dat, para, para1, text)

# converting it into tibble format
text_df <- tibble(text = text)

# it looks like this
head(text_df)

# tokenizing
text_df <- text_df %>%
  unnest_tokens(word, text)

# it looks like this
view(text_df)

#-------------------------------------------------------------------------------

# Model 1 : Lexicon based approach 

# the tidytext package comprises of sentiment lexicons that are present in the dataset
#The bing lexicon model classifies the sentiment into a binary category of negative or positive. 
# we will make use of the bing lexicons to extract the sentiments out of our data.
# retrieve the lexicons using the get_sentiments() function.

# for example
head(get_sentiments("bing"))

#implement filter() over the words that correspond to positivity 
positive_senti <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

# how many positive words and their word count
word_count <- text_df %>%
    semi_join(positive_senti) %>%
  count(word, sort = TRUE)


# we will use spread() function to segregate the data into positive and negative sentiments
# We will then use the mutate() function to calculate the total sentiment(difference between positive and negative sentiment.)
bing <- get_sentiments("bing")
text_sentiment <- text_df %>%
  inner_join(bing) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# most common positive and negative words
counting_words <- text_df %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)

head(counting_words)

#visualization of our sentiment score.
counting_words %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Sentiment Score")

#we can also use a wordcloud
text_df %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"),
                   max.words = 100)


# proportion of positive to negative sentiment
sent_pos <- sum(counting_words$sentiment == "positive")
sent_neg <- sum(counting_words$sentiment == "negative")

prop <- sent_pos/sent_neg
prop #positive sentiments are three-times greater in number

# check the accuracy of this value using number of recommenders in dat_final
rec_pos <- sum(dat_final$reviews.doRecommend == "TRUE")
rec_neg <- sum(dat_final$reviews.doRecommend == "FALSE")

rec_pos/rec_neg
#this massive difference can be attributed to two factors : only about 30% of reviews were included in the text file.
# this model is not so accurate. 

#----------------------------------------------------------------------------------

# Model 2 : Naive Bayes Approach
 
# data_scored is a csv file which is a manually scored form of all 590 reviews
# a positive review is assigned a score of 1; a negative review gets a score of 0
# it has two columns - sentence (the review) and score(its score)
data2_raw <- read.csv("C:/Users/MAG/Documents/CYO/data_scr.csv", header = FALSE, 
                      col.names = c("sentence","score"), stringsAsFactors = FALSE)

# overview of data frame
str(data2_raw)

# score is of class chr. make it into factors for analysis
data2_raw$score <- factor(data2_raw$score)
str(data2_raw$score)

# we can see the number of 0s and 1s
table(data2_raw$score)

# only about 2% of the reviews are negative

# transform the data for analysis
# create volatile coprus
#volatile coprus -  the corpus is stored in memory and would be destroyed when the R object containing it is destroyed.
# corpus - represents a collection of text documents

data2_corpus <- VCorpus(VectorSource(data2_raw$sentence))

# for example look at the first sentence(review). since the first cell contains column name, indexing is from 2
as.character(data2_corpus[[2]]) # first sentence

# convert all words to lowercase incase they are in differing formats
data2_corpus_clean <- tm_map(data2_corpus, content_transformer(tolower))

# see the first review again
as.character(data2_corpus[[2]]) # before
as.character(data2_corpus_clean[[2]]) # after
  
# remove numbers
data2_corpus_clean <- tm_map(data2_corpus_clean, removeNumbers)

# remove stopwords along with unecessary words like Alexa, Echo and Dot since they are repetitive but serve no purpose
data2_corpus_clean <- tm_map(data2_corpus_clean, removeWords, c(stopwords("english"),"Alexa","Echo","Dot"))

# remove punctuation
data2_corpus_clean <- tm_map(data2_corpus_clean, removePunctuation)

as.character((data2_corpus_clean[[2]])) # no punctuations

# stemming operation
data2_corpus_clean <- tm_map(data2_corpus_clean, stemDocument)

# remove whitespaces
data2_corpus_clean <- tm_map(data2_corpus_clean, stripWhitespace)

# finally we have 
as.character(data2_corpus_clean[[2]])

#now perform tokenization
data2_dtm <- DocumentTermMatrix(data2_corpus_clean)

# split into train (80%) and test (20%)
# excluding the column names (sentence and score) we have 590 observations
# 0.8*590 = 472 and 0.2*590 = 118
# first 472 observations form training set
# last 118 observations form test set

data2_dtm_train <- data2_dtm[2:472, ]
data2_dtm_test <- data2_dtm[473:591, ]

# label the rows in both sets
data2_train_labels <- data2_raw[2:472, ]$score
data2_test_labels <- data2_raw[473:591,]$score

# proportion of 0 scores must be roughly equal in both sets
prop.table(table(data2_train_labels))[1]

prop.table(table(data2_test_labels))[1]

# yes they are roughly the same meaning both sets have roughly 2% negative reviews

# word cloud
# frequently occuring words are blue
# less frequent words are green
wordcloud(data2_corpus_clean, max.words = 290, random.order = TRUE, colors = c("blue", "dark green"))

#Remove words from the matrix that appear less than 5 times

data2_freq_words <- findFreqTerms(data2_dtm_train, 5)
str(data2_freq_words)

#Limit our Document Term Matrix to only include words in the data2_freq_vector.
data2_dtm_freq_train <- data2_dtm_train[ , data2_freq_words]
data2_dtm_freq_test <- data2_dtm_test[ , data2_freq_words]

# convert the matrix to “yes” and “no” categorical variables since naive bayes works with categoriacl data
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

# values greater than 0 : yes ; values not greater than 0 : no
data2_train <- apply(data2_dtm_freq_train, MARGIN = 2, convert_counts)
data2_test <- apply(data2_dtm_freq_test, MARGIN = 2, convert_counts)

# train the model using e1071 package
data2_classifier <- naiveBayes(data2_train, data2_train_labels)

# predict and evaluate
data2_test_pred <- predict(data2_classifier, data2_test)

CrossTable(data2_test_pred, data2_test_labels, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))
# because of the nature of the data chosen it so happens that the data2_test_pred set contains only 1s.

# accuracy of 1s predicted
error <- 3/119
accuracy <- 100 - (100*error)
print(paste("Accuracy of the prediction is : ", round((accuracy), digits = 2)))

# the accuracy achieved in this model is very good. Hence we can recommend this item to a customer