#Φτιάχνουμε τα  Heatmaps plots----

#Εισάγουμε τα δεδομένα

#Χρειαζόμαστε πρώτα την παρακάτω βιβιλιοθήκη
library(readr)

#Φορτώνουμε τα δεδομένα από το URL
data = read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

#Το μετατρέπουμε σε data frame με σκοπό να μπορούμε να το χειριστούμε
data1 = as.data.frame(data)

#Μετατρέπουμε την ημερομηνία σε χαρακτήρα γιατί μας βολεύει
data1$date = as.character(data1$date)

#Ορίζουμε μια μεταβλητή η οποία θα περιέχει όλες τις χώρες

list_of_countries = unique(data1$location)

data1 = data1[,c("continent","location","date","total_cases","total_deaths")]

data1 = subset(data1, data1$location!="Western Sahara")

#Με την εντολή subset παίρνουμε ένα υποσυνολο του data1 και με βάση την συνθήκη
#data1$location!="Western Sahara" παίρνουμε το υποσύνολο του data1 όπου δεν
#έχει την χώρα Western Sahara.

#Ορίζουμε μια μεταβλητή η οποία θα περιέχει όλες τις χώρες

list_of_countries = unique(data1$location)

#Η εντολή unique μας δίνει το σύνολο όλων των χωρών

#Εύρεση ημερομηνιών έναρξης και λήξης της καταγραφής----

#Η παρακάτω λίστα θα περιέχει τις ημερομηνίες από τις οποίες ξεκίνησε η καταγραφή
#για κάθε χώρα

starting_date = rep(0,length(list_of_countries))

#Η παρακάτω λίστα θα περιέχει τις ημερομηνίες από τις οποίες τελείωσε η καταγραφή
#για κάθε χώρα

ending_date = rep(0,length(list_of_countries))

for (i in 1:length(list_of_countries)){
  
  k = data1$date[data1$location == list_of_countries[i]];
  
  starting_date[i] = k[1]
  
  ending_date[i] = k[length(k)]
  
}

#Τώρα θα βρούμε τις χώρες για τις οι οποίες η καταγραφή ξεκινάει ή τελειώνει
#το 2021.

#Φτιάχνουμε μια λίστα που θα περιέχει αυτές τις χώρες

countries_2021 = list()

#Φτιάχνουμε ένα dataframe με τις χώρες και τις ημερομηνίες έναρξης και λήξης
#της καταγραφής

df = data.frame("location" = unique(data1$location),"start_date" = starting_date
                ,"end_date" = ending_date)

#Θα χρησιμοποιήσουμε την συνάρτηση grepl η οποία επιστρέφει TRUE όταν το όρισμα
#που θα βάλουμε σε αυτήν υπάρχει σε μια άλλη συμβολοσειρά

for (i in 1:length(list_of_countries)) {
  
  #Για όλες τις χώρες που υπάρχουν στο dataset
  
  if ((grepl("2021",df$start_date[i],fixed=TRUE) == TRUE) |
      
      (grepl("2021",df$end_date[i],fixed=TRUE) == TRUE)){
    
    #Αν η ημερομηνία έναρξης καταγραφής περιέχει το 2021 ή
    #η ημερομηνία λήξης καταγραφής περιέχει το 2021 τότε
    #βάλε αυτή την χώρα στην λίστα countries_2021
    
    countries_2021 = append(countries_2021,df$location[i])
    
  }
  
}

#Σε αυτό το σημείο ξέρουμε ότι έχουμε αποθηκευμένες σε μια λίστα όλες τις
#χώρες για τις οποίες η καταγραφή αρχίζει ή τελείωνει το 2021

#Τώρα θα πρέπει να βγάλουμε τις παρατηρήσεις που σχετίζονται με αυτές τι χώρες

for (country in countries_2021) {
  
  data1 = subset(data1, data1$location != country)
  
}

#Μετά θα πρέπει να βγάλουμε αυτές τις χώρες από την μεταβλητή list_of_countries

for (country in countries_2021) {
  
  list_of_countries = list_of_countries[list_of_countries != country]
  
}

drop_countries = c("Africa","Asia","Europe","European Union","International",
                   "North America","Oceania","South America","High income",
                   "Low income","Lower middle income","Upper middle income",
                   "World")

drop_countries = c(drop_countries,"Saint Helena","Samoa","Solomon Islands","Vanuatu","Wallis and Futuna")

for (country in drop_countries) {
  
  data1 = subset(data1, data1$location != country)
  
}

#Τώρα θα πρέπει να αφαιρέσουμε αυτές τις "χώρες" και από την λίστα των χωρών

for (country in drop_countries) {
  
  list_of_countries = list_of_countries[list_of_countries != country]
  
}

#Υπολογισμός των ημερομηνιών έναρξης και λήξης καταγραφής δεδομένων vol2----

#Θα υπολογίσουμε πάλι τις ημερομηνίες έναρξης και λήξης καταγραφής δεδομένων
#για τις χώρες που έχουν μείνει (θα κάνουμε αντιγραφή-επικόλληση τον κώδικα που
#έχουμε γράψει πιο πάνω)

starting_date = rep(0,length(list_of_countries))

#Η παρακάτω λίστα θα περιέχει τις ημερομηνίες από τις οποίες τελείωσε η καταγραφή
#για κάθε χώρα

ending_date = rep(0,length(list_of_countries))

for (i in 1:length(list_of_countries)){
  
  k = data1$date[data1$location == list_of_countries[i]];
  
  starting_date[i] = k[1]
  
  ending_date[i] = k[length(k)]
  
}

max_starting_date = max(as.Date(starting_date))

min_ending_date = min(as.Date(ending_date))

#Η παρακάτω βιβλιοθήκη μας είναι απαραίτητη
library(dplyr)

starting_date = as.data.frame(starting_date)

rownames(starting_date) = list_of_countries

for (country in list_of_countries) {
  
  #Για τις χώρες που ξεκινά πιο παλιά η καταγραφή από το maximum (ιδιότητα)
  
  if(starting_date[country,]!= max_starting_date){
    
    #Θα ορίσω έναν δείκτη ο οποίος θα βρίσκει την θέση από την οποία ξεκινάει η
    #καταγραφή
    
    count1 = 1
    
    while (data1$location[count1]!=country) {
      
      count1 = count1 + 1
      
    }
    
    count2 = count1
    
    while (data1$date[count2]!=max_starting_date) {
      
      count2 = count2 + 1
      
    }
    
    count2 = count2 - 1
    
    rows_to_delete = count1:count2
    
    data1 = data1[-rows_to_delete,]
    
  }
  
}

#Σε αυτό το σημείο ξέρουμε πως η καταγραφή των δεδομένων κάθε χώρας έχει
#ξεκινήσει από την ίδια μέρα. Μένει να κάνουμε το δυικό για την λήξη.

#Διόρθωση της ημερομηνίας λήξης της καταγραφής δεδομένων----

i = 1

while (i <= length(list_of_countries)){
  
  country = list_of_countries[i]
  
  data_c = data1[data1$location == country,]
  
  if(data_c$date[length(data_c$date)] != min_ending_date){
    
    start1 = 1
    
    while ((data1$location[start1]!=country) == TRUE) {
      
      start1 = start1 + 1
      
    }
    
    #Θα βρούμε την σειρά του dataframe από την οποία τελειώνει η καταγραφή για
    #την χώρα country στο αρχικό dataset
    
    data_c = data1[data1$location == country,]
    
    ind = 1
    
    steps = 0
    
    while (as.Date(data_c$date[ind])<=as.Date(min_ending_date)) {
      
      steps = steps + 1
      
      ind = ind + 1
      
    }
    
    start1 = start1 + steps
    
    finish1  = start1
    
    while((data1$location[finish1] == country) & (is.na(data1$location[finish1]) == FALSE)){
      
      finish1 = finish1 + 1
      
    }
    
    finish1 = finish1 - 1
    
    rows_to_delete = start1:finish1
    
    data1 = data1[-rows_to_delete,]
    
  }
  
  i = i + 1
  
}

#Yπολογισμός ημερομηνίων έναρξης και λήξης καταγραφής δεδομένων vol3 (για επλαήθευση αυτή τη φορά)----

#Τώρα υπολογίζουμε και εμφανίζουμε τις ημερομηνίες έναρξης και λήξης καταγραφής
#των δεδομένων ώστε να επαληθεύσουμε ότι τα παραπάνω έχουν γίνει σωστά.
#Ακολουθούμε ακριβώς την ίδια διαδικασία που κάναμε και πιο πάνω

starting_date = rep(0,length(list_of_countries))

#Η παρακάτω λίστα θα περιέχει τις ημερομηνίες από τις οποίες τελείωσε η καταγραφή
#για κάθε χώρα

ending_date = rep(0,length(list_of_countries))

for (i in 1:length(list_of_countries)){
  
  k = data1$date[data1$location == list_of_countries[i]];
  
  starting_date[i] = k[1]
  
  ending_date[i] = k[length(k)]
  
}

#Ακολουθεί η αναπαράσταση των αποτελεσμάτων σε μορφή χώρα ημερομηνία έναρξης
#ημερομηνία λήξης

cbind(unique(data1$location),starting_date,ending_date)

#Προετοιμασία δεδομένων σχετικά με τα γραφήματα που αντιστοιχούν στην Ευρώπη----

data.europe = subset(data1, data1$continent == "Europe") #με τα missing values

data.europe.not.na = na.omit(data.europe) #χωρίς τα missing values

#Heatmap για την μεταβλητή total_cases στις Ευρωπαικές χώρες----

library(ggplot2)
library(plotly)
library(scales)

brk = seq(from = min(data.europe.not.na$total_cases),
          to = max(data.europe.not.na$total_cases),
                   length.out = 4)

p = as.data.frame(data.europe.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_cases)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total cases in European countries")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες της Ευρώπης που εμφάνισαν
# μεγάλο αριθμό συνολικών κρουσμάτων ήταν: το Ηνωμένο Βασίλειο,η Ισπανία,
# η Ολλανδία,η Ιταλία,η Γερμανία,η Γαλλία. Επιπλέον παρατηρούμε ότι στην
# Γερμανία και στην Γαλλία τον Ιούνιο του 2022 ο συνολικός αριθμός
# κρουσμάτων ήταν συγκριτικά πολύ υψηλότερος από ότι των υπολοίπων
# Ευρωπαϊκών χωρών.

#Heatmap για την μεταβλητή total_deaths στις Ευρωπαικές χώρες----

brk = seq(from = min(data.europe.not.na$total_deaths),
          to = max(data.europe.not.na$total_deaths),
          length.out = 4)

p = as.data.frame(data.europe.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_deaths)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total deaths in European countries")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες της Ευρώπης που εμφάνισαν
# μεγάλο αριθμό συνολικών θανάτων ήταν: το Ηνωμένο Βασίλειο, η Ουκρανία,
# η Ισπανία, η Ρωσία, η Πολωνία, η Ιταλία, η Γερμανία, η Γαλλία. Επιπλέον,
# βλέπουμε ότι στην Ρωσία από τον Απρίλιο έως και τον Μάϊο του 2022 ο
# συνολικός αριθμός θανάτων ήταν συγκριτικά πολύ υψηλότερος από ότι των
# υπολοίπων Ευρωπαϊκών χωρών.

#Προετοιμασία δεδομένων σχετικά με τα γραφήματα που αντιστοιχούν στην Ασία----

data.asia = subset(data1, data1$continent == "Asia") #με τα missing values

data.asia.not.na = na.omit(data.asia) #χωρίς τα missing values

#Heatmap για την μεταβλητή total cases στις Ασιατικές χώρες----

brk = seq(from = min(data.asia.not.na$total_cases),
          to = max(data.asia.not.na$total_cases),
          length.out = 4)

p = as.data.frame(data.asia.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_cases)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total cases in Asian countries")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες της Ασίας που εμφάνισαν μεγάλο
# αριθμό συνολικών κρουσμάτων ήταν: το Βιετνάμ, η Τουρκία, η Νότια Κορέα, η Ινδία.
# Επιπλεόν, βλέπουμε ότι η Ινδία από το Μάϊο του 2021 και μετά εμφάνιζε συγκριτικά
# πολύ υψηλότερο αριθμό συνολικών κρουσμάτων από ότι οι υπόλοιπες Ασιατικές χώρες.

#Heatmap για την μεταβλητή total deaths στις Ασιατικές χώρες----

brk = seq(from = min(data.asia.not.na$total_deaths),
          to = max(data.asia.not.na$total_deaths),
          length.out = 4)

p = as.data.frame(data.asia.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_deaths)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total deaths in Asian countries")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες της Ασίας που εμφάνισαν μεγάλο αριθμό
# συνολικών θανάτων ήταν: η Τουρκία, το Ιράν, η Ινδονησία, η Ινδία. Επιπλέον, βλέπουμε
# ότι η Ινδία από τον Ιούνιο του 2021 και μετά εμφάνιζε συγκριτικά πολύ υψηλότερο
# αριθμό συνολικών θανάτων από ότι οι υπόλοιπες Ασιατικές χώρες.


#Προετοιμασία δεδομένων για τα γραφήματα που σχετίζονται με τις χώρες της Νότιας Αμερικής----

data.south.america = subset(data1, data1$continent == "South America") #με τα missing values

data.south.america.not.na = na.omit(data.south.america) #χωρίς τα missing values

#Heatmap για την μεταβλητή total cases στις χώρες της Νότιας Αμερικής----

brk = seq(from = min(data.south.america.not.na$total_cases),
          to = max(data.south.america.not.na$total_cases),
          length.out = 4)

p = as.data.frame(data.south.america.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_cases)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total cases in countries of South America")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες της Νότιας Αμερικής που εμφάνισαν μεγάλο
# αριθμό συνολικών κρουσμάτων ήταν: η Βραζιλία, η Αργεντινή. Επιπλέον, βλέπουμε ότι η
# Βραζιλία από τον Ιούλιο του 2021 και μετά εμφάνιζε συγκριτικά πολύ υψηλότερο αριθμό
# συνολικών κρουσμάτων από ότι οι υπόλοιπες χώρες της Νότιας Αμερικής.

#Heatmap για την μεταβλητή total deaths στις χώρες της Νότιας Αμερικής----

brk = seq(from = min(data.south.america.not.na$total_deaths),
          to = max(data.south.america.not.na$total_deaths),
          length.out = 4)

p = as.data.frame(data.south.america.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_deaths)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total deaths in countries of South America")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες της Νότιας Αμερικής που εμφάνισαν
# μεγάλο αριθμό συνολικών θανάτων ήταν: το Περού, η Βραζιλία. Επιπλεόν βλέπουμε ότι
# η Βραζιλία από τον Μάϊο του 2021 και μετά η Βραζιλία εμφάνιζε συγκριτικά πολύ
# υψηλότερο αριθμό συνολικών θανάτων από ότι οι υπόλοιπες χώρες της Νότιας Αμερικής.

#Προετοιμασία δεδομένων σχετικά με τα γραφήματα που αντιστοιχούν στην Βόρεια Αμερική----

data.north.america = subset(data1, data1$continent == "North America") #με τα missing values

data.north.america.not.na = na.omit(data.north.america) #χωρίς τα missing values

#Heatmap για την μεταβλητή total cases στις χώρες της Βόρειας Αμερικής----

brk = seq(from = min(data.north.america.not.na$total_cases),
          to = max(data.north.america.not.na$total_cases),
          length.out = 4)

p = as.data.frame(data.north.america.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_cases)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total cases in countries of North America")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι Η.Π.Α εμφάνισαν αισθητά πολύ μεγαλύτερο
# αριθμό συνολικών κρουσμάτων από ότι οι υπόλοιπες χώρες της Βόρειας Αμερικής.

#Heatmap για την μεταβλητή total deaths στις χώρες της Βόρειας Αμερικής----

brk = seq(from = min(data.north.america.not.na$total_deaths),
          to = max(data.north.america.not.na$total_deaths),
          length.out = 4)

p = as.data.frame(data.north.america.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_deaths)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total deaths in countries of North America")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες της Βόρειας Αμερικής που εμφάνισαν
# μεγάλο αριθμό συνολικών θανάτων ήταν: οι Η.Π.Α, το Μεξικό. Επιπλεόν, βλέπουμε ότι
# οι Η.Π.Α από τον Σεπτέμβριο του 2021 και μετά εμφάνιζαν συγκριτικά πολύ υψηλότερο
# αριθμό συνολικών θανάτων από ότι οι υπόλοιπες χώρες της Βόρειας Αμερικής.


#Προετοιμασία δεδομένων για τα γραφήματα σχετικά με τις χώρες της Ωκεανίας----

data.oceania = subset(data1, data1$continent == "Oceania") #με τα missing values

data.oceania.not.na = na.omit(data.oceania) #χωρίς τα missing values

#Heatmap για την μεταβλητή total cases στις χώρες της Ωκεανίας----

brk = seq(from = min(data.oceania.not.na$total_cases),
          to = max(data.oceania.not.na$total_cases),
          length.out = 4)

p = as.data.frame(data.oceania.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_cases)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total cases in countries of Oceania")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι η Αυστραλία εμφάνισε αισθητά πολύ
# μεγαλύτερο αριθμό συνολικών κρουσμάτων από ότι οι υπόλοιπες χώρες της
# Ωκεανίας.

#Heatmap για την μεταβλητή total deaths στις χώρες της Ωκεανίας----

brk = seq(from = min(data.oceania.not.na$total_deaths),
          to = max(data.oceania.not.na$total_deaths),
          length.out = 4)

p = as.data.frame(data.oceania.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_deaths)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total deaths in countries of Oceania")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι η Αυστραλία εμφάνισε αισθητά πολύ μεγαλύτερο
# αριθμό συνολικών θανάτων από ότι οι υπόλοιπες χώρες της Ωκεανίας.

#Προετοιμασία δεδομένων για τα γραφήματα που σχετίζονται με τις χώρες της Αφρικής----

data.africa = subset(data1, data1$continent == "Africa") #με τα missing values

data.africa.not.na = na.omit(data.africa) #χωρίς τα missing values

#Heatmap για την μεταβλητή total cases στις χώρες της Αφρικής----

brk = seq(from = min(data.africa.not.na$total_cases),
          to = max(data.africa.not.na$total_cases),
          length.out = 4)

p = as.data.frame(data.africa.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_cases)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total cases in African countries")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες της Αφρικής που εμφάνισαν
# μεγάλο αριθμό συνολικών κρουσμάτων ήταν: η Τυνισία, η Νότια Αφρική, το
# Μαρόκο. Επιπλέον, βλέπουμε ότι η Νότια Αφρική από τον Σεπτέμβριο του 2021
# και μετά εμφάνιζε συγκριτικά πολύ υψηλότερο συνολικό αριθμό κρουσμάτων
# από ότι οι υπόλοιπες Αφρικανικές χώρες.

#Heatmap για την μεταβλητή total deaths στις χώρες της Αφρικής----

brk = seq(from = min(data.africa.not.na$total_deaths),
          to = max(data.africa.not.na$total_deaths),
          length.out = 4)

p = as.data.frame(data.africa.not.na) %>%
  ggplot(aes(x = date, y = location, fill = total_deaths)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total deaths in African countries")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε οτι οι χώρες της Αφρικής που εμφάνισαν μεγάλο
# αριθμό συνολικών θανάτων ήταν: η Τυνησία, η Νότια Αφρική. Επιπλεόν, βλέπουμε
# ότι η Νότια Αφρική από τον Ιούλιο του 2021 και μετά εμφάνιζε συγκριτικά πολύ
# υψηλότερο συνολικό αριθμό θανάτων από ότι οι υπόλοιπες Αφρικανικές χώρες.

#Τώρα πάμε να κάνουμε για όλες τις ηπείρους----

data2 = as.data.frame(data)

#Μετατρέπουμε την ημερομηνία σε χαρακτήρα γιατί μας βολεύει
data2$date = as.character(data2$date)

data2 = data2[,c("location","date","total_cases","total_deaths")]

data.europe = data2[data2$location == "Europe",]
data.europe = na.omit(data.europe)

data.asia = data2[data2$location == "Asia",]
data.asia = na.omit(data.asia)

data.south.america = data2[data2$location == "South America",]
data.south.america = na.omit(data.south.america)

data.north.america = data2[data2$location == "North America",]
data.north.america = na.omit(data.north.america)

data.oceania = data2[data2$location == "Oceania",]
data.oceania = na.omit(data.oceania)

data.africa = data2[data2$location == "Africa",]
data.africa = na.omit(data.africa)

#Φτιάχνουμε όλο το dataset χωρίς τα missing values

data.all = rbind.data.frame(data.europe,data.asia,data.south.america,data.north.america,data.oceania,data.africa)

#Heatmap για την μεταβλητή total cases----

brk = seq(from = min(data.all$total_cases),
          to = max(data.all$total_cases),
          length.out = 4)

p = as.data.frame(data.all) %>%
  ggplot(aes(x = date, y = location, fill = total_cases)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total cases in all continents")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι η Νότια Αμερική,η Ευρώπη,η Ασία εμφάνισαν
# αισθητά μεγαλύτερο αριθμό συνολικών κρουσμάτων από ότι οι υπόλοιπες ήπειροι.
# Επιπλέον, βλέπουμε ότι η Ευρώπη ήταν η ήπειρος που εμφάνισε τον υψηλότερο αριθμό
# συνολικών κρουσμάτων από ότι οι άλλες ήπειροι.

#Heatmap για την μεταβλητή total deaths----

brk = seq(from = min(data.all$total_deaths),
          to = max(data.all$total_deaths),
          length.out = 4)

p = as.data.frame(data.all) %>%
  ggplot(aes(x = date, y = location, fill = total_deaths)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Progress of total deaths in all continents")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι η Νότια Αμερική, η Βόρεια Αμερική, η Ευρώπη,
# η Ασία εμφάνισαν αισθητά μεγαλύτερο αριθμό συνολικών θανάτων από ότι οι άλλες
# ήπειροι. Επιπλέον, βλέπουμε ότι η Ευρώπη εμφάνισε τον υψηλότερο αριθμό συνολικών
# θανάτων από ότι οι άλλες ήπειροι.

#Καθαρίζουμε το workspace
rm(list = ls())

#Εισαγωγή δεδομένων και cleaning----

#Χρειαζόμαστε πρώτα την παρακάτω βιβιλιοθήκη
library(readr)

#Φορτώνουμε τα δεδομένα από το URL
data1 = read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

#Το μετατρέπουμε σε data frame με σκοπό να μπορούμε να το χειριστούμε
data1 = as.data.frame(data1)

#Μετατρέπουμε την ημερομηνία σε χαρακτήρα γιατί μας βολεύει
data1$date = as.character(data1$date)

#Θα θέλαμε να δούμε το dataset σε μορφή χώρα,από πότε ξεκινάει η καταγραφή
#των δεδομένων,πότε τελειώνει η καταγραφή των δεδομένων

#Ορίζουμε μια μεταβλητή η οποία θα περιέχει όλες τις χώρες

list_of_countries = unique(data1$location)

#Η εντολή unique μας δίνει το σύνολο όλων των χωρών

#Εύρεση ημερομηνιών έναρξης και λήξης της καταγραφής----

#Η παρακάτω λίστα θα περιέχει τις ημερομηνίες από τις οποίες ξεκίνησε η καταγραφή
#για κάθε χώρα

starting_date = rep(0,length(list_of_countries))

#Η παρακάτω λίστα θα περιέχει τις ημερομηνίες από τις οποίες τελείωσε η καταγραφή
#για κάθε χώρα

ending_date = rep(0,length(list_of_countries))

for (i in 1:length(list_of_countries)){
  
  k = data1$date[data1$location == list_of_countries[i]];
  
  starting_date[i] = k[1]
  
  ending_date[i] = k[length(k)]
  
}

#Ακολουθεί η αναπαράσταση των αποτελεσμάτων σε μορφή χώρα ημερομηνία έναρξης
#ημερομηνία λήξης

cbind(unique(data1$location),starting_date,ending_date)

#Αφαίρεση χωρών για τις οποίες η καταγραφή έχει ξεκινήσει ή τελειώσει το 2021----

#Βλέπουμε ότι υπάρχουν χώρες για τις οποίες η καταγραφή έχει ξεκινήσει από το
#2021 (π.χ η χώρα με όνομα "Cook Islands") ή έχει τελειώσει το 2021. Επιπλέον,
#η καταγραφή για την χώρα με όνομα Western Sahar ξεκινάει το 2022 και τελείωνει
#το 2022. Επειδή θέλουμε η καταγραφή των δεδομένων να είναι μεγάλης διάρκειας
#τις παραπάνω χώρες θα τις αφαιρέσουμε.

#Αφαιρούμε την χώρα Western Sahara

data1 = subset(data1, data1$location!="Western Sahara")

#Με την εντολή subset παίρνουμε ένα υποσυνολο του data1 και με βάση την συνθήκη
#data1$location!="Western Sahara" παίρνουμε το υποσύνολο του data1 όπου δεν
#έχει την χώρα Western Sahara.

#Αφαιρούμε την ημερομηνία έναρξης καταγραφής των δεδομένων

starting_date = starting_date[-240]

#Αφαιρούμε την ημερομηνία λήξης καταγραφής των δεδομένων

ending_date = ending_date[-240]

#Θα πρέπει να αφαιρέσουμε την χώρα "Western Sahara" και από την λίστα
#list_of_countries

list_of_countries = list_of_countries[list_of_countries != "Western Sahara"]

#Τώρα θα βρούμε τις χώρες για τις οι οποίες η καταγραφή ξεκινάει ή τελειώνει
#το 2021.

#Φτιάχνουμε μια λίστα που θα περιέχει αυτές τις χώρες

countries_2021 = list()

#Φτιάχνουμε ένα dataframe με τις χώρες και τις ημερομηνίες έναρξης και λήξης
#της καταγραφής

df = data.frame("location" = unique(data1$location),"start_date" = starting_date
                ,"end_date" = ending_date)

#Θα χρησιμοποιήσουμε την συνάρτηση grepl η οποία επιστρέφει TRUE όταν το όρισμα
#που θα βάλουμε σε αυτήν υπάρχει σε μια άλλη συμβολοσειρά

for (i in 1:length(list_of_countries)) {
  
  #Για όλες τις χώρες που υπάρχουν στο dataset
  
  if ((grepl("2021",df$start_date[i],fixed=TRUE) == TRUE) |
      
      (grepl("2021",df$end_date[i],fixed=TRUE) == TRUE)){
    
    #Αν η ημερομηνία έναρξης καταγραφής περιέχει το 2021 ή
    #η ημερομηνία λήξης καταγραφής περιέχει το 2021 τότε
    #βάλε αυτή την χώρα στην λίστα countries_2021
    
    countries_2021 = append(countries_2021,df$location[i])
    
  }
  
}

#Σε αυτό το σημείο ξέρουμε ότι έχουμε αποθηκευμένες σε μια λίστα όλες τις
#χώρες για τις οποίες η καταγραφή αρχίζει ή τελείωνει το 2021

#Τώρα θα πρέπει να βγάλουμε τις παρατηρήσεις που σχετίζονται με αυτές τι χώρες

for (country in countries_2021) {
  
  data1 = subset(data1, data1$location != country)
  
}

#Μετά θα πρέπει να βγάλουμε αυτές τις χώρες από την μεταβλητή list_of_countries

for (country in countries_2021) {
  
  list_of_countries = list_of_countries[list_of_countries != country]
  
}

list_of_countries

###ΕΡΩΤΗΣΗ 1----

#Στο dataset υπάρχουν δεδομένα για Asia, Europe, European Union,International,
#World αυτές τις πετάω ή όχι; Μπορούμε αν είναι να τις πετάξουμε και να κάνουμε
#μια ανάλυση και με αυτές π.χ πάμε να δούμε πως εξελίχθηκε ο κορωνοιος σε
#επίπεδο ηπείρων.

#Όταν γράφει Asia αναφέρεται στα άτομα που ζουν στην χώρα την οποία θεωρούν
#αυτοί ως Ασία link στο mail( θα παραθέσω από κάτω link)

#https://ourworldindata.org/grapher/continents-according-to-our-world-in-data

#(Εγώ πέταξα τις χώρες Αφρική,Ασία,Ευρώπη, Ευρωπαική Ένωση, International,
#Βόρεια Αμερική, Ωκεανία, Νότια Αμερική)

#Υπάρχει διαφορά μεταξύ International και World (δες το e-mail)

####ΕΡΩΤΗΣΗ 2----

#Στο dataset υπάρχει η Βουλγαρία και η Βόρεια Μακεδονία ποια από τις δυο θα
#κρατήσω;

#Τώρα θα βγάλω τις παρατηρήσεις που αντιστοιχούν στις "χώρες":

#High income
#Low income
#Lower middle income
#Upper middle income
#World

#Όταν λέει Low Income αναφέται σε όλες τις χώρες οι οποίες θεωρούνται
#ως χώρες των οποίων οι πολίτες έχουν χαμηλό εισόδημα με βάση την Παγκόσμια
#τράπεζα. Περισσότερα δες το mail (θα βάλω από κάτω link)

#https://datatopics.worldbank.org/world-development-indicators/the-world-by-income-and-region.html

#Εγώ προς το παρόν τις πετάω.Μπορώ τα δεδομένα αυτά να τα κρατήσω και να
#απαντήσω σε ερωτήματα σχετικά με το εισόδημα και την εξέλιξη του κορονοιου

drop_countries = c("Africa","Asia","Europe","European Union","International",
                   "North America","Oceania","South America","High income",
                   "Low income","Lower middle income","Upper middle income",
                   "World")

###ΕΡΩΤΗΣΗ 3----

#Αν συνεχίσω κανονικά την διαδικασία που έχω κάνει τότε θα προκύψει η κοινή
#ημερομηνία έναρξης καταγραφής δεδομένων 18/11/20. Αν αφαιρέσω και τις χώρες
#"Saint Helena","Samoa","Solomon Islands","Vanuatu","Wallis and Futuna" τότε
#η κοινή ημερομηνία έναρξης καταγραφής των δεδομένων θα είναι 13/5/20. Η
#αλήθεια είναι ότι πιστεύω ότι είναι καλύτερο να προτιμήσουμε αυτή την ημερομηνία
#(το οποίο σημαίνει ότι πρέπει να αφαιρέσουμε και τις προαναφερθείσες χώρες)
#γιατί ο ιος ξέσπασε το 2020 οπότε το να ξεκινούν τα δεδομένα μας από την ημερο-
#μηνία 18/11/20 έχει σαν αντίκτυπο να χάσουμε πολλές μέρες στις οποίες ο ιος
#ξεκίνησε να εξαπλώνεται.

drop_countries = c(drop_countries,"Saint Helena","Samoa","Solomon Islands","Vanuatu","Wallis and Futuna")

for (country in drop_countries) {
  
  data1 = subset(data1, data1$location != country)
  
}

#Τώρα θα πρέπει να αφαιρέσουμε αυτές τις "χώρες" και από την λίστα των χωρών

for (country in drop_countries) {
  
  list_of_countries = list_of_countries[list_of_countries != country]
  
}

#Υπολογισμός των ημερομηνιών έναρξης και λήξης καταγραφής δεδομένων vol2----

#Θα υπολογίσουμε πάλι τις ημερομηνίες έναρξης και λήξης καταγραφής δεδομένων
#για τις χώρες που έχουν μείνει (θα κάνουμε αντιγραφή-επικόλληση τον κώδικα που
#έχουμε γράψει πιο πάνω)

starting_date = rep(0,length(list_of_countries))

#Η παρακάτω λίστα θα περιέχει τις ημερομηνίες από τις οποίες τελείωσε η καταγραφή
#για κάθε χώρα

ending_date = rep(0,length(list_of_countries))

for (i in 1:length(list_of_countries)){
  
  k = data1$date[data1$location == list_of_countries[i]];
  
  starting_date[i] = k[1]
  
  ending_date[i] = k[length(k)]
  
}

#Ακολουθεί η αναπαράσταση των αποτελεσμάτων σε μορφή χώρα ημερομηνία έναρξης
#ημερομηνία λήξης

cbind(unique(data1$location),starting_date,ending_date)

#Παρατήρηση προβλήματος διαφορετικών ημερομηνίων έναρξης και λήξης δεδομένων και επίλυση του----

#Βλέπουμε ότι οι ημερομηνίες έναρξης και λήξης καταγραφής δεδομένων των χωρών
#είναι διαφορετικές (π.χ για την πρώτη περίπτωση η καταγραφή για το Afghanistan
#ξεκινάει από 24/2/2020 ενώ στην Αφρική ξεκινάει από την 13/2/2020). Αυτό 

#Θα χρησιμοποιήσουμε την εντολή as.Date με σκοπό να μπορούμε να χειριστούμε
#πιο εύκολα τις ημερομηνίες. Με αυτή την εντολή μας είναι πιο εύκολο να
#συγκρίνουμε 2 ημερομηνίες και να δούμε π.χ ποια είναι η παλαιότερη

#Θα βρούμε την ημερομηνία από την οποία θα πρέπει να ξεκινάει η καταγραφή
#κάθε χώρας

max_starting_date = max(as.Date(starting_date))

#Θα βρούμε την ημερομηνία στην οποία θα πρέπει να τελειώνει η καταγραφή της
#κάθε χώρας

min_ending_date = min(as.Date(ending_date))

#Διόρθωση ημερομηνιών έναρξης καταγραφής δεδομένων----

#’ρα θα πρέπει να παρουμε τις παρατηρήσεις που ανήκουν στο χρονικό διάστημα
# 18/11/2020 έως και 18/6/2022 για όλες τις χώρες

#Η παρακάτω βιβλιοθήκη μας είναι απαραίτητη
library(dplyr)

starting_date = as.data.frame(starting_date)

rownames(starting_date) = list_of_countries

for (country in list_of_countries) {
  
  #Για τις χώρες που ξεκινά πιο παλιά η καταγραφή από το maximum (ιδιότητα)
  
  if(starting_date[country,]!= max_starting_date){
    
    #Θα ορίσω έναν δείκτη ο οποίος θα βρίσκει την θέση από την οποία ξεκινάει η
    #καταγραφή
    
    count1 = 1
    
    while (data1$location[count1]!=country) {
      
      count1 = count1 + 1
      
    }
    
    count2 = count1
    
    while (data1$date[count2]!=max_starting_date) {
      
      count2 = count2 + 1
      
    }
    
    count2 = count2 - 1
    
    rows_to_delete = count1:count2
    
    data1 = data1[-rows_to_delete,]
    
  }
  
}

#Σε αυτό το σημείο ξέρουμε πως η καταγραφή των δεδομένων κάθε χώρας έχει
#ξεκινήσει από την ίδια μέρα. Μένει να κάνουμε το δυικό για την λήξη.

#Διόρθωση της ημερομηνίας λήξης της καταγραφής δεδομένων----

i = 1

while (i <= length(list_of_countries)){
  
  country = list_of_countries[i]
  
  data_c = data1[data1$location == country,]
  
  if(data_c$date[length(data_c$date)] != min_ending_date){
    
    start1 = 1
    
    while ((data1$location[start1]!=country) == TRUE) {
      
      start1 = start1 + 1
      
    }
    
    #Θα βρούμε την σειρά του dataframe από την οποία τελειώνει η καταγραφή για
    #την χώρα country στο αρχικό dataset
    
    data_c = data1[data1$location == country,]
    
    ind = 1
    
    steps = 0
    
    while (as.Date(data_c$date[ind])<=as.Date(min_ending_date)) {
      
      steps = steps + 1
      
      ind = ind + 1
      
    }
    
    start1 = start1 + steps
    
    finish1  = start1
    
    while((data1$location[finish1] == country) & (is.na(data1$location[finish1]) == FALSE)){
      
      finish1 = finish1 + 1
      
    }
    
    finish1 = finish1 - 1
    
    rows_to_delete = start1:finish1
    
    data1 = data1[-rows_to_delete,]
    
  }
  
  i = i + 1
  
}

#Yπολογισμός ημερομηνίων έναρξης και λήξης καταγραφής δεδομένων vol3 (για επλαήθευση αυτή τη φορά)----

#Τώρα υπολογίζουμε και εμφανίζουμε τις ημερομηνίες έναρξης και λήξης καταγραφής
#των δεδομένων ώστε να επαληθεύσουμε ότι τα παραπάνω έχουν γίνει σωστά.
#Ακολουθούμε ακριβώς την ίδια διαδικασία που κάναμε και πιο πάνω

starting_date = rep(0,length(list_of_countries))

#Η παρακάτω λίστα θα περιέχει τις ημερομηνίες από τις οποίες τελείωσε η καταγραφή
#για κάθε χώρα

ending_date = rep(0,length(list_of_countries))

for (i in 1:length(list_of_countries)){
  
  k = data1$date[data1$location == list_of_countries[i]];
  
  starting_date[i] = k[1]
  
  ending_date[i] = k[length(k)]
  
}

#Ακολουθεί η αναπαράσταση των αποτελεσμάτων σε μορφή χώρα ημερομηνία έναρξης
#ημερομηνία λήξης

cbind(unique(data1$location),starting_date,ending_date)

#Αφού κάναμε ένα μέρος του data cleaning ας δούμε λίγα πράγματα

#Πλήθος χωρών που έχουμε πάρει----

length(list_of_countries)

#’ρα έχουμε πάρει 209 χώρες

#Ποιες χώρες έχουμε πάρει----
list_of_countries

#Ημερομηνία έναρξης καταγραφής δεδομένων----
as.Date(max_starting_date)

#13/5/2020

#Ημερομηνία λήξης καταγραφής δεδομένων----
as.Date(min_ending_date)

#18/6/22

#Πλήθος παρατηρήσεων για κάθε χώρα----

as.Date(min_ending_date) - as.Date(max_starting_date)

#’ρα για κάθε χώρα έχουμε 766 παρατηρήσεις

#Δομή dataset----
str(data1)

#Προς στιγμής δεν θα κάνουμε κάποια μεταβλητή string χαρακτήρα

#Μεταβλητές dataset----

names(data1)

#Πλήθος μεταβλητών dataset----

length(names(data1))

#Πλήθος Missing Value που έχει κάθε μεταβλητή στο dataset με όλες τις μεταβλητές----
colSums(is.na(data1))

#Ποσοστά των missing values στο dataset με όλες τις μεταβλητές----

(colMeans(is.na(data1)))*100

#Δημιουργία Excel με τα ποσοστά missing values που έχει η κάθε χώρα σε κάθε μεταβλητή----

#Θα υπολογίσουμε το ποσοστό των missing value που έχει κάθε χώρα στα δεδομένα
#της αποκλειστικά

#Φτιάχνουμε ένα πίνακα με τα στοιχεία αυτά
  
missing_values_dataset = matrix(0 ,nrow = length(list_of_countries),ncol = length(names(data1))-4)

for (i in 1:length(list_of_countries)) {
  
  data_sub = data1[data1$location == list_of_countries[i],]
  
  data_sub = data_sub[,-(1:4)]
  
  missing_values_dataset[i,] = (colMeans(is.na(data_sub)))*100
  
}

#Ονομάζουμε τις γραμμές με τα ονόματα των χωρών και τις στήλες με τα ονόματα
#των μεταβλητών

rownames(missing_values_dataset)=list_of_countries
colnames(missing_values_dataset)= names(data1[-(1:4)])

#Μας βολεύει να το έχουμε σαν data frame
missing_data = as.data.frame(missing_values_dataset)

#Προσθέτουμε την στήλη με τα ονόματα των χωρών(θα μπει στο τέλος)
missing_data$Country = list_of_countries

#Βάζουμε την στήλη με τα ονόματα πρώτη για λόγους παρουσίασης
missing_data = missing_data[, c(64,1:63)]

#Θέλουμε να κάνουμε export αυτό τον τύπο δεδομένων σε ένα Excel ωστε να
#είναι πιο εύκολη η κύλιση κλπ.

#Το παρακάτω βήμα δεν χρειάζεται να το κάνεις μιας και έχει αποθηκευτεί το
#Excel αρχείο με όνομα Missing Values στον φάκελο Dissertation
#write_xlsx(missing_data,"C:/Users/e-mashine/Desktop/Missing Data 1.xlsx")

#Θα φτιάξουμε έναν data frame το οποίο θα έχει τις αναλογίες των missing value
#που έχει κάθε χώρα σε κάθε μεταβλητή ως προς τον πλήθος των missing value που
#έχει αυτή η μεταβλητή συνολικά

#Φτιάχνουμε αρχικά έναν πίνακα με μηδενικά με τόσες γραμμές και στήλες όσες το
#dataset που έχουμε καταλήξει
ratio_of_missing_var = matrix(0, nrow = length(list_of_countries), ncol = length(names(data1)))

#Το μετατρέπουμε σε dataframe
ratio_of_missing_var = as.data.frame(ratio_of_missing_var)

#Δίνουμε όνομα στις στήλες
colnames(ratio_of_missing_var) = names(data1)

#Βάζουμε στην τρίτη στήλη την λίστα με τις χώρες
ratio_of_missing_var[,3] = list_of_countries

#Το πλήθος των παρατηρήσεων κάθε χαρατκτηριστικού είναι
no.obs = length(data1[,1])

for (i in 5:length(names(data1))) {

  for (j in 1:length(ratio_of_missing_var$location)) {#Το j παίρνει τιμές από
    #1 έως και 209
    
    #Εργαζόμαστε για την χώρα country
    country = ratio_of_missing_var$location[j]
    
    #Για κάθε χώρα βρες το μέρος του dataset που αντιστοιχεί σε αυτήν
    data_k = data1[data1$location == country,]
    
    #Υπολόγισε το πλήθος των missing value που έχει αυτή η χώρα σε αυτό το
    #χαρακτηριστικό
    
    l = sum(is.na(data_k[,i]))

    ratio_of_missing_var[j,i] = (l/no.obs)*100 #το κάνουμε έτσι ώστε να βγει σαν ποσοτό
    
  }
}

#Το κάνουμε eport δεν χρειάζεται να το κάνεις
#write_xlsx(ratio_of_missing_var,"C:/Users/e-mashine/Desktop/Missing Data 2.xlsx")

#Υπάρχει κάποια χώρα σε κάποιο χαρακτηριστικό-μεταβλητή να συμμετέχει ΠΑΡΑ πολύ
#στις τόσο υψηλές τιμές των missing value;

#Θα εξετάσουμε αν υπάρχει έστω και ένα ποσοστό που να είναι μεγαλύτερο από
#1%
any(ratio_of_missing_var[,-(1:4)]>1)

#Με βάση την απάντηση δεν υπάρχει. Οπότε δεν υπάρχει κάποια χώρα σε κάποιο
#χαρακτηριστικό που να συνδράμει πάρα πολύ στο πρόβλημα με τα missing value.
#’ρα, καταλήγουμε στο συμπέρασμα ότι το πρόβλημα των missing value στην
#συγκεκριμένη περίπτωση προκύπτει επειδή έχουμε πάρα πολλές χώρες με μικρά
#ποσοστά missing value σε κάθε χαρακτηριστικό.

#Επειδή το πλήθος των δεδομένων που έχουμε είναι πάρα πολύ μεγάλο 209 χώρες
#όπου για κάθε μια έχουμε 766 παρατηρήσεις και για κάθε μια έχουμε καταγράψει
#67 χαρακτηριστικά. Θα sortάρουμε τα ποσοστά των missing values και θα πάρουμε
#τις μεταβλητές που έχουν ένα υποφερτό ποσοστό.

sort((colMeans(is.na(data1)))*100)

#Με βάση τα παραπάνω αποτελέσματα θα συνεχίσουμε την ανάλυση μας με τις
#μεταβλητές: iso_code,continent,location,date,population,life_expectancy,
#total_cases,total_cases_per_million,new_cases,new_cases_per_million,
#new_cases_smoothed,new_cases_smoothed_per_million,population_density,
#diabetes_prevalence,total_deaths,total_deaths_per_million,new_deaths,
#new_deaths_per_million,new_deaths_smoothed,new_deaths_smoothed_per_million,
#median_age,aged_70_older,cardiovasc_death_rate,aged_65_older,gdp_per_capita,
#human_development_index,reproduction_rate,stringency_index,
#hospital_beds_per_thousands

#Παρατηρώ ότι στις παραπάνω μεταβλητές υπάρχουν μεταβλητές όπως new_cases
#και new_cases_per_million. Η πληροφροιες που δίνουν αυτές οι μεταβλητές είναι
#κάπως κοινή. Οποτε θεωρώ ότι δεν πρέπει να πάρω την new_cases_per_million.
#Ομοιώς για το per_hunderd άρα καταλήγουμε στις μεταβλητές:

#iso_code,continent,location,date,population,life_expectancy,
#total_cases,new_cases,population_density,diabetes_prevalence,total_deaths,
#new_deaths,median_age,aged_70_older,cardiovasc_death_rate,gdp_per_capita,
#human_development_index,reproduction_rate,stringency_index,
#hospital_beds_per_thousands

final_dataset = data1[,c("iso_code","continent","location","date","population"
                    ,"life_expectancy","total_cases","new_cases",
                    "population_density","diabetes_prevalence","total_deaths",
                    "new_deaths","median_age","aged_70_older",
                    "cardiovasc_death_rate","gdp_per_capita",
                    "human_development_index","reproduction_rate",
                    "stringency_index","hospital_beds_per_thousand")]

#Βγάζουμε τις γραμμές που έχουν έστω και ένα missing value με την μέθοδο
#na.omit

final_dataset = na.omit(final_dataset)

#’ρα η λίστα με τις χώρες που έχουμε πάρει είναι:
list_of_countries = unique(final_dataset$location)

#Δηλαδή πήραμε 145 χώρες

#Πρόκειται να εφαρμόσουμε μέθοδους πολυμεταβλητής ανάλυσης δεδομένων. Οπότε είναι καλό να
#υπολογίσουμε τις συσχετίσεις μεταξύ των μεταβλητών. Αν δούμε υψηλές συσχετίσεις αυτό
#σημαίνει ότι οι μέθοδοι που θα εφαρμοστούν θα μας δώσουν ικανοποιητικά αποτελέσματα.

#Υπολογισμός και εμφάνιση συσχετίσεων μεταξύ των μεταβλητών----

#Φορτώνουμε την απαραίτητη βιβλιοθήκη
library(corrplot)

M = cor(final_dataset[,-(1:4)])

#Επειδή δεν μας αρέσει έτσι όπως το βγάζει μεγαλώνουμε λίγο τον χώρο του plot

X11(width=55, height=35) #αυτά τα αλλάζει αν θέλεις, η αλλαγή αυτή ισχύει μόνο για αυτή την γραμμή

corrplot(M, method = 'number',number.cex = 0.7,type = "lower",addCoef.col = 1) # colorful number

#Από το παραπάνω output βλέπουμε ότι υπάρχει υψηλή συσχέτιση μεταξύ των μεταβλητών median_age - 
#life_expectancy, aged_70_older - life_expectancy, gdp_per_capita - life_expectancy,
#human_development_index - life_expectancy, total_deaths - total_cases, aged_70_older - 
#median_age,gdp_per_capita - median_age,human_development_index - median_age, hospital_beds_per_thousand -
#median_age,human_development_index - aged_70__older, hospital_beds_per_thousand - aged_70_older,
#human_development_index - gdp_per_capita

#Εφαρμογή PCA----

#Θα αποθηκεύσουμε σε μια μεταβλητή το συνολικό πλήθος των παρατηρήσεων του
#συνόλου δεδομένων

n = length(final_dataset[,1])

#Συνολικό πλήθος παρατηρήσεων 107721

#Θα αποθηκεύσουμε σε μια μεταβλητή το συνολίκο πλήθος των χαρακτηριστικών του
#συνόλου δεδομένων

p = length(final_dataset[1,]) - 4 #γιατί οι πρώτες 4 στήλες είναι: iso_code,continent,location,date

#Συνολικό πλήθος μεταβλητών του συνόλου δεδομένων 20

#Εισάγουμε την απαιτούμενη βιβλιοθήκη
library(tidyverse)

#Στην PCA θα χρησιμοποιήσουμε τον πίνακα συσχετίσεων R ώστε να μην επιδρούν
#η μονάδες μέτρησης στην ανάλυση μας

data.pca = prcomp(final_dataset[,-(1:4)], scale. = TRUE)

#Στην μεταβλητή data.pca$rotation υπάρχουν τα ιδιοδιανύσματα.Επειδή η R τα βγάζει
#από τα αρνητικά πολλαπλασιάζουμε με -1

#Πηγή stratology για τον επερχόμενο πολ/σμο

#Also note that eigenvectors in R point in the negative direction by default, so well 
#multiply by -1 to reverse the signs.

data.pca$rotation = -1*data.pca$rotation

#Επειδή τα score των παρατηρήσεων έχουν υπολογιστεί με τα παλιά ιδιοδιανύσματα πρέπει και εδώ να πολλαπλασιάσουμε με -1

#Πηγή stratology για τον επερχόμενο πολ/σμο

#Note that the principal components scores for each state are stored in results$x.
#We will also multiply these scores by -1 to reverse the signs:

data.pca$x = -1*data.pca$x

#Γενική εικόνα PCA----
summary(data.pca)

#Η πρώτη γραμμή μας δείχνει τις ρίζες των ιδιοτιμών από την μεγαλύτερη προς την #μικρότερη

#Ιδιοτιμές----
data.pca$sdev^2

#Η δεύτερη γραμμή του summary μας δείχνει το ποσοστό της μεταβλητότητας που εξηγεί κάθε κύρια συνιστώσα

#Η τρίτη γραμμή μας δείχνει το cummulative ποσοστό της μεταβλητότητας που εξηγείται σε κάθε περίπτωση

#Score των παρατηρήσεων
View(data.pca$x)

#Ποσοστά της μεταβλητότητας των αρχικών δεδομένων που ερμηνεύουν οι κύριες συνιστώσες----

percentages_of_var = ((data.pca$sdev^2) / sum(data.pca$sdev^2))*100

#Αθροιστικά ποσοστά της μεταβλητότητας των αρχικών δεδομένων που ερμηνεύουν οι κύριες συνιστώσες----

cum_percentages_of_var = ((cumsum(data.pca$sdev^2)/ sum(data.pca$sdev^2)))*100

#Scree plot----

library(ggplot2)

qplot(c(1:p),data.pca$sdev^2) + geom_line() + xlab("Component Number") +
  ylab("Eigenvalue") + ggtitle("Scree Plot") + ylim(0,max(data.pca$sdev^2)+1)+
  geom_hline(yintercept = 1,linetype = "dashed",col = "red")

#Με βάση το κριτήριο Kaiser θα πρέπει να επιλέξουμε 5 κύριες συνιστώσες

#Ποσοστό της μεταβλητότητας που εξηγείται από κάθε κύρια συνιστώσα(γράφημα)----

qplot(c(1:p), percentages_of_var) + geom_line() + xlab("Component Number") + 
  ylab("Variance Explained(%)") + ggtitle("Percentage of Variance explained by every component") +
  ylim(0, 35)


#Αθροιστικό ποσοστό της μεταβλητότητας που εξηγείται από τις κύριες συνιστώσες(γράφημα)----

qplot(c(1:p),cum_percentages_of_var) + geom_line() + xlab ("Principal Component")+
  ylab("Cumulative Variance Explained(%)") + ggtitle("Cumulative percentage of Variance explained by components") + ylim(0,100)

#Υπολογισμός των συσχετίσεων μεταξύ των αρχικών μεταβλητών και των score----
cor.variables.pcs = cor(final_dataset[,-(1:4)],data.pca$x,method = c("pearson"))

#Τώρα θα δούμε αν υπάρχει κάποια συσχέτιση μεγαλύτερη ή ίση κατά απόλυτη τιμή
#από ένα a που το ορίζουμε εμείς

a = 0.40

indices = which(t(t(abs(cor.variables.pcs))) >= a, arr.ind=TRUE)
indices[,"row"] = rownames(cor.variables.pcs)[as.numeric(indices[,"row"])]
indices[,"col"] = colnames(cor.variables.pcs)[as.numeric(indices[,"col"])]

cbind(indices,cor.variables.pcs[indices])

#Με βάση τα παραπάνω αποτελέσματα βλέπουμε ότι:

#H πρώτη κύρια συνιστώσα είναι ισχυρά αρνητικά συσχετισμένη με τις μεταβλητές: life_expectancy,median_age,aged_70_older,
#human_development_index,και ασθενέστερα αρνητικά συσχετισμένη με τις μεταβλητές gdp_per_capita,hospital_beds_per_thousands.
#Επιπλέον, είναι θετικά συσχετισμένη με την μεταβλητή cardiovasc_death_rate.

#Η δεύτερη κύρια συνιστώσα είναι ισχυρά αρνητικά συσχετισμένη με τις μεταβλητές total_cases,total_deaths,new deaths. Επιπλέον,
#είναι ασθενέστερα αρνητικά συσχετισμένη με τις μεταβλητές population,new_cases.

#Η τρίτη κύρια συνιστώσα είναι αρνητικά συσχετισμένη με τις μεταβλητές: population_density,diabetes_prevalence.Επιπλέον,
#είναι θετικά συσχετισμένη με τις μεταβλητές aged_70_older,hospital_beds_per_thousand

#Η τέταρτη κύρια συνιστώσα είναι αρνητικά συσχετισμένη με την μεταβλητή reproduction_rate και ακόμη πιο ισχυρά αρνητικά
#συσχετισμένη με την μεταβλητή stringency_index.

#H πέμπτη κύρια συνιστώσα είναι αρνητικά συσχετισμένη με την μεταβλητή diabetes_prevalence,cardiovasc_death_rate

#Υπολογισμός ποιότητας αναπαράστασης όταν παίρνουμε 5 κύριες συνιστώσες----

k = 5 #η μεταβλητή αυτή μου κρατάει το πλήθος των κύριων συνιστωσών που έχω πάρει

#Συμβολίζουμε με f το διάνυσμα (f_1,f_2,...,f_k)
f = matrix(0, nrow = n, ncol = k)

for (i in 1:k) {
  
  f[,i] = (scale(final_dataset[,-(1:4)],scale = TRUE)) %*% (data.pca$rotation[,i])
  
  #scale(final_dataset[,-(1:4),scale = TRUE) είναι ο πίνακας Z. Δηλαδή τα
  #τυποποιημένα αρχικά δεδομένα
  
  #data.pca$rotation[,i] είναι το i-οστό ιδιοδιάνυσμα
  
}

#Υπολογίζουμε την ποσότητα (f_{i1}^2) + (f_{i2}^2) + ... + (f_{1k}^2)

f_star = rowSums(f^2)

#Υπολογίζουμε το μέτρο κάθε διανύσματος z_i

magnitude_of_vectors_sq = rowSums((scale(final_dataset[,-(1:4)],scale = TRUE))^2)

#Κάνουμε την απαιτούμενη διαίρεση για να υπολογίσουμε την ποιότητα αναπαράστασης

q.x_i = (f_star/magnitude_of_vectors_sq)*100 #πολ/ζουμε με 100 για να βγει σε μορφή ποσοστού

#Υπολογίζουμε τον πλήθος των παρατηρήσεων των οποίων η ποιότητα είναι παραπάνω
#από ένα όριο a το οποίο το θέτουμε εμείς

a = 60

number_of_obs_very_well_represented = length(q.x_i[q.x_i>a])

#Το ποσοστό αυτών είναι:
(number_of_obs_very_well_represented/n)*100

#Ιστόγραμμα της ποιότητας αναπαράστασης των παρατηρήσεων----

ggplot() + geom_histogram(aes(q.x_i), bins = 50, col = "black",fill = "cadetblue1") + xlab("Quality of Representation(%)") + 
  ylab("Counts") + ggtitle("Histogram of Quality of Representation")

#Υπολογισμός της συμβολής της κάθε παρατήρησης στην διαμόρφωση του επιπέδου με των πρώτων 5 κύριων συνιστωσών----

ct = (f_star/sum((f_star)))*100 #για να βγει σε μορφή ποσοστού

#PCA κατασκευή hetamaps----

scores.all.countries = cbind(final_dataset[,c(2,3,4)],data.pca$x)

library(ggplot2)
library(plotly)
library(scales)

#Για την πρώτη κύρια συνιστώσα----

#Για την Ευρώπη----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Europe",])$PC1),
          to = max((scores.all.countries[scores.all.countries$continent == "Europe",])$PC1),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PC1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the first PC")
   
 ggplotly(p)
 
# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες: Ουκρανία, Μολδαβία, Βοζνία
# Ερζεγοβίνη, Αλβανία λαμβάνουν υψηλές τιμές στην πρώτη κύρια συνιστώσα με
# την Μολδαβία να παίρνει την υψηλότερη. Από την άλλη μεριά, βλέπουμε ότι
# οι χώρες: Ιταλία, Γερμανία, Γαλλία παίρνουν χαμηλές τιμές στην πρώτη κύρια
# συνιστώσα.
 
#Για την Ασία----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Asia",])$PC1),
          to = max((scores.all.countries[scores.all.countries$continent == "Asia",])$PC1),length.out = 4)
 
p = scores.all.countries %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PC1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the first PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες: Υεμένη, Πακιστάν, Αφγανιστάν
# λαμβάνουν υψηλές τιμές στην πρώτη κύρια συνιστώσα. ΑΠό την άλλη μεριά, η
# Νότια Κορέα, η Σινγκαπούρη, η Ιαπωνία λαμβάνουν χαμηλές τιμές στην πρώτη
# κύρια συνιστώσα.

#Για την Νότια Αμερική----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "South America",])$PC1),
          to = max((scores.all.countries[scores.all.countries$continent == "South America",])$PC1),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PC1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries of South America in the first PC")

ggplotly(p)

# Από το παραπάνω γράφημ βλέπουμε ότι οι χώρες: Γουιάνα, Βολιβία λαμβάνουν υψηλές
# τιμές στην πρώτη κύρια συνιστώσα. Από την άλλη μεριά, οι χώρες: Ουρουγουάι,
# Βραζιλία, Αργεντινή λαμβάνουν χαμηλές τιμές στην πρώτη κύρια συνιστώσα.

#Για την Βόρεια Αμερική----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "North America",])$PC1),
          to = max((scores.all.countries[scores.all.countries$continent == "North America",])$PC1),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PC1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries of North America in the first PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι η Αϊτή λαμβάνει υψηλές τιμές στην πρώτη κύρια
# συνιστώσα. Από την άλλη μεριά, βλέπουμε ότι οι Η.Π.Α λαμβάνουν χαμηλές τιμές στην
# πρώτη κύρια συνιστώσα. Επιπλεόν, βλέπουμε ότι οι χώρες: Μεξικό, Καναδάς λαμβάνουν
# σχετικά υψηλές τιμές στην πρώτη κύρια συνιστώσα.

#Για την Ωκεανία----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC1),
          to = max((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC1),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PC1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries of Oceania in the first PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι το νησιωτικό κράτος Φίτζι από τον Απρίλιο
# του 2021 και μετά λαμβάνει πολύ υψηλές τιμές στην πρώτη κύρια συνιστώσα. Από την
# άλλη μεριά, η Νέα Ζηλανδία και η Αυστραλία λαμβάνουν χαμηλές τιμές στην πρώτη
# κύρια συνιστώσα.

#Για την Αφρική----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Africa",])$PC1),
          to = max((scores.all.countries[scores.all.countries$continent == "Africa",])$PC1),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PC1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the first PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες: Ζιμπάμπουε, Ζάμπια, Ουγκάντα, Τονγκό,
# Τανζανία, Σουδάν, Νιγηρία, Μοζαμβίκη, Μαλάουι, Μαγαδασκάρη, Λιβερία, Γουινέα, Γκάμπια,
# Αιθιοπία, Σουαζιλάνδη, Τζιμπουτί, Καμερούν, Μπουρούντι, Μπουρκίνα Φάσο, Μπενίν
# λαμβάνουν σχετικά υψηλές τιμές στην πρώτη κύρια συνιστώσα . Επιπλέον, βλέπουμε ότι οι
# χώρες: Μάλι, Κεντροαφρικανική Δημοκρατία λαμβάνουν πολύ υψηλές τιμές στην πρώτη κύρια
# συνιστώσα.Ακόμη,βλέπουμε ότι οι χώρες: Νότια Αφρική, Μαρόκο, Λιβύη, Γκαμπόν, Αίγυπτος,
# Πράσινο Ακρωτήρι (Cape Verde), Μποτσουάνα, Αλγερία λαμβάνουν σχετικά χαμηλές τιμές στην
# πρώτη κύρια συνιστώσα.Τέλος, βλέπουμε ότι οι χώρες:Μαυρίκιος, Σευχέλλες λαμβάνουν πολύ
# χαμηλές τιμές στην πρώτη κύρια συνιστώσα.


#Για την δεύτερη κύρια συνιστώσα----

#Για την Ευρώπη----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Europe",])$PC2),
          to = max((scores.all.countries[scores.all.countries$continent == "Europe",])$PC2),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PC2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the second PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι όλες οι Ευρωπαϊκές χώρες αρχικά λάμβαναν
# πολύ υψηλές τιμές στην δεύτερη κύρια συνιστώσα.Καθώς παιρνάει ο χρόνος βλέπουμε
# ότι τα score των χωρών: Ηνωμένο Βασίλειο, Ρωσία, Ιταλία, Γερμανία, Γαλλία
# μειώνονται ενώ τα score των υπολοίπων χωρών παραμένουν σχετικά σταθερά.

#Για την Ασία----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Asia",])$PC2),
          to = max((scores.all.countries[scores.all.countries$continent == "Asia",])$PC2),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PC2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the second PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι όλες οι Ασιατικές χώρες εκτός από την Ινδία
# λαμβάνουν αρκετά υψηλές τιμές στην δεύτερη κύρια συνιστώσα.Από την άλλη μεριά, η
# Ινδία λαμβάνει αισθητά μικρότερες τιμές από ότι οι υπόλοιπες Ασιατικές χώρες και
# επιπλέον βλέπουμε ότι τον Μάϊο του 2021 τα score της Ινδίας πήραν πολύ μικρές
# τιμές

#Για την Νότια Αμερική----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "South America",])$PC2),
          to = max((scores.all.countries[scores.all.countries$continent == "South America",])$PC2),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PC2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the second PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι όλες οι χώρες της Νότιας Αμερικής εκτός από το
# Περού και την Βραζιλία λαμβάνουν αρκετά υψηλές τιμές στην δεύτερη κύρια συνιστώσα.
# Από την άλλη μεριά, το Περού και η Βραζιλία λαμβάνουν συγκριτικά χαμηλότερες τιμές
# από ότι οι υπόλοιπες χώρες της Νότιας Αμερικής. Επιπλέον, βλέπουμε ότι η Βραζιλία
# τον Απρίλιο του 2021, τον Ιούνιο του 2021, τον Φεβρουάριο του 2022 έλαβε πολύ μικρές
# τιμές στην δεύτερη κύρια συνιστώσα.

#Για την Βόρεια Αμερική----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "North America",])$PC2),
          to = max((scores.all.countries[scores.all.countries$continent == "North America",])$PC2),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PC2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the second PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι όλες οι χώρες της Βόρειας Αμερικής εκτός από
# τις Η.Π.Α λαμβάνουν αρκετά υψηλές τιμές στην δεύτερη κύρια συνιστώσα.Από την άλλη
# μεριά, βλέπουμε ότι οι Η.Π.Α λαμβάνουν συγκριτικά χαμηλότερες τιμές από ότι οι
# υπόλοιπες χώρες της Βόρειας Αμερικής και ότι τον Ιανουάριο του 2022 οι Η.Π.Α
# έλαβαν πολύ μικρές τιμές στην δεύτερη κύρια συνιστώσα.

#Για την Ωκεανία----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC2),
          to = max((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC2),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PC2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the second PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες: Νέα Ζηλανδία, Αυστραλία αρχικά λαμβάνουν
# υψηλές τιμές στην δεύτερη κύρια συνιστώσα. Επιπλέον, βλέπουμε ότι από τον Ιανουάριο του
# 2022 και μετά τα score της Αυστραλίας μειώνονται αισθητά.Τέλος, βλέπουμε ότι το
# νησιωτικό κράτος Φίτζι από τον Απρίλιο του 2021 και μετά λαμβάνει σχετικά υψηλές τιμές
# στην δεύτερη κύρια συνιστώσα.

#Για την Αφρική----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Africa",])$PC2),
          to = max((scores.all.countries[scores.all.countries$continent == "Africa",])$PC2),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PC2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the second PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες: Τυνησία, Σευχέλλες, Μαυρίκιος, Λιβύη,
# Γκαμπόν, Πράσινο Ακρωτήρι (Cape Verde), Μπουτσουάνα, Αλγερία λαμβάνουν αρκετά υψηλές
# τιμές στην δεύτερη κύρια συνιστώσα. Από την άλλη μεριά, βλέπουμε ότι η Νότια Αφρική
# στην αρχή λαμβάνει αρκετά υψηλές τιμές οι οποίες μετά μειώνονται σημαντικά.

#Για την τρίτη κύρια συνιστώσα----

#Για την Ευρώπη----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Europe",])$PC3),
          to = max((scores.all.countries[scores.all.countries$continent == "Europe",])$PC3),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PC3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the third PC")

ggplotly(p)

# Από το παραπάνω γράφημα μπορούμε να δούμε ότι χώρες: Ουκρανία, Ρωσία, Μολδαβία,
# Λιθουανία, Λετονία, Εστονία, Κροατία, Βουλγαρία λαμβάνουν αρκετά υψηλές τιμές
# στην τρίτη κύρια συνιστώσα. Επιπλέον, βλέπουμε ότι η Λευκορωσία λαμβάνει πολύ
# υψηλές τιμές στην τρίτη κύρια συνιστώσα. Ακόμη, βλέπουμε ότι οι χώρες: Ηνωμένο
# Βασίλειο, Ελβετία, Σουηδία, Ισπανία, Σλοβενία, Σλοβακία, Ιταλία, Φιλανδία,
# Βοζνία Ερζεγοβίνη, Βέλγιο, Αλβανία λαμβάνουν σχετικά χαμηλές τιμές στην τρίτη
# κύρια συνιστώσα. Τέλος, βλέπουμε ότι η Μάλτα και η Κύπρος λαμβάνουν πολύ χαμηλές
# τιμές στην τρίτη κύρια συνιστώσα.

#Για την Ασία----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Asia",])$PC3),
          to = max((scores.all.countries[scores.all.countries$continent == "Asia",])$PC3),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PC3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the third PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες: Ουζμπεκιστάν, Τατζικιστάν, Νότια
# Κορέα, Μονγκολία, Ιαπωνία λαμβάνουν πολύ υψηλές τιμές στην τρίτη κύρια συνιστώσα.
# Επιπλέον, βλέπουμε ότι οι χώρες: Υεμένη, Βιετνάμ, Τιμόρ, Ταϊλάνδη, Σιρι Λάνκα,
# Φιλλιπίνες, Πακιστάν, Νεπάλ, Μιανμάρ (Βιρμανία), Λάος, Κιργιζία, Καζακστάν, Ισραήλ,
# Ιράκ, Ιράν,Ινδονησία, Κίνα, Καμπότζη, Μπουτάν, Αζερμπαϊτζάν, Αφγανιστάν ελάβαν
# σχετικά υψηλές τιμές στην τρίτη κύρια συνιστώσα. Ακόμη, βλέπουμε ότι οι χώρες:
# Ηνωμένα Αραβικά Εριμάτα, Σαουδική Αραβία, Κατάρ, Κουβέιτ, Μπαχρέιν έλαβαν σχετικά
# χαμηλές τιμές στην τρίτη κύρια συνιστώσα. Τέλος, βλέπουμε ότι η Σινγκαπούρη έλαβε
# πολύ χαμηλές τιμές στην τρίτη κύρια συνιστώσα.

#Για την Νότια Αμερική----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "South America",])$PC3),
          to = max((scores.all.countries[scores.all.countries$continent == "South America",])$PC3),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PC3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the third PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι τα score όλων των χωρών της Νότιας Αμερικής
# εκτός από την Ουρουγουάι και την Αργεντινή αρχικά λαμβάνουν χαμηλές τιμές στην
# τρίτη κύρια συνιστώσα. Η Ουρουγουάι και η Αργεντινή βλέπουμε ότι αρχικά λαμβάνουν
# σχετικά χαμηλές τιμές στην τρίτη κύρια συνιστώσα. Τέλος, παρατηρούμε ότι σε όλες
# τις χώρες της Νότιας Αμερικής με την πάροδο του χρόνου εμφανίζεται μια αύξηση των
# score τους στην τρίτη κύρια συνιστώσα με τα score της Αργεντινής να πιάνουν πολύ
# υψηλές τιμές.

#Για την Βόρεια Αμερική----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "North America",])$PC3),
          to = max((scores.all.countries[scores.all.countries$continent == "North America",])$PC3),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PC3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the third PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι τα score των Η.Π.Α στην τρίτη κύρια συνιστώσα
# αρχικά είναι σχετικά χαμηλά και ότι με την πάροδο του χρόνου αυξάνονται. Επιπλέον,
# βλέπουμε ότι τον Ιανουάριο του 2022 τα score των Η.Π.Α έλαβαν πολύ υψηλές τιμές
# στην τρίτη κύρια συνιστώσα. Επιπλέον, βλέπουμε ότι τα score του Μεξικό στην αρχή
# λαμβάνουν πολύ χαμηλές τιμές στην τρίτη κύρια συνιστώσα και ότι με την πάραοδο του
# χρόνου αυξάνονται ελαφρώς. Ακόμη, βλέπουμε ότι τα score της Μπελίζ λαμβάνουν πολύ
# χαμηλές τιμές στην τρίτη κύρια συνιστώσα. Επιπροσθέτως, βλέπουμε τα score της Αϊτής
# αρχικά λαμβάνουν σχετικά χαμηλές τιμές και με την πάροδο του χρόνου αυξάνονται και
# φτάνουν στο σημείο να λαμβάνουν σχετικά υψηλές τιμές στην τρίτη κύρια συνιστώσα.
# Τέλος, βλέπουμε ότι τα score όλων των υπολοίπων χωρών της Νότιας Αμερικής λαμβάνουν
# σχετικά χαμηλές τιμές στην τρίτη κύρια συνιστώσα.

#Για την Ωκεανία----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC3),
          to = max((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC3),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PC3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the third PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι τα score της Αυστραλίας στην τρίτη κύρια συνιστώσα
# αρχικά λαμβάνουν σχετικά υψηλές τιμές και με την πάροδο του χρόνου λαμβάνουν πολύ
# υψηλές τιμές. Επιπλέον, βλέπουμε ότι τα score της Νέας Ζηλανδίας στην τρίτη κύρια
# συνιστώσα αρχικά λαμβάνουν σχετικά υψηλές τιμές και ότι τον Αύγουστο του 2021 λαμβάνουν
# πολύ χαμηλές τιμές.Μετά από τον Αύγουστο του 2021, βλέπουμε ότι τα score της Νέας
# Ζηλανδίας αυξάνονται σημαντικά και φτάνουν στο σημείο να παίρνουν αρκετά υψηλές τιμές
# στην τρίτη κύρια συνιστώσα. Επιπροσθέτως, βλέπουμε ότι τα score του νησιωτικού κράτους
# Φίτζι μέχρι και τον Ιανουάριο του 2022 λαμβάνουν πολύ χαμηλές τιμές στην τρίτη κύρια
# συνιστώσα. Από τον Ιανουάριο του 2022 και μετά, βλέπουμε ότι τα score του νησιωτικού
# κράτους Φίτζι στην τρίτη κύρια συνιστώσα εμφανίζουν σημαντική αύξηση.

#Για την Αφρική----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Africa",])$PC3),
          to = max((scores.all.countries[scores.all.countries$continent == "Africa",])$PC3),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PC3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the third PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες: Σουδάν, Σευχέλλες, Λιβύη, Αιθιοπία,
# Αίγυπτος λαμβάνουν σχετικά χαμηλές τιμές στην τρίτη κύρια συνιστώσα. Επιπλέον,
# βλέπουμε ότι τα score της χώρας Μαυρίκιος στην τρίτη κύρια συνιστώσα μέχρι τον
# Μάρτιο του 2021 ήταν σχετικά χαμηλά και ότι μετά τον Μάρτιο του 2021 ήταν πολύ
# χαμηλά. Τέλος, βλέπουμε ότι τα score των υπολοίπων χωρών της Αφρικής στην τρίτη
# κύρια συνιστώσα παίρνουν από σχετικά υψηλές έως και πολύ υψηλές τιμές.

#Για την τέταρτη κύρια συνιστώσα----

#Για την Ευρώπη----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Europe",])$PC4),
          to = max((scores.all.countries[scores.all.countries$continent == "Europe",])$PC4),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PC4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the fourth PC")

ggplotly(p)

# Παρατηρώντας το παραπάνω γράφημα βλέπουμε ότι τα score των περισσότερων Ευρωπαϊκών
# χωρών αρχικά λαμβάνουν σχετικά χαμηλές τιμές στην τέταρτη κύρια συνιστώσα και με
# την πάροδο του χρόνου λαμβάνουν υψηλότερες τιμές.

#Για την Ασία----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Asia",])$PC4),
          to = max((scores.all.countries[scores.all.countries$continent == "Asia",])$PC4),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PC4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the fourth PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι τα score της Σινγκαπούρης στην τέταρτη κύρια
# συνιστώσα λαμβάνουν πολύ υψηλές τιμές. Επιπλέον, βλέπουμε ότι τα score των χωρών:
# Μπρουνέι, Μπαχραίν στην τέταρτη κύρια συνιστώσα λαμβάνουν σχετικά υψηλές τιμές.
# Τέλος, βλέπουμε ότι τα score όλων των υπόλοιπων χωρών της Ασίας στην τέταρτη κύρια
# συνιστώσα λαμβάνουν σχετικά χαμηλές τιμές

#Για την Νότια Αμερική----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "South America",])$PC4),
          to = max((scores.all.countries[scores.all.countries$continent == "South America",])$PC4),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PC4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the fourth PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε οτι τα score όλων των χωρών της Νότιας Αμερικής 
# αρχικά λαμβάνουν σχετικά χαμηλές τιμές στην τέταρτη κύρια συνιστώσα και ότι με
# την πάροδο του χρόνου οι τιμές αυτών των score αυξάνονται. Επιπλέον, βλέπουμε ότι
# οι χώρες: Ουρουγουάι, Παραγουάι, Κολομβία, Βραζιλία, Αργεντινή καταφέρνουν να
# πιάσουν πολύ υψηλές τιμές στην τέταρτη κύρια συνιστώσα τον Μάϊο του 2022.

#Για την Βόρεια Αμερική----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "North America",])$PC4),
          to = max((scores.all.countries[scores.all.countries$continent == "North America",])$PC4),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PC4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the fourth PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι τα score όλων των χωρών της Βόρειας Αμερικής
# στην τέταρτη κύρια συνιστώσα αρχικά λαμβάνουν πολύ χαμηλές τιμές και ότι με την
# πάροδο του χρόνου αυξάνονται. Επιπλέον, βλέπουμε ότι οι Η.Π.Α καταφέρνουν να
# λάβουν πολύ υψηλές τιμές στην τέταρτη κύρια συνιστώσα από τον Μάρτιο του 2022 και
# μετά.

#Για την Ωκεανία----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC4),
          to = max((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC4),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PC4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the fourth PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι τα score της Νέας Ζηλανδίας στην τέταρτη κύρια
# συνιστώσα αρχικά λαμβάνουν πολύ υψηλές τιμές. Επιπλέον, βλέπουμε ότι τον Αύγουστο
# του 2021 η Νέα Ζηλανδία πήρε πολύ μικρές τιμές στην τέταρτη κύρια συνιστώσα και
# ότι από τον Αύγουστο του 2021 έως και τον Μάρτιο του 2022 πήρε σχετικά χαμηλές
# τιμές. Από τον Μάρτιο του 2022 και μετά βλέπουμε ότι υπήρξε σημαντική αύξηση στα
# score της Νέα Ζηλανδίας στην τέταρτη κύρια συνιστώσα και ότι οι τιμές των score ήταν
# σχετικά υψηλές. Επιπλέον, βλέπουμε ότι το νησιωτικό κράτος Φίτζι πήρε σχετικά υψηλές
# τιμές στην τέταρτη κύρια συνιστώσα με εξαίρεση την περίοδο Δεκέμβριος του 2021 έως
# και αρχές Ιανουαρίου του 2022 στην οποία πήρε πολύ χαμηλές τιμές. Επίσης, βλέπουμε
# ότι η Αυστραλία μέχρι και τον Ιανουάριο του 2022 λάμβανε σχετικά υψηλές τιμές στην
# τέταρτη κύρια συνιστώσα και ότι από τον Ιανουάριο του 2022 και μετά οι τιμές αυτές
# εμφάνισαν αισθητή αύξηση.

#Για την Αφρική----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Africa",])$PC4),
          to = max((scores.all.countries[scores.all.countries$continent == "Africa",])$PC4),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PC4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the fourth PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι η Τανζανία καθόλη την πάροδο του χρόνου
# λάμβανε πολύ υψηλές τιμές στην τέταρτη κύρια συνιστώσα. Επιπλέον, βλέπουμε
# ότι όλες οι υπόλοιπες χώρες της Αφρικής λαμβάνουν σχετικά υψηλές τιμές στην
# τέταρτη κύρια συνιστώσα.

#Για την πέμπτη κύρια συνιστώσα----

#Για την Ευρώπη----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Europe",])$PC5),
          to = max((scores.all.countries[scores.all.countries$continent == "Europe",])$PC5),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PC5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the fifth PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες: Ουκρανία, Ρωσία, Ρουμανία, Βουλγαρία,
# Λευκορωσία παίρνουν πολύ χαμηλές τιμές στην πέμπτη κύρια συνιστώσα. Επιπλέον,
# βλέπουμε ότι οι χώρες: Σλοβακία, Πολωνία, Μολδαβία, Μάλτα, Λιθουανία, Λετονία,
# Ουγκαρία, Γερμανία, Τσεχία, Βοσνία Ερζεγοβίνη, Αλβανία παίρνουν σχετικά χαμηλές
# τιμές στην πέμπτη κύρια συνιστώσα. Τέλος, βλέπουμε ότι όλες οι υπόλοιπες Ευρωπαϊκές
# χώρες παίρνουν σχετικά υψηλές τιμές στην πέμπτη κύρια συνιστώσα.

#Για την Ασία----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Asia",])$PC5),
          to = max((scores.all.countries[scores.all.countries$continent == "Asia",])$PC5),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PC5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the fifth PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες: Ουζμπεκιστάν, Ηνωμένα Αραβικά Εμιράτα,
# Σαουδική Αραβία, Κίνα, Αζερμπαϊτζάν, Αφγανιστάν παίρνουν πολύ χαμηλές τιμές στην
# πέμπτη κύρια συνιστώσα. Επιπλέον, βλέπουμε ότι την μεγαλύτερη διάρκεια του χρόνου η
# χώρα Τατζικιστάν πήρε πολύ χαμηλές τιμές στην πέμπτη κύρια συνιστώσα εκτός από δυο
# περιόδους στις οποίες πήρε σχετικά χαμηλές τιμές. Ακόμη, βλέπουμε ότι η Ινδία για
# πολύ μεγάλο χρονικό διάστημα έπαιρνε πολύ χαμηλές τιμές στην πέμπτη κύρια συνιστώσα
# εκτός από κάποιες μικρές περιόδους που πήρε σχετικά υψηλέ τιμές. Επιπροσθέτως,
# βλέπουμε ότι οι χώρες: Βιετνάμ, Ταϊλάνδη, Νεπάλ, Βιρμανία, Λάος, Ισραήλ, Ιράκ, Ιράν,
# Ινδονησία, Καμπότζη παίρνουν πολύ υψηλές τιμές στην πέμπτη κύρια συνιστώσα. Τέλος,
# βλέπουμε ότι όλες οι υπόλοιπες Ασιατικές χώρες παίρνουν σχετικά χαμηλές τιμές στην
# πέμπτη κύρια συνιστώσα.

#Για την Νότια Αμερική----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "South America",])$PC5),
          to = max((scores.all.countries[scores.all.countries$continent == "South America",])$PC5),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PC5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the fifth PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι η Γουιάνα και η Σουρινάμ για μεγάλο χρονικό
# διάστημα παίρνουν πολύ χαμηλές τιμές στην πέμπτη κύρια συνιστώσα. Από την άλλη
# μεριά, βλέπουμε ότι το Περού παίρνει πολύ υψηλές τιμές στην πέμπτη κύρια
# συνιστώσα. Τέλος, βλέπουμε ότι όλες οι υπόλοιπες χώρες της Νότιας Αμερικής
# παίρνουν σχετικά υψηλές τιμές στην πέμπτη κύρια συνιστώσα και ότι έχουν κάποιες
# περιόδους στις οποίες παίρνουν σχετικά χαμηλές τιμές.

#Για την Βόρεια Αμερική----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "North America",])$PC5),
          to = max((scores.all.countries[scores.all.countries$continent == "North America",])$PC5),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PC5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the fifth PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι οι χώρες: Η.Π.Α, Τρινιντάντ και Τομπάγκο,
# Μεξικό, Τζαμαικα, Αϊτή, Γρενάδα, Δομινικανή Δημοκρατία, Μπελίζ,Μπαρμπάντος,
# Μπαχάμες παίρνουν πολύ χαμηλές τιμές στην πέμπτη κύρια συνιστώσα. Από την άλλη
# μεριά, βλέπουμε ότι όλες οι υπόλοιπες χώρες της Βόρειας Αμερικής παίρνουν
# σχετικά χαμηλές τιμές στην πέμπτη κύρια συνιστώσα.

#Για την Ωκεανία----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC5),
          to = max((scores.all.countries[scores.all.countries$continent == "Oceania",])$PC5),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PC5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the fifth PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι η Αυστραλία και η Νέα Ζηλανδία παίρνουν
# σχετικά υψηλές τιμές στην πέμπτη κύρια συνιστώσα ενώ το νησιωτικό κράτος
# Φίτζι παίρνει πολύ χαμηλές τιμές.


#Για την Αφρική----

brk = seq(from = min((scores.all.countries[scores.all.countries$continent == "Africa",])$PC5),
          to = max((scores.all.countries[scores.all.countries$continent == "Africa",])$PC5),length.out = 4)

p = scores.all.countries %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PC5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the fifth PC")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι η Αίγυπτος και η χώρα Μαυρίκιος παίρνουν πολύ
# χαμηλές τιμές στην πέμπτη κύρια συνιστώσα. Επιπλέον, βλέπουμε ότι οι χώρες:
# Τυνισία, Τανζανία, Σουδάν, Σεϋχέλλες, Μαρόκο, Λιβύη, Γκαμπότζη, Κεντροαφρικανική
# Δημοκρατία παίρνουν σχετικά χαμηλές τιμές στην πέμπτη κύρια συνιστώσα. Τέλος,
# βλέπουμε ότι όλες οι υπόλοιπες Αφρικανικές χώρες παίρνουν σχετικά υψηλές τιμές στην
# πέμπτη κύρια συνιστώσα.

#Factor Analysis----

#Υπολογισμός του KMO

#Φορτώνουμε πρώτα την βιβλιοθήκη
library(psych)

KMO(final_dataset[,-(1:4)])

#Η τιμή του KMO είναι 75% η οποία είναι αρκετά κοντά στο 80%. Αυτό αποτελεί καλό οιωνό
#για το γεγονός ότι η Factor Analysis θα δώσει ικανοποιητικά αποτελέσματα.

#Factor Analysis with pca, 5 factors, no rotation----

#Επειδή στην PCA πήραμε 5 κύριες συνιστώσες στην Factor Aanlysis θα πάρουμε 5 παράγοντες.
#Προς στιγμής δεν θα κάνουμε κάποια περιστροφή και θα χρησιμοποιήσουμε τον R ώστε να μην
#επηρεάσουν τα αποτελέσματα οι μονάδες μέτρησης.

data.fa.pca = fa(r = cor(final_dataset[,-(1:4)]), nfactors = 5, rotate = "none",fm = "pa")

#Αν βάλλω 6 τότε έχω το παρακάτω μήνυμα
#maximum iteration exceeded
#Warning messages:
#  1: In fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs,  :
#                   The estimated weights for the factor scores are probably incorrect.  Try a different factor score estimation method.
#                 2: In fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate,  :
#                             An ultra-Heywood case was detected.  Examine the results carefully

#Factor Analysis with pca results----

data.fa.pca

#Με την PCA με τις 5 κύριες συνιστώσες εξηγείται το 59% της συνολικής μεταβλητότητας των
#αρχικών δεδομένων.

#Οι πρώτες στήλες PA1,PA2,....,PA5 περιέχουν τα φορτία των παραγόντων. Δηλαδή το 0.04 είναι
#το φορτίο του πρώτου παράγοντα που αντιστοιχεί στην μεταβλητή population. Το 0.41 είναι
#το φορτίο του δεύτερου παράγοντα που αντιστοιχεί στην μεταβλητή population κ.ο.κ.

#Επειδή χρησιμοποιούμε τον πίνακα R αυτά τα φορτία εκφράζουν τον συντελεστή συσχέτισης του
#αντίστοιχου παράγοντα με την αντίστοιχη μεταβλητή. ’ρα ο συντελεστής γραμμικής συσχέτισης
#μεταξύ του population και του πρώτου παράγοντα είναι 0.04 κ.ο.κ. Βλέπουμε ότι ο πρώτος
#παράγοντας είναι ισχυρά και θετικά συσχετισμένος με τις μεταβλητές: life_expectancy,
#median_age,aged_70_older,human_development_index. Ο δεύτερος παράγοντας είναι ισχυρά
#και θετικά συσχετισμένος με τις μεταβλητές: total_cases,total_deaths και λιγότερο ισχυρά
#και θετικά με την μεταβλητή new_deaths.Ο τρίτος παράγοντας γενικά δεν έχει πάρα πολύ
#υψηλές συσχετίσεις.Παρόλαυτα βλέπουμε ότι είναι θετικά συσχετισμένος με την μεταβλητή
#diabetes_prevalence και αρνητικά συσχετισμένος με τις μεταβλητές:aged_70_older,
#cardiovasc_death_rate,hospital_beds_per_thousand.Ο τέταρτος παράγοντας δεν έχει υψηλές
#συσχετίσεις με τις αρχικές μεταβλητές.Παρόλαυτα, είναι θετικά συσχετισμένος με την
#μεταβλητή diabetes_prevalence και με την cardiovasc_death_rate. Ο πέμπτος παράγοντας
#δεν έχει πάρα πολύ ισχυρές συσχετίσεις με τις αρχικές μεταβλητές. Παρόλαυτα εμφανίζει
#μια θετική συσχέτιση με την μεταβλητή stringency_index

#Η στήλη h2 περιέχει τα ποσοστά της μεταβλητότητας των αρχικών μεταβλητών που εξηγούνται
#από το μοντέλο που μόλις φτιάξαμε.Παρατηρούμε ότι οι μεταβλητές που εξηγούνται
#ικανοποιητικά από το μοντέλο είναι: life_expectancy,total_cases,total_deaths,median_age,
#aged_70_older,cardiovasc_death_rate,human_development_index.Οι μεταβλητές που δεν
#αξηγούνται ικανοποιητικά από το μοντέλο είναι: population,population_density,
#reproduction_rate,stringency_index.

#Η στήλη u2 περιέχει τα ποσοστά της μεταβλητότητας των αρχικών μεταβλητών που ΔΕΝ
#εξηγούνται από το μοντέλο που μόλις φτιάξαμε.

#Η στήλη com περιέχει της κοινές διασπορές δηλαδή τις μεταβλητότητες των αρχικών μεταβλητών
#που εξηγούνται από το μοντέλο που μόλις φτιάξαμε.

#Η σειρά SS loadings περιέχει τα αθροίσματα τετραγώνων των φορτίων του κάθε παράγοντα.
#Δηλαδή το 4.75 είναι το άθροισμα των τετραγώνων των φορτίων του πρώτου παράγοντα.

#Η σειρά Proportion Var περιέχει τα ποσοστά της μεταβλητότητας των αρχικών δεδομένων
#που εξηγούνται από το μοντέλο. Από κάτω υπάρχουν τα αθροιστικά ποσοστά.

#Η στήλη com περιέχει το πόσοι παράγοντες συνεισφέρουν σε μια μεταβλητή

#Υπολογισμός διαφοράς μεταξύ αναπραγόμενου και πίνακας συσχετίσεων

data.fa.pca$residual

#Factor Analysis with MLE, no rotation----

data.fa.mle = fa(r = cor(final_dataset[,-(1:4)]), nfactors = 5, rotate = "none",fm = "ml")

#Factor Analysis with MLE results----

data.fa.mle

#Το ποσοστό της μεταβλητότητας των αρχικών δεδομένων που εξηγούν οι 5 παράγοντες είναι 57%
#το οποίο δεν διαφέρει και πάρα πολύ από το 59% που εξηγείται με την μέθοδο PCA.

#Όμοια με πάνω εξηγούνται τα αποτελέσματα

#Υπολογισμός διαφοράς μεταξύ αναπραγόμενου και κανονικού πίνακα συσχετίσεων

data.fa.mle$residual

#Οριστική απόφαση με ποια μέθοδο θα συνεχίσω----

#Υπολογίζουμε το πλήθος των καταλοίπων που είναι <= από 0.05 στο μοντέλο με μέθοδο εκτίμησης
#των φορτίων την PCA

sum(data.fa.pca$residual<=0.05)

#Υπολογίζουμε το πλήθος των καταλοίπων που είναι <= από 0.05 στο μοντέλο με μέθοδο εκτίμησης
#των φορτίων την MLE

sum(data.fa.mle$residual<=0.05)

#’ρα με την μέθοδο PCA επιτυγχάνουμε να έχουμε περισσότερα κατάλοιπα που είναι μικρά.Για αυτό
#τον λόγο προτιμάμε να συνεχίσουμε την ανάλυση με αυτήν

#Factor Analysis with pca and rotations----

#Για να ξεκαθραίσουμε λίγο οι ερμηνείες θα κάνουμε δυο περιστροφές. Θα κάνουμε την Varimax και
#την Quartimax

#Περιστροφή Varimax----

data.fa.pca.varimax = fa(r = cor(final_dataset[,-(1:4)]), nfactors = 5, rotate = "varimax",fm = "pa")

#Με βάση τα αποτελέσματα βλέπουμε ότι:

#Ο πρώτος παράγοντας συσχετίζεται ισχυρά και θετικά με τις μεταβλητές: median_age,aged_70_older,
#human_development_index. ’ρα, ο πρώτος παράγοντας συσχετίζεται με 3 μεταβλητές.

#Ο δεύτερος παράγοντας συσχετίζεται ισχυρά και θετικά με τις μεταβλητές: total_cases,total_deaths
#’ρα, ο δεύτερος παράγοντας συσχετίζεται με 2 μεταβλητές.

#Ο τρίτος παράγοντας συσχετίζεται με την θετικά με την μεταβλητή diabetes_prevalence.
#’ρα, ο τρίτος παράγοντας συσχετίζεται με 1 μεταβλητή.

#Ο τέταρτος παράγοντας συσχετίζεται ισχυρά και αρνητικά με την μεταβλητή cardiovasc_death_rate
#’ρα ο τέταρτος παράγοντας συσχετίζεται με 1 μεταβλητή.

#Ο πέπμτος παράγοντας συσχετίζεται θετικά με την μεταβλητή stringency_index.
#’ρα ο πέμπτος παράγοντας συσχετίζεται με 1 μεταβλητή.

#Περιστροφή Quartimax----

#Φορτώνουμε την απαραίτητη βιβλιοθήκη

library(GPArotation)

data.fa.pca.quartimax = fa(r = cor(final_dataset[,-(1:4)]), nfactors = 5, rotate = "quartimax",fm = "pa")

#Με βάση τα παραπάνω αποτελέσματα βλέπουμε ότι:

#Ο πρώτος παράγοντας συσχετίζεται ισχυρά και θετικά με τις μεταβλητές: life_expectancy,median_age,
#aged_70_older,human_development_index.
#’ρα ο πρώτος παράγοντας συσχετίζεται με 4 μεταβλητές

#Ο δεύτερος παράγοντας συσχετίζεται ισχυρά και θετικά με τις μεταβλητές: total_cases,total_deaths
#’ρα, ο δεύτερος παράγοντας συσχετίζεται με 2 μεταβλητές.

#Ο τρίτος παράγοντας συσχετίζεται θετικά με την μεταβλητή diabetes_prevalence και αρνητικά με την
#μεταβλητή aged_70_older.
#’ρα ο τρίτος παράγοντας συσχετίζεται με 2 μεταβλητή.

#O τέταρτος παράγοντας συσχετίζεται θετικά με την μεταβλητή cardiovasc_death_rate
#’ρα ο τέταρτος παράγοντας συσχετίζεται με 1 μεταβλητή.

#Ο πέμπτος παράγοντας συσχετίζεται θετικά με την μεταβλητή stringency_index.
#’ρα ο πέμπτος παράγοντας συσχετίζεται με 1 μεταβλητή.

#Περιστροφή Equamax----

data.fa.pca.equamax = fa(r = cor(final_dataset[,-(1:4)]), nfactors = 5, rotate = "equamax",fm = "pa")

#Με βάση τα παραπάνω αποτελέσματα βλέπουμε ότι:

#Ο πρώτος παράγοντας συσχετίζεται ισχυρά και θετικά με τις μεταβλητές: median_age,aged_70_older
#’ρα ο πρώτος παράγοντας συσχετίζεται με 2 μεταβλητές.

#Ο δεύτερος παράγοντας συσχετίζεται ισχυρά και θετικά με τις μεταβλητές: total_cases,total_deaths
#’ρα ο δεύτερος παράγοντας συσχετίζεται με 2 μεταβλητές.

#Ο τρίτος παράγοντας συσχετίζεται θετικά με τις μεταβλητές: diabetes_prevalence,gdp_per_capita,
#human_development_index.
#’ρα ο τρίτος παράγοντας συσχετίζεται με 3 μεταβλητές.

#Ο τέταρτος παράγοντας συσχετίζεται ισχύρα και αρνητικά με την μεταβλητή cardiovasc_death_rate και
#θετικά αλλά ασθενέστερα με την μεταβλητή life_expectancy
#’ρα ο τέταρτος παράγοντας συσχετίζεται με 2 μεταβλητές.

#Ο πέμπτος παράγοντας συσχετίζεται θετικά με τις μεταβλητές stringency_index,new_deaths.
#’ρα ο πέμπτος παράγοντας συσχετίζεται με 2 μεταβλητή.

#Επιλογή περιστροφής----

#Βλέποντας τα αποτελέσματα των περιστροφών επιλέγουμε να συνεχίσουμε την ανάλυση με την
#περιστροφή Quartimax.

#Ερμηνεία παραγόντων----

#Με βάση την περιστροφή Quartimax μια ερμηνεία που μπορεί να αποδοθεί στους παράγοντες είναι:

#Πρώτος παράγοντας:----

# Η ερμηνεία του πρώτου παράγοντα είναι η ποιότητα της ζωής σε μια χώρα (θετικές συσχετίσεις 
#με τις μεταβλητές: life_expectancy,median_age,aged_70_older,human_development_index, υψηλές
#τιμές δηλώνουν καλή ποιότητα ζωής, ενώ χαμηλές δηλώνουν χαμηλή).

#Δεύτερος παράγοντας:----

# Η ερμηνεία του δεύτερου παράγοντα είναι το πόσο κακή είναι η επιδειμιολογική εικόνα μιας χώρας
# (θετικές συσχετίσεις με τις μεταβλητές total_cases, total_deaths, υψηλές τιμές δηλώνουν κακή
# επιδειμιολογική εικόνα, ενώ χαμηλές δηλώνουν καλή επιδειμιολογική εικόνα)

#Τρίτος παράγοντας:----

# Η ερμηνεία του τρίτου παράγοντα είναι η επιρροή του ποσοστού των ατόμων ηλικίας
# 20 - 79 ετών που έπασχαν από διαβήτη το 2017 (θετική συσχέτιση με την μεταβλητή
# diabetes_prevalence, άρα υψηλές τιμές δηλώνουν μεγάλη επιρροή του ποσοστού των
# ατόμων 20 -79 ετών που έπασχαν από διαβήτη το 2017, ενώ χαμηλές δηλώνουν χαμηλή

#Τέταρτος παράγοντας:----

# Η ερμηνεία του τέταρτου παράγοντα είναι η επιρροή του δείκτη θνησιμότητας από
# καρδιαγγειακές ασθένειες το 2017 (θετική συσχέτιση με την μεταβλητή cardiovasc
# _death_rate, άρα υψηλές τιμές δηλώνουν μεγάλη επιρροή του δείκτη θνησιμότητας
# από καρδιαγγειακές ασθένειες το 2017,ενώ χαμηλές τιμές δηλώνουν χαμηλή)

#Πέμπτος παράγοντας:----

# Η ερμηνεία του πέμπτου παράγοντα είναι η σφοδρότητα των κυβερνητικών μέτρων
# με στόχο την καταπολέμηση της πανδημίας Covid19 (θετική συσχέτιση με την
# μεταβλητή stringency_index, άρα υψηλές τιμές δηλώνουν μεγάλη σφοδρότητα
# κυβερνητικών μέτρων, ενώ μικρές τιμές δηλώνουν μικρή σφοδρότητα κυβερνητικών
# μέτρων,δηλαδή πιο χαλαρα)

#Υπολογισμός scores με την μέθοδο Barlett

barlett.scores = factor.scores(x = final_dataset[,-(1:4)], f=data.fa.pca.quartimax,method = "Bartlett")$scores #στο x=Dataframe or matrix of raw data (needed to get factor scores) or matrix with correlations.

#Factor Analysis κατασκευή Heatmaps----

bar.all.scores = cbind(final_dataset[,c(2,3,4)],barlett.scores)

#Για τον πρώτο παράγοντα----

#Για την Ευρώπη----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Europe",])$PA1),
          to = max((bar.all.scores[bar.all.scores$continent == "Europe",])$PA1),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PA1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the first factor")

ggplotly(p)

# Παρατηρώντας το παραπάνω γράφημα και έχοντας κατά νου την ερμηνεία του πρώτου παράγοντα
# μπορούμε να πούμε ότι η Μολδαβία καθόλη την διάρκεια της πανδημίας Covid19 είχε πολύ
# κακή ποιότητα ζωής. Επιπλέον, βλέπουμε ότι οι χώρες: Ουκρανία, Σλοβακία,Ρωσία, Ρουμανία,
# Πολωνία, Κύπρος, Βοσνία Ερζεγοβίνη, Λευκορωσία ήταν σχετικά χαμηλή ποιότητα ζωής.
# Επιπροσθέτως, βλέπουμε ότι η Ιταλία και η Γερμανία είχαν πολύ καλή ποιότητα ζωής. Τέλος,
# βλέπουμε ότι όλες οι υπόλοιπες Ευρωπαϊκές χώρες είχαν σχετικά υψηλή ποιότητα ζωής.

#Για την Ασία----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Asia",])$PA1),
          to = max((bar.all.scores[bar.all.scores$continent == "Asia",])$PA1),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PA1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the first factor")

ggplotly(p)

# Παρατηρώντας το παραπάνω γράφημα βλέπουμε ότι η χώρες: Υεμένη, Τίμορ, Πακιστάν, Νεπάλ,
# Βιρμανία, Λάος, Ιρακ, Ινδία, Καμπότζη, Μπουτάν, Μπανγκλαντέζ, Αφγανιστάν είχαν πολύ
# κακή ποιότητα ζωής την περίοδο της πανδημίας Covid19. Επιπλέον, βλέπουμε ότι οι χώρες:
# Νότια Κορέα, Σινγκαπούρη, Ισραήλ, Γεωργία είχαν σχετικά υψηλή ποιότητα ζωής. Ακόμη,
# βλέπουμε ότι η Ιαπωνία είναι η μοναδική χώρα της Ασίας η οποία είχε πολύ καλή ποιότητα
# ζωής. Τέλος, βλέπουμε ότι όλες οι υπόλοιπες χώρες της Ασίας είχαν σχετικά χαμηλή
# ποιότητα ζωής.

#Για την Νότια Αμερική----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "South America",])$PA1),
          to = max((bar.all.scores[bar.all.scores$continent == "South America",])$PA1),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PA1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the first factor")

ggplotly(p)

# Παρατηρώντας το παραπάνω γράφημα βλέπουμε ότι οι χώρες: Υεμένη, Τίμορ, Πακιστάν,
# Νεπάλ, Βιρμανία, Λάος, Ιρακ, Ινδία, Καμπότζη, Μπουτάν, Μπανγκλαντέζ, Αφγανιστάν
# είχαν πολύ κακή ποιότητα ζωής την περίοδο της πανδημίας Covid19. Επιπλέον,
# βλέπουμε ότι οι χώρες: Νότια Κορέα, Σινγκαπούρη, Ισραήλ, Γεωργία είχαν σχετικά
# υψηλή ποιότητα ζωής. Ακόμη, βλέπουμε ότι η Ιαπωνία είναι η μοναδική χώρα της
# Ασίας η οποία είχε πολύ καλή ποιότητα ζωής. Τέλος, βλέπουμε ότι όλες οι
# υπόλοιπες χώρες της Ασίας είχαν σχετικά χαμηλή ποιότητα ζωής.

#Για την Βόρεια Αμερική----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "North America",])$PA1),
          to = max((bar.all.scores[bar.all.scores$continent == "North America",])$PA1),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PA1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the first factor")

ggplotly(p)

# Παρατηρώντας το παραπάνω γράφημα βλέπουμε ότι ο Καναδάς είχε πολύ καλή ποιότητα
# ζωής κατά την διάρκεια της πανδημίας. Επιπλέον, βλέπουμε ότι οι χώρες: Η.Π.Α,
# Τρινιντάντ και Τομπάγκο, Παναμάς, Κόστα Ρίκα, Μπαρμπάντος, Μπαχάμες είχαν
# σχετικά καλή ποιότητα ζωής. Επιπροσθέτως, βλέπουμε ότι η Αϊτή είχε πολύ κακή
# ποιότητα ζωής. Τέλος, βλέπουμε ότι όλες οι υπόλοιπες χώρες της Βόρειας Αμερικής
# είχαν σχετικά κακή ποιότητα ζωής.

#Για την Ωκεανία----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA1),
          to = max((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA1),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PA1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the first factor")

ggplotly(p)

# Παρατηρώντας το παραπάνω γράφημα βλέπουμε ότι η Νέα Ζηλανδία και η Αυστραλία
# είχαν πολύ καλή ποιότητα ζωής. Από την άλλη μεριά το νησιωτικό κράτος Φίτζι
# είχε πολύ κακή ποιότητα ζωής.

#Για την Αφρική----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Africa",])$PA1),
          to = max((bar.all.scores[bar.all.scores$continent == "Africa",])$PA1),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PA1)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the first factor")

ggplotly(p)

# Παρατηρώντας το παραπάνω γράφημα βλέπουμε ότι οι Σεϋχέλλες και η χώρα Μαυρίκιος
# είχαν πολύ καλή ποιότητα ζωής κατά την διάρκεια της πανδημίας Covid19. Από την
# άλλη μεριά βλέπουμε ότι οι χώρες: Νιγηρία, Μαλί, ΚεντροΑφρικανική Δημοκρατία,
# Μπουρουντί, Μπουρκίνα Φάσο είχαν πολύ κακή ποιότητα ζωής. Επιπλέον, βλέπουμε
# ότι οι χώρες: Τυνησία, Μαρόκο, Λιβύη, Γκαμπόν, Αίγυπτος, Αλγερία είχαν σχετικά
# καλή ποιότητα ζωής. Επιπροσθέτως, βλέπουμε ότι οι χώρες: Κένυα, Γκάνα,
# Σουασιλάνδη, Τζιμπουτί είχαν σχετικά κακή ποιότητα ζωής.

#Για τον δεύτερο παράγοντα----

#Για την Ευρώπη----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Europe",])$PA2),
          to = max((bar.all.scores[bar.all.scores$continent == "Europe",])$PA2),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PA2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the second factor")

ggplotly(p)

# Παρατηρώντας το παραπάνω γράφημα και έχοντας κατά νου την ερμηνεία του δεύτερου
# παράγοντα μπορούμε να πούμε ότι ο ρυθμός επιδείνωσης της επιδειμιολογικής εικόνας
# των χωρών: Ηνωμένο Βασίλειο, Ουκρανία, Ισπανία, Ρωσία, Πολωνία, Ολλανδία, Ιταλία,
# Γερμανία, Γαλλία ήταν αισθητά μεγαλύτερος από των υπολοίπων Ευρωπαϊκών χωρών.
# Επιπλέον, παρατηρούμε ότι οι χώρες: Ηνωμένο Βασίλειο, Ρωσία, Γερμανία, Γαλλία τον
# Ιούνιο του 2022 είχαν πολύ κακή επιδειμιολογική εικόνα. Από την άλλη μεριά στις
# υπόλοιπες χώρες βλέπουμε ότι η επιδειμιολογική εικόνα ήταν πολύ καλύτερη.

#Για την Ασία----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Asia",])$PA2),
          to = max((bar.all.scores[bar.all.scores$continent == "Asia",])$PA2),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PA2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the second factor")

ggplotly(p)

# Παρατηρώντας το παραπάνω γράφημα βλέπουμε ότι ο ρυθμός επιδείνωσης της επιδειμιολογικής
# εικόνας των χωρών: Βιετνάμ, Τουρκία, Νότια Κορεά, Ινδία είναι αισθητά μεγαλύτερος από
# ότι των υπολοίπων χωρών. Επιπλέον, βλέπουμε ότι η επιδειμιολογική εικόνα της Ινδίας από
# τον Απρίλιο του 2021 και μετά ήταν αρκετά κακή. Από την άλλη μεριά βλέπουμε στις
# υπόλοιπες χώρες της Ασίας η επιδειμιολογική εικόνα ήταν πολύ καλύτερη.

#Για την Νότια Αμερική----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "South America",])$PA2),
          to = max((bar.all.scores[bar.all.scores$continent == "South America",])$PA2),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PA2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the second factor")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι ο ρυθμός επιδείνωσης της επιδειμιολογικής
# εικόνας των χωρών: Περού, Κολομβία, Βραζιλία, Αργεντινή είναι αισθητά
# μεγαλύτερος από ότι των υπολοίπων χωρών με την Βραζιλία να έχει τον υψηλότερο.
# Επιπλέον, βλέπουμε ότι από τον Φεβρουάριο του 2022 και μετά η επιδειμιολογική
# εικόνα της Βραζιλίας ήταν πολύ κακή. Από την άλλη μεριά βλέπουμε ότι στις
# υπόλοιπες χώρες της Νότιας Αμερικής η επιδειμιολογική εικόνα ήταν πολύ καλύτερη.


#Για την Βόρεια Αμερική----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "North America",])$PA2),
          to = max((bar.all.scores[bar.all.scores$continent == "North America",])$PA2),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PA2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the second factor")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι ο ρυθμός επιδείνωσης της επιδειμιολογικής
# εικόνας των Η.Π.Α και του Μεξικό είναι αισθητά μεγαλύτερος από ότι των
# υπολοίπων χωρών με τις Η.Π.Α να έχουν τον υψηλότερο. Επιπλέον, βλέπουμε ότι από
# τον Ιανουάριο του 2022 και μετά η επιδειμιολογική εικόνα των Η.Π.Α ήταν πολύ
# κακή. Από την άλλη μεριά βλέπουμε ότι στις υπόλοιπες χώρες της Βόρειας
# Αμερικής η επιδειμιολογική εικόνα ήταν πολύ καλύτερη.

#Για την Ωκεανία----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA2),
          to = max((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA2),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PA2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the second factor")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι ο ρυθμός επιδείνωσης της επιδειμιολογικής
# εικόνας της Αυστραλίας είναι αισθητά μεγαλύτερος από ότι των υπολοίπων χωρών.
# Επιλέον, βλέπουμε ότι από τον Μάϊο του 2022 και μετά η επιδειμιολογική εικόνα
# της Αυστραλίας ήταν πολύ κακή. Από την άλλη πλευρά, βλέπουμε ότι στις
# υπόλοιπες χώρες της Ωκεανίας η επιδειμιολογική εικόνα ήταν πολύ καλύτερη.

#Για την Αφρική----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Africa",])$PA2),
          to = max((bar.all.scores[bar.all.scores$continent == "Africa",])$PA2),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PA2)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the second factor")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι η χώρα Μαυρίκιος είχε την καλύτερη επιδειμιολογική
# εικόνα από όλες τις Αφρικανικές χώρες. Επιπλέον, βλέπουμε ότι ο ρυθμός επιδείνωσης της
# επιδειμιολογικής εικόνας της Νότιας Αφρικής είναι αισθητά μεγαλύτερος από ότι των
# υπολοίπων χωρών και ότι από τον Ιούλιο του 2021 και μετά η επιδειμιολογική εικόνα της
# Νότιας Αφρικής είναι πολύ κακή. Από την άλλη πλευρά, βλέπουμε ότι στις υπόλοιπες χώρες
# της Αφρικής η επιδειμιολογική εικόνα ήταν πολύ καλύτερη.

#Για τον τρίτο παράγοντα----

#Για την Ευρώπη----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Europe",])$PA3),
          to = max((bar.all.scores[bar.all.scores$continent == "Europe",])$PA3),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PA3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the third factor")

ggplotly(p)

# Παρατηρώντας το παραπάνω γράφημα και έχοντας κατά νου την ερμηνεία του τρίτου
# παράγοντα μπορούμε να δούμε ότι η Ιταλία είναι η χώρα στην οποία η επιρροή του
# ποσοστού των ατόμων ηλικίας 20 - 79 ετών που έπασχαν από διαβήτη το 2017 ήταν
# μικρότερη από όλες τις άλλες Ευρωπαϊκές χώρες. Επιπλέον, βλέπουμε στις χώρες:
# Πορτογαλία, Λιθουανία, Λετονία, Ελλάδα, Γερμανία, Γαλλία, Εστονία, Κροατία,
# Βουλγαρία, Αυστρία η επιρροή του ποσοστού των ατόμων ηλικίας 20 - 79 ετών που
# έπασχαν από διαβήτη το 2017 ήταν σχετικά μικρή. Επιπροσθέτως, βλέπουμε ότι η
# επιρροή του ποσοστού των ατόμων ηλικίας 20 - 79 ετών που έπασχαν από διαβήτη
# το 2017 ήταν πολύ υψηλή στις χώρες: Σλοβακία, Νορβηγία, Λουξεμβούργο,
# Ιρλανδία, Ισλανδία, Κύπρος. Τέλος, βλέπουμε ότι η επιρροή του ποσοστού των
# ατόμων ηλικίας 20 - 79 ετών που έπασχαν από διαβήτη το 2017 ήταν μεγάλη στις
# χώρες: Ελβετία, Ρωσία, Ρουμανία, Πολωνία, Ολλανδία, Μολδαβία, Μάλτα, Δανία,
# Τσεχία, Λευκορωσία, Αλβανία.

#Για την Ασία----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Asia",])$PA3),
          to = max((bar.all.scores[bar.all.scores$continent == "Asia",])$PA3),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PA3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the third factor")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι η επιρροή του ποσοστού των ατόμων ηλικίας
# 20 - 79 ετών που έπασχαν από διαβήτη το 2017 ήταν πολύ υψηλή στις χώρες:
# Ηνωμένα Αραβικά Εμιράτα, Κατάρ, Μπαχρειν. Από την άλλη μεριά, βλέπουμε ότι στην
# Ιαπωνία η επιρροή του ποσοστού των ατόμων ηλικίας 20 - 79 ετών που έπασχαν από
# διαβήτη το 2017 ήταν πολύ μικρή. Επιπλέον, βλέπουμε ότι η επιρροή του ποσοστού
# των ατόμων ηλικίας 20 - 79 ετών που έπασχαν από διαβήτη το 2017 ήταν σχετικά
# υψηλή στις χώρες: Ουζμπεκιστάν, Σινγκαπούρη, Σαουδική Αραβία, Όμαν, Μαλεσία,
# Κουβέιτ, Αζερμπαϊτζάν. Τέλος, βλέπουμε ότι σε όλες τις υπόλοιπες Ασιατικές
# χώρες η επιρροή του ποσοστού των ατόμων ηλικίας 20 - 79 ετών που έπασχαν από
# διαβήτη το 2017 ήταν σχετικά μικρή.


#Για την Νότια Αμερική----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "South America",])$PA3),
          to = max((bar.all.scores[bar.all.scores$continent == "South America",])$PA3),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PA3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the third factor")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι η επιρροή του ποσοστού των ατόμων ηλικίας
# 20 - 79 ετών που έπασχαν από διαβήτη το 2017 ήταν πολύ υψηλή στις χώρες
# Σουρινάμ και Γουιάνα. Από την άλλη μεριά, βλέπουμε ότι η επιρροή του ποσοστού
# των ατόμων ηλικίας 20 - 79 ετών που έπασχαν από διαβήτη το 2017 ήταν αρκετά
# χαμηλή στην Ουρουγουάι. Επιπλέον, βλέπουμε ότι στην Βραζιλία υπήρξαν περίοδοι
# όπου η επιρροή του ποσοστού των ατόμων ηλικίας 20 - 79 ετών που έπασχαν από
# διαβήτη το 2017 ήταν: πολύ χαμηλή,πολύ υψηλή,σχετικά χαμηλή, σχετικά υψηλή.
# Τέλος, βλέπουμε ότι σε όλες τις υπόλοιπες χώρες της Νότιας Αφρικής η επιρροή
# του ποσοστού των ατόμων ηλικίας 20- 79 ετών που έπασχαν από διαβήτη το 2017
# ήταν σχετικά υψηλή.

#Για την Βόρεια Αμερική----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "North America",])$PA3),
          to = max((bar.all.scores[bar.all.scores$continent == "North America",])$PA3),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PA3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the third factor")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι στις Η.Π.Α υπήρξαν περιόδοι όπου η επιρροή
# του ποσοστού των ατόμων ηλικίας 20 - 79 ετών που έπασχαν από διαβήτη το 2017
# ήταν: πολύ χαμηλή, σχετικά χαμηλή, σχετικά υψηλή, πολύ υψηλή. Επιπλέον, βλέπουμε
# ότι στις χώρες: Αϊτή, Ελ Σαβαδόρ, Καναδάς, Μπαρμπάντος η επιρροή του ποσοστού
# των ατόμων ηλικίας 20 - 79 ετών που έπασχαν από διαβήτη το 2017 ήταν σχετικά
# χαμηλή. Τέλος, βλέπουμε ότι σε όλες τις υπόλοιπες χώρες της Βόρειας Αμερικής η
# επιρροή του ποσοστού των ατόμων ηλικίας 20 - 79 ετών που έπασχαν από διαβήτη
# το 2017 ήταν σχετικά υψηλή.

#Για την Ωκεανία----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA3),
          to = max((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA3),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PA3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the third factor")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι στην Νέα Ζηλανδία και στην Αυσταλία η
# επιρροή του ποσοστού των ατόμων ηλικίας 20 - 79 ετών που έπασχαν από
# διαβήτη το 2017 ήταν σχετικά χαμηλή. Από την άλλη μεριά, βλέπουμε ότι στο
# νησιωτικό κράτος Φίτζι η επιρροή του ποσοστού των ατόμων ηλικίας 20 - 79
# ετών που έπασχαν από διαβήτη το 2017 ήταν πολύ υψηλή.

#Για την Αφρική----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Africa",])$PA3),
          to = max((bar.all.scores[bar.all.scores$continent == "Africa",])$PA3),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PA3)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the third factor")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι στις χώρες: Νιγηρία, Μοζαμβίκη, Μαλι, Μαλαουί,
# Λιβερία, Γουινέα, Γκάμπια, Αιθιοπία, Μπουρουντί, Μπουρκίνα Φάσο, Μπενίν η επιρροή
# του ποσοστού των ατόμων ηλικίας 20 - 79 ετών που έπασχαν από διαβήτη το 2017 ήταν
# πολύ χαμηλή. Από την άλλη μεριά, βλέπουμε ότι στις χώρες Μαυρίκιος και Αίγυπτος
# η επιρροή του ποσοστού των ατόμων ηλικίας 20 - 79 ετών που έπασχαν από διαβήτη
# το 2017 ήταν πολύ υψηλή. Επιπλέον, βλέπουμε ότι στις χώρες: Τυνησία, Σεϋχέλλες,
# Μαρόκο, Λιβύη, Μποτσουάνα, Αλγερία η επιρροή του ποσοστού των ατόμων ηλικίας 20 -
# 79 ετών που έπασχαν από διαβήτη το 2017 ήταν σχετικά υψηλή. Τέλος, βλέπουμε ότι σε
# όλες τις υπόλοιπες χώρες της Αφρικής η επιρροή του ποσοστού των ατόμων ηλικίας 20
# - 79 ετών που έπασχαν από διαβήτη το 2017 ήταν σχετικά χαμηλή.

#Για τον τέταρτο παράγοντα----

#Για την Ευρώπη----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Europe",])$PA4),
          to = max((bar.all.scores[bar.all.scores$continent == "Europe",])$PA4),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PA4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the fourth factor")

ggplotly(p)

# Παρατηρώντας το παραπάνω γράφημα και έχοντας κατά νου την ερμηνεία του τέταρτου
# παράγοντα μπορούμε να πούμε ότι στις χώρες: Ηνωμένο Βασίλειο, Ελβετία, Σουηδία,
# Ισπανία, Νορβηγία, Ολλανδία, Λουξεμβούργο, Ιρλανδία, Ισλανδία, Γαλλία, Δανία,
# Τσεχία, Βέλγιο η επιρροή του δείκτη θνησιμότητας από καρδιαγγειακές ασθένειες
# το 2017 ήταν πολύ χαμηλή. Από την άλλη μεριά, βλέπουμε ότι στην Ουκρανία η
# επιρροή του δείκτη θνησιμότητας από καρδιαγγειακές ασθένειες το 2017 ήταν πολύ
# υψηλή. Επιπλέον, βλέπουμε ότι στις χώρες: Ρωσία, Ρουμανία, Μολδαβία, Βουλγαρία,
# Λευκορωσία η επιρροή του δείκτη θνησιμότητας από καρδιαγγειακές ασθένειες το
# 2017 ήταν σχετικά υψηλή. Τέλος, βλέπουμε ότι σε όλες τις υπόλοιπες χώρες της
# Ευρώπης η επιρροή του δείκτη θνησιμότητας από καρδιαγγειακές ασθένειες το 2017
# ήταν σχετικά χαμηλή.


#Για την Ασία----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Asia",])$PA4),
          to = max((bar.all.scores[bar.all.scores$continent == "Asia",])$PA4),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PA4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the fourth factor")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι στο Ουζμπεκιστάν η επιρροή του δείκτη θνησιμότητας
# από καρδιαγγειακές ασθένειες το 2017 ήταν πολύ υψηλή. Από την άλλη μεριά, βλέπουμε ότι
# στις χώρες: Σινγκαπούρη, Κατάρ, Ισραήλ, Ιράκ η επιρροή του δείκτη θνησιμότητας από
# καρδιαγγειακές ασθένειες το 2017 ήταν πολύ χαμηλή. Επιπλέον, βλέπουμε ότι στις χώρες:
# Υεμένη, Μονγκολία, Κιργιζία, Καζακστάν, Γεωργία, Αζερμπαιτζάν, Αφγανιστάν η επιρροή του
# δείκτη θνησιμότητας από καρδιαγγειακές ασθένειες το 2017 ήταν σχετικά υψηλή. Τέλος,
# βλέπουμε ότι σε όλες τις υπόλοιπες χώρες της Ασίας η επιρροή του δείκτη θνησιμότητας
# από καρδιαγγειακές ασθένειες το 2017 ήταν σχετικά χαμηλή.

#Για την Νότια Αμερική----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "South America",])$PA4),
          to = max((bar.all.scores[bar.all.scores$continent == "South America",])$PA4),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PA4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the fourth factor")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι στην Γουιάνα η επιρροή του δείκτη θνησιμότητας
# από καρδιαγγειακές ασθένειες το 2017 ήταν πολύ υψηλή. Από την άλλη μεριά, βλέπουμε
# ότι στο Περού η επιρροή του δείκτη θνησιμότητας από καρδιαγγειακές ασθένειες το
# 2017 ήταν πολύ χαμηλή. Επιπλέον, βλέπουμε στην Βραζιλία και στην Σουρινάμ η επιρροή
# του δείκτη θνησιμότητας από καρδιαγγειακές ασθένειες το 2017 ήταν σχετικά χαμηλή.
# Τέλος, βλέπουμε ότι σε όλες τις υπόλοιπες χώρες της Νότιας Αμερικής η επιρροή του
# δείκτη θνησιμότητας από καρδιαγγειακές ασθένειες το 2017 ήταν σχετικά χαμηλή.

#Για την Βόρεια Αμερική----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "North America",])$PA4),
          to = max((bar.all.scores[bar.all.scores$continent == "North America",])$PA4),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PA4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the fourth factor")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι στις χώρες: Παναμάς, Νικαράγουα, Γουατεμάλα
# η επιρροή του δείκτη θνησιμότητας από καρδιαγγειακές ασθένειες το 2017 ήταν πολύ
# χαμηλή. Από την άλλη μεριά, βλέπουμε ότι στην Αϊτή η επιρροή του δείκτη
# θνησιμότητας από καρδιαγγειακές ασθένειες το 2017 ήταν πολύ υψηλή. Επιπλέον,
# βλέπουμε ότι στις χώρες: Τρινιντάντ και Τομπάγκο, Μπαρμπάντος, Μπαχάμες η επιρροή
# του δείκτη θνησιμότητας από καρδιαγγειακές ασθένειες το 2017 ήταν σχετικά υψηλή.
# Τέλος, σε όλες τις υπόλοιπες χώρες της Βόρειας Αμερικής βλέπουμε ότι η επιρροή
# του δείκτη θνησιμότητας από καρδιαγγειακές ασθένειες το 2017 ήταν σχετικά χαμηλή.

#Για την Ωκεανία----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA4),
          to = max((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA4),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PA4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the fourth factor")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι στις χώρες Νέα Ζηλανδία και Αυστραλία η
# επιρροή του δείκτη θνησιμότητας από καρδιαγγειακές ασθένειες το 2017 ήταν
# πολύ χαμηλή. Από την άλλη μεριά βλέπουμε ότι στο νησιωτικό κράτος Φίτζι η
# επιρροή του δείκτη θνησιμότητας από καρδιαγγειακές ασθένειες το 2017 ήταν
# πολύ υψηλή.

#Για την Αφρική----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Africa",])$PA4),
          to = max((bar.all.scores[bar.all.scores$continent == "Africa",])$PA4),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PA4)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the fourth factor")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι στις χώρες: Ουγκάντα, Τανζανία, Νιγηρία, Μαλαουί,
# Κένυα, Αιθιοπία, Πράσινο Ακρωτήρι (Cape Verde) η επιρροή του δείκτη θνησιμότητας από
# καρδιαγγειακές ασθένειες το 2017 ήταν πολύ χαμηλή. Από την άλλη μεριά, βλέπουμε ότι
# στην Αίγυπτο η επιρροή του δείκτη θνησιμότητας από καρδιαγγειακές ασθένειες το 2017
# ήταν πολύ υψηλή. Επιπλέον, βλέπουμε ότι στις χώρες: Σουδάν, Μαρόκο, Μαυρίκιος, Λιβύη,
# Κεντροαφρικανική Δημοκρατία η επιρροή του δείκτη θνησιμότητας από καρδιαγγειακές
# ασθένειες το 2017 ήταν σχετικά υψηλή. Τέλος, βλέπουμε ότι στις υπόλοιπες χώρες της
# Αφρικής η επιρροή του δείκτη θνησιμότητας από καρδιαγγειακές ασθένειες το 2017 ήταν
# σχετικά χαμηλή.

#Για τον πέμπτο παράγοντα----

#Για την Ευρώπη----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Europe",])$PA5),
          to = max((bar.all.scores[bar.all.scores$continent == "Europe",])$PA5),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Europe') %>%
  ggplot(aes(x = date, y = location, fill = PA5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of European countries in the fifth factor")

ggplotly(p)

# Παρατηρώντας το παραπάνω γράφημα και έχοντας κατά νου την ερμηνεία του πέμπτου
# παράγοντα μπορούμε να πούμε ότι η σφοδρότητα των κυβερνητικών μέτρων των
# Ευρωπαϊκών χωρών με στόχο την καταπολέμηση της πανδημίας ήταν σε γενικές
# γραμμές σχετικά χαμηλή με εξαίρεση κάποιες περιόδους στις οποίες ήταν σχετικά
# υψηλή.

#Για την Ασία----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Asia",])$PA5),
          to = max((bar.all.scores[bar.all.scores$continent == "Asia",])$PA5),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = date, y = location, fill = PA5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of Asian countries in the fifth factor")

ggplotly(p)

# Παρατηρώντας το παραπάνω γράφημα μπορούμε να πούμε ότι σε γενικές γραμμές η
# σφοδρότητα των κυβερνητικών μέτρων των Ασιατικών χωρών με στόχο την
# καταπολέμηση της πανδημίας ήταν σχετικά χαμηλή. Επιπλέον, παρατηρούμε ότι
# στην Ινδία υπήρξε μια περίοδο όπου η σφοδρότητα των κυβερνητικών μέτρων με
# στόχο την καταπολέμηση της πανδημίας ήταν πολύ υψηλή.

#Για την Νότια Αμερική----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "South America",])$PA5),
          to = max((bar.all.scores[bar.all.scores$continent == "South America",])$PA5),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'South America') %>%
  ggplot(aes(x = date, y = location, fill = PA5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in South America in the fifth factor")

ggplotly(p)

# Παρατηρώντας το παραπάνω γράφημα βλέπουμε ότι στην Βραζιλία υπήρξαν περιόδους
# όπου η σφοδρότητα των κυβερνητικών μέτρων με στόχο την καταπολέμηση της
# πανδημίας ήταν σχετικά υψηλή και περιόδους όπου η σφοδρότητα των κυβερνητικών
# μέτρων με στόχο την καταπολέμηση της πανδημίας ήταν υψηλή. Κατα τα άλλα
# βλέπουμε ότι η σφοδρότητα των κυβερνητικών μέτρων με στόχο την καταπολέμηση
# της πανδημίας ήταν σχετικά χαμηλή.

#Για την Βόρεια Αμερική----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "North America",])$PA5),
          to = max((bar.all.scores[bar.all.scores$continent == "North America",])$PA5),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'North America') %>%
  ggplot(aes(x = date, y = location, fill = PA5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in North America in the fifth factor")

ggplotly(p)

# Παρατηρώντας το παραπάνω γράφημα βλέπουμε ότι στις Η.Π.Α υπήρξαν περίοδοι όπου η
# σφοδρότητα των κυβερνητικών μέτρων με στόχο την καταπολέμηση της πανδημίας ήταν:
# πολύ υψηλή, σχετικά υψηλή, σχετικά χαμηλή, πολύ χαμηλή. Επιπλέον, βλέπουμε ότι
# στις υπόλοιπες χώρες της Βόρειας Αμερικής η σφοδρότητα των κυβερνητικών μέτρων
# ήταν σχετικά χαμηλή.

#Για την Ωκεανία----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA5),
          to = max((bar.all.scores[bar.all.scores$continent == "Oceania",])$PA5),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Oceania') %>%
  ggplot(aes(x = date, y = location, fill = PA5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of countries in Oceania in the fifth factor")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι στην Αυστραλία η σφοδρότητα των κυβερνητικών
# μέτρων με στόχο την καταπολέμηση της πανδημίας μέχρι και τον Ιανουάριο του 2022
# ήταν σχετικά υψηλή. Από τον Ιανουάριο του 2022 και μετά βλέπουμε ότι υπάρχει
# αισθητή μείωση της σφοδρότητας των κυβερνητικών μέτρων για την καταπολέμηση
# της πανδημίας. Επιπλέον, βλέπουμε ότι στην Νέα Ζηλανδία μέχρι και τον Αύγουστο
# του 2021 η σφοδρότητα των κυβερνητικών μέτρων με στόχο την καταπολέμηση της
# πανδημίας ήταν σχετικά χαμηλή.Την χρονική περίοδο Αύγουστος του 2021 έως και
# Νοέμβριος του 2021 η σφοδρότητα των κυβερνητικών μέτρων με στόχο την καταπολέμηση
# της πανδημίας ήταν σχετικά υψηλή. Από τον Αύγουστο του 2021 και μετά η σφοδρότητα
# των κυβερνητικών μέτρων της Νέας Ζηλανδίας με στόχο την καταπολέμηση της πανδημίας
# εμφάνισε αισθητή μείωση. Τέλος, βλέπουμε ότι στο νησιωτικό κράτος Φίτζι μέχρι και
# τον Ιανουάριο του 2022 η σφοδρότητα των κυβερνητικών μέτρων ήταν σχετικά υψηλή.Από
# τον Ιανουάριο του 2022 και μετά βλέπουμε ότι η σφοδρότητα των κυβερνητικών μέτρων
# του νησιωτικού κράτους Φίτζι ήταν σχετικά χαμηλή.

#Για την Αφρική----

brk = seq(from = min((bar.all.scores[bar.all.scores$continent == "Africa",])$PA5),
          to = max((bar.all.scores[bar.all.scores$continent == "Africa",])$PA5),length.out = 4)

p = bar.all.scores %>%
  filter(continent == 'Africa') %>%
  ggplot(aes(x = date, y = location, fill = PA5)) +
  geom_tile() +
  scale_fill_gradientn(colours = hue_pal()(length(brk)), breaks = brk) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  ggtitle("Scores of African countries in the fifth factor")

ggplotly(p)

# Από το παραπάνω γράφημα βλέπουμε ότι στις χώρες: Τανζανία, Κεντροαφρικανική
# Δημοκρατία, Καμερούν, Μπουρουντί, Μπουρκίνα Φάσο η σφοδρότητα των
# κυβερνητικών μέτρων με στόχο την καταπολέμηση της πανδημίας ήταν πολύ χαμηλή.
# Επιπλέον, βλέπουμε ότι οι χώρες: Ζάμπια, Σουδάν, Μαυρίκιος, Γκάμπια, Γκαμπόν,
# Μποτσουάνα, Μπενίν είχαν περιόδους όπου η σφοδρότητα των κυβερνητικών μέτρων
# με στόχο την καταπολέμηση της πανδημίας ήταν πολύ χαμηλή. Η σφοδρότητα των
# κυβερνητικών μέτρων με στόχο την καταπολέμηση της πανδημίας στις υπόλοιπες
# χώρες της Αφρικής ήταν σε γενικές γραμμές σχετικά χαμηλή.

#Cluster Analysis----

#Cluster Analysis Προεργασία----

#Τυποποιούμε τα δεδομένα μας ώστε να μην επιρεαστούν από μονάδες μέτρησης

data.scaled = scale(final_dataset[,-(1:4)], scale = TRUE)

#Για να κάνω πιο κάτω τα γραφήματα θέλω να είναι data frame για αυτό τον
#λόγο κάνω τα παρακάτω 

data.scaled = as.data.frame(data.scaled)

#Cluster Analysis Non Hierarchical Clustering Finding the optimal number of clusters ----

#Θα βάλουμε το πλήθος των συστάδων ίσο με 145 μιας και μας φαίνεται κάπως "λογικό" κάθε χώρα
#να ανήκει σε μια μόνο συστάδα.

#Cluster Analysis Non Hierarchical Clustering Execution----

no.clusters = 145

data.nonh = kmeans(data.scaled, centers = no.clusters, algorithm = "MacQueen",iter.max = 100)

#Βλέπουμε και την αναλογία μεταξύ της συνολικής μεταβλητότητας ανάμεσα στις ομάδες με την
#συνολικής μεταβλητότητα

(data.nonh$betweenss/data.nonh$totss)*100

#Πολύ καλό!!!!

#Πλήθος παρατηρήσεων σε κάθε ομάδα----

data.nonh$size

#Γενικά βλέπουμε ομάδες οι οποίες δεν έχουν και πάρα πολύ παρόμοια μεγέθη

#Εύρεση ποσοστού ορθής ταξινόμησης----

#Φτιάχνουμε το διάνυσμα που θα περιέχει για κάθε παρατήρηση την σωστή ομάδα
#στην οποία ανήκει

group_1 = rep(0,nrow(final_dataset))

for(i in 1:length(list_of_countries)){
  
  country = list_of_countries[i]
  
  start = 1
  
  while (final_dataset$location[start]!= country) {
    
    start = start + 1
  }
  
  while (final_dataset$location[start] == country & start<=nrow(final_dataset)) {
    
    group_1[start] = i
    
    start = start + 1
    
  }
}

((sum(group_1 == data.nonh$cluster))/(nrow(final_dataset)))*100 #αυτό έχει νόημα όταν το πλήθος των ομάδων είναι 145

#Το ποσοστό αυτό οφείλουμε να πούμε ότι είναι πάρα πολύ μικρό

#Εύρεση κεντρών----
data.nonh$centers

#Γράφημα/γραφήματα με τα σημεία και τα κέντρα

data.scaled$cluster = as.character(data.nonh$cluster) #προσοχή εδώ αλλάζω τον τύπο

X11(width=55, height=35)

ggplot() + geom_point(data.scaled, mapping = aes(x = population, y = life_expectancy, 
                                           colour = cluster)) + 
  geom_point(mapping = aes_string(x = data.nonh$centers[, "population"], 
                                  y = data.nonh$centers[, "life_expectancy"]), 
             color = "red", size = 4) +
  ggtitle("Graph of population and life_expectancy") +
  geom_text(mapping = aes_string(x = data.nonh$centers[, "population"],
                                 y = data.nonh$centers[, "life_expectancy"],
                                 label = 1:no.clusters),color = "black",
            size = 4) +
  theme_light()

#Μην σε αγχώνει το γεγονός ότι βγαίνουν κάποια σημεία στο αρνητικό κομμάτι, έχει γίνει τυποποίηση οπότε είναι κάπως λογικό

#Στο παραπάνω γράφημα σου φαίνονται λίγα τα σημεία γιατί οι τιμές της μεταβλητής
#population τις περισσότερες φορές επαναλαμβάνονται ή είναι πολύ κοντά η μια με
#την άλλη. Για αυτό υπάρχει και το παρακάω γράφημα για να δεις την διαφορά.

#Στο παραπάνω γράφημα βλέπουμε ότι μεγάλο πλήθος ομάδων έχουν τα κέντρα τους κοντά στην ευθεία x = 0.Επιπλέον, βλέπουμε ότι υπάρχουν ομάφες των οποίων τα κέντρα
#εκτείνονται από την x = 1 έως και την x = 5.Τέλος παρατηρούμε ότι η ομάδα 97 απέχει πάρα πολύ από τις υπόλοιπες ομάδες.

#Πάμε να δούμε ποιες χώρες ανήκουν στις ομάδες που περιγράψαμε (για αυτό θα φτιάξουμε το data frame locations.kmeans)

locations.kmeans = cbind.data.frame(final_dataset$location,data.scaled)

#Αλλάζουμε το όνομα της πρώτης στήλης
colnames(locations.kmeans)[1] = "location"

#Οι χώρες που εχουν παρατηρήσεις στην ομάδα 97 είναι:
unique(locations.kmeans[locations.kmeans$cluster == "97",]$location)

sum(locations.kmeans[locations.kmeans$cluster == "97",]$location == "China")

sum(locations.kmeans[locations.kmeans$cluster == "97",]$location == "India")

length(final_dataset[final_dataset$location == "China",1]) #το οποίο είναι ίσο με αυτό βρήκαμε για την Κίνα πιο πάνω

length(final_dataset[final_dataset$location == "India",1]) #άρα δεν μπήκαν όλες οι παρατηρήσεις στην ομάδα 97

#’ρα μπορούμε να πούμε ότι όλες οι παρατηρήσεις που αντιστοιχούν στην Κίνα και 336 που αντιστοιχούν στην Ινδία διαφέρουν ολότελα από τις παρατηρήσεις του υπόλοιπου συνόλου
#δεδομένων.Αξίζει να σημειωθεί ότι το παραπάνω αποτέλεσμα ήταν αναμενόμενο μιας και η Κίνα και η Ινδία έχουν πολύ μεγαλύτερο πληθυσμό από τις υπόλοιπες χώρες
#(απόδειξη αυτού είναι το παρακάτω γράφημα)

# library(ggplot2)
# library(directlabels)
# 
# range = c(as.Date("2019-08-15"),as.Date("2023-06-01"))
# 
# ggplot(data = final_dataset,
#        aes(x = as.Date(date),y = population,group = location, #στο y μπαίνουν τα score στον τέταρτο παράγοντα
#            color = location)) + geom_line(size = 0.5) +
#   labs(y = "Population", x = "Date") +
#   ggtitle("Populations of Asian countries") +
#   scale_colour_discrete(guide = 'none')  +
#   geom_dl(aes(label = location), method = list(dl.combine("first.points", "last.points"))) +
#   scale_x_date(limits = range)

#Όμοια μπορείς να κάνεις τα άλλα γραφήματα (αν θες)

X11(width=55, height=35)

ggplot() + geom_point(data.scaled, mapping = aes(x = new_cases, y = new_deaths, 
                                                 colour = cluster)) + 
  geom_point(mapping = aes_string(x = data.nonh$centers[, "new_cases"], 
                                  y = data.nonh$centers[, "new_deaths"]), 
             color = "red", size = 4) +
  ggtitle("Graph of new cases and new deaths") +
  geom_text(mapping = aes_string(x = data.nonh$centers[, "new_cases"],
                                 y = data.nonh$centers[, "new_deaths"],
                                 label = 1:no.clusters),color = "black",
            size = 4) +
  theme_light()

#Από το παραπάνω γράφημα βλέπουμε ότι τα πιο πολλά κέντρα είναι μαζεμένα στην αρχή των αξόνων.Επιπλέον βλέπουμε τα κέντρα των ομάδων 75,139,127
#τα οποία απέχουν από τα κέντρα των ομάδων που είναι πιο κοντά στην αρχή των αξόνων.Τα κέντρα των ομάδων 38 και 140 διαφέρουν αισθητά από όλα τα
#υπόλοιπα κέντρα.Πάμε να δούμε ποιές παρατηρήσεις ανήκουν εκεί

unique(locations.kmeans[locations.kmeans$cluster == "38",]$location)

#Με βάση το παραπάνω αποτέλεσμα βλέπουμε ότι στην ομάδα 38 ανήκουν παρατηρήσεις από τις χώρες: Βραζιλία,Ινδία,Η.Π.Α.Αυτό σημαίνει ότι υπήρξαν
#μέρες στις προαναφερθείσες χώρες όπου ο αριθμός των ημερήσιων θανάτων ήταν πολύ υψηλός και ο ημερήσιος αριθμός κρούσματων σχετικά χαμηλός
#(αφού ανήκει στην πάνω αριστερά μεριά).Η αλήθεια είναι ότι το παραπάνω αποτέλεσμα ήταν αναμενόμενο μιας και οι παραπάνω χώρες στις αντίστοιχες
#ηπείρους είχαν τα περισσότερα ημερήσια κρούσματα καθόλη την διάρκεια της πανδημίας(απόδειξη αυτού τα παρακάτω γραφήματα)

# library(ggplot2)
# library(directlabels)
# 
# range = c(as.Date("2019-08-15"),as.Date("2023-06-01"))
# 
# ggplot(data = final_dataset[final_dataset$continent == "South America",],
#        aes(x = as.Date(date),y = new_deaths,group = location, #στο y μπαίνουν τα score στον τέταρτο παράγοντα
#            color = location)) + geom_line(size = 0.5) +
#   labs(y = "New_deaths", x = "Date") +
#   ggtitle("New_deaths in South America countries") +
#   scale_colour_discrete(guide = 'none')  +
#   geom_dl(aes(label = location), method = list(dl.combine("first.points", "last.points"))) +
#   scale_x_date(limits = range)
# 
# ggplot(data = final_dataset[final_dataset$continent == "North America",],
#        aes(x = as.Date(date),y = new_deaths,group = location, #στο y μπαίνουν τα score στον τέταρτο παράγοντα
#            color = location)) + geom_line(size = 0.5) +
#   labs(y = "New_deaths", x = "Date") +
#   ggtitle("New_deaths in North America countries") +
#   scale_colour_discrete(guide = 'none')  +
#   geom_dl(aes(label = location), method = list(dl.combine("first.points", "last.points"))) +
#   scale_x_date(limits = range)
# 
# ggplot(data = final_dataset[final_dataset$continent == "Asia",],
#        aes(x = as.Date(date),y = new_deaths,group = location, #στο y μπαίνουν τα score στον τέταρτο παράγοντα
#            color = location)) + geom_line(size = 0.5) +
#   labs(y = "New_deaths", x = "Date") +
#   ggtitle("New_deaths in Asia countries") +
#   scale_colour_discrete(guide = 'none')  +
#   geom_dl(aes(label = location), method = list(dl.combine("first.points", "last.points"))) +
#   scale_x_date(limits = range)

unique(locations.kmeans[locations.kmeans$cluster == "140",]$location)

#Με βάση το παραπάνω αποτέλεσμα βλέπουμε ότι στην ομάδα 140 ανήκουν παρατηρήσεις από τις Η.Π.Α.Αυτό σημαίνει ότι στις Η.Π.Α υπήρξαν μέρες όπου
#ο αριθμός των ημερήσιων κρουσμάτων ήταν πολύ υψηλός και ο αριθμός των ημερήσιων θανάτων σχετικά υψηλός

# library(ggplot2)
# library(directlabels)
# 
# range = c(as.Date("2019-08-15"),as.Date("2023-06-01"))
# 
# ggplot(data = final_dataset,
#        aes(x = as.Date(date),y = new_cases,group = location, #στο y μπαίνουν τα score στον τέταρτο παράγοντα
#            color = location)) + geom_line(size = 0.5) +
#   labs(y = "New_cases", x = "Date") +
#   ggtitle("New cases in all countries") +
#   scale_colour_discrete(guide = 'none')  +
#   geom_dl(aes(label = location), method = list(dl.combine("first.points", "last.points"))) +
#   scale_x_date(limits = range)

X11(width=55, height=35)

ggplot() + geom_point(data.scaled, mapping = aes(x = total_cases, y = total_deaths, 
                                                 colour = cluster)) + 
  geom_point(mapping = aes_string(x = data.nonh$centers[, "total_cases"], 
                                  y = data.nonh$centers[, "total_deaths"]), 
             color = "red", size = 4) +
  ggtitle("Graph of total cases and total deaths") +
  geom_text(mapping = aes_string(x = data.nonh$centers[, "total_cases"],
                                 y = data.nonh$centers[, "total_deaths"],
                                 label = 1:no.clusters),color = "black",
            size = 4) +
  theme_light()

#Από το παραπάνω γράφημα βλέπουμε ότι τα κέντρα των περισσότερων ομάδων είναι μαζεμένα στην αρχή των αξόνων.Από την άλλη μερια
#βλέπουμε ότι τα κέντρα των ομάδων 139,140,90 απέχουν πάρα πολύ από την αρχή των αξόνων και είναι στο επάνω δεξιά μέρος του γραφήματος
#που σημαίνει ότι σε αυτές τις ομάδες υπάρχουν παρατηρήσεις με μεγάλο αριθμό συνολικών κρουσμάτων και μεγάλο αριθμό συνολικών θανάτων.
#Πάμε να δούμε ποιες χώρες ανήκουν σε αυτές τις ομάδες

unique(locations.kmeans[locations.kmeans$cluster == "139",]$location)

#Από το παραπάνω αποτέλεσμα βλέπουμε ότι στις χώρες Βραζιλία,Ινδία,Η.Π.Α υπήρξαν μέρες τις οποίες ο αριθμός των συνολικών κρουσμάτων 
#ήταν αρκετά μεγάλος και ο αριθμός των συνολικών θανάτων επίσης.

unique(locations.kmeans[locations.kmeans$cluster == "140",]$location)

#Από το παραπάνω αποτέλσμα βλέπουμε ότι στις Η.Π.Α υπήρξαν μέρες τις οποίες ο αριθμός των συνολικών κρουσμάτων και ο αριθμός των
#συνολικών θανάτων ήταν πολύ μεγάλος.

unique(locations.kmeans[locations.kmeans$cluster == "90",]$location)

#Από το παραπάνω αποτέλεσμα βλέπουμε ότι στις Η.Π.Α υπήρξαν μέρες τις οποίες ο αριθμός των συνολικών κρουσμάτων και ο αριθμός των
#συνολικών θανάτων ήταν σε ακραίο βαθμό υψηλός.

X11(width=55, height=35)

ggplot() + geom_point(data.scaled, mapping = aes(x = stringency_index, y = new_cases, 
                                                 colour = cluster)) + 
  geom_point(mapping = aes_string(x = data.nonh$centers[, "stringency_index"], 
                                  y = data.nonh$centers[, "new_cases"]), 
             color = "red", size = 4) +
  ggtitle("Graph of stringency index and new cases") +
  geom_text(mapping = aes_string(x = data.nonh$centers[, "stringency_index"],
                                 y = data.nonh$centers[, "new_cases"],
                                 label = 1:no.clusters),color = "black",
            size = 4) +
  theme_light()

#Από το παραπάνω γράφημα βλέπουμε ότι τα κέντρα των περισσότερων ομάδων είναι στο κάτω μέρος του άξονα y.Αισθητή απόκλιση υπάρχει
#μεταξύ του κέντρου της ομάδας 140 από των υπολοίπων. Αυτή την απόκλιση την αναμέναμε μιας και στην ομάδα 140 ανήκουν οι Η.Π.Α
#στις οποίες τα ημερήσια κρούσματα ήταν υψηλά.

unique(locations.kmeans[locations.kmeans$cluster == "95",]$location)

#Από το παραπάνω αποτέλεσμα βλέπουμε ότι στις χώρες Τζαμαικα,Ιορδανία,Νικαραγουά
#υπήρξαν μέρες τις οποίες και η σφοδρότητα των κυβερνητικών μέτρων ήταν πολύ
#χαμηλή και ο αριθμός των ημερήσιων κρουσμάτων ήταν πολύ χαμηλός.

unique(locations.kmeans[locations.kmeans$cluster == "116",]$location)

#Από το παραπάνω αποτέλεσμα βλέπουμε ότι στις χώρες Μπουτάν,Βολιβία,Δομινικανή
#Δημοκρατία,Ελ Σαβαδόρ,Γουατεμάλα,Ονδούρα,Ιράκ,Ιορδανία,Νεπάλ,Παραγουάι,
#Φιλιππίνες,Βενεζουέλα υπήρξαν μέρες τις οποίες και η σφοδρότητα των
#κυβερνητικών μέτρων ήταν πολύ υψηλή και ο αριθμός των ημερήσιων κρουσμάτων
#ήταν αρκετά χαμηλός.

#Warning στην εφαρμογή του αλγορίθμου K-means----

#Ένα από τα μειονεκτήματα αυτού του αλγορίθμου είναι ότι επηρεάζεται από outliers. Ας δούμε
#τα παρακάτω γραφήματα για να πάρουμε μια εικόνα σχετικά με το τι γίνεται με τα outliers.

#Boxplot Location~Total cases

g = ggplot(data = final_dataset,aes(x = final_dataset$location
                                    ,y = final_dataset$total_cases)) +
  geom_boxplot()

g + labs(x = "Country",y = "Total Cases")

#Boxplot Location~New cases
g = ggplot(data = final_dataset,aes(x = final_dataset$location
                                    ,y = final_dataset$new_cases)) +
  geom_boxplot()

g + labs(x = "Country",y = "New Cases")

#Boxplot Location~Total deaths
g = ggplot(data = final_dataset,aes(x = final_dataset$location
                                    ,y = final_dataset$total_deaths)) +
  geom_boxplot()

g + labs(x = "Country",y = "Total Deaths")

#Boxplot Location~New deaths
g = ggplot(data = final_dataset,aes(x = final_dataset$location
                                    ,y = final_dataset$new_deaths)) +
  geom_boxplot()

g + labs(x = "Country",y = "New Deaths")

#Boxplot Location~Reproduction rate
g = ggplot(data = final_dataset,aes(x = final_dataset$location
                                    ,y = final_dataset$reproduction_rate)) +
  geom_boxplot()

g + labs(x = "Country",y = "Reproduction Rate")

#Boxplot Location~Stringency Index
g = ggplot(data = final_dataset,aes(x = final_dataset$location
                                    ,y = final_dataset$stringency_index)) +
  geom_boxplot()

g + labs(x = "Country",y = "Stringency Index")

#Από τα παραπάνω γραφήματα καταλαβαίνουμε ότι τα outliers δεν μπορούν να αγνοηθούν. Για αυτό
#τον λόγο θεωρούμε καταλληλότερο να εφαρμοστεί η μέθοδος k-medians αντί k-means

#Εφαρμογή K-medians----

library(Gmedian)

data.scaled = scale(final_dataset[,-(1:4)], scale = TRUE) #το ξαναφτιάχνω για να είμαι οκ γιατί πριν είχα την στήλη Cluster
data.scaled = as.data.frame(data.scaled)

data.kmedians = kGmedian(X = data.scaled,ncenters=145,iter.max = 100)

#Πάμε να υπολογίσουμε την ποσοστό της ορθής ταξινόμησης----

#Θα χρησιμοποιήσουμε το group_1 διάνυσμα που φτιάξαμε πιο πριν το οποίο περιέχει
#το σε ποια ομάδα ανήκει πραγματικά η κάθε παρατήρηση

((sum(group_1 == data.kmedians$cluster))/(nrow(final_dataset)))*100 #αυτό έχει νόημα όταν το πλήθος των ομάδων είναι 145

data.scaled$cluster = as.character(data.kmedians$cluster)

#Στα παρακάτω συμπεράσματα θα μας είναι χρήσιμο το παρακάτω dataframe
locations_and_data.scaled = cbind(final_dataset$continent,final_dataset$location,data.scaled)
colnames(locations_and_data.scaled)[1] = "Continent"
colnames(locations_and_data.scaled)[2] = "Location"

#Γράφημα new_cases~new_deaths ----

ggplot() + geom_point(data.scaled, mapping = aes(x = new_cases, y = new_deaths, 
                                                 colour = cluster)) + 
  geom_point(mapping = aes_string(x = data.kmedians$centers[, "new_cases"], 
                                  y = data.kmedians$centers[, "new_deaths"]), 
             color = "red", size = 4) +
  geom_text(mapping = aes_string(x = data.kmedians$centers[, "new_cases"],
                                 y = data.kmedians$centers[, "new_deaths"],
                                 label = 1:no.clusters),color = "black",
            size = 4) +
  
  #Βάζουμε τίτλο
  ggtitle("Graph for new cases and new deaths") +
  
  theme_light()

#Γράφημα concave new_cases~new_deaths ----

library(ggforce)
library(concaveman)

ggplot() +
  
  #Βάζουμε τα σημεία με χρώμα. Τα χρώματα είναι όσα και οι συσταδες δηλαδή 145
  geom_point(data.scaled, mapping = aes(x = new_cases, y = new_deaths,colour = cluster)) +
  
  #Βάζουμε τα κέντρα των συστάδων με κόκκινες βούλες
  geom_point(mapping = aes_string(x = data.kmedians$centers[, "new_cases"],
                                  y = data.kmedians$centers[, "new_deaths"]),
             color = "red", size = 4) +
  
  #Βάζουμε στα κέντρα των συστάδων τον αύξοντα αριθμό
  geom_text(mapping = aes_string(x = data.kmedians$centers[, "new_cases"],
                                 y = data.kmedians$centers[, "new_deaths"],
                                 label = 1:no.clusters),color = "black",size = 4) +

  #Βάζουμε τα χωρία των συστάδων
  geom_mark_hull(mapping = aes(x = data.scaled$new_cases, y = data.scaled$new_deaths,
                               colour = data.scaled$cluster),concavity = 2.8) +
  
  #Βάζουμε τίτλο
  ggtitle("Graph for new cases and new deaths") +
  
  #Βάζουμε το χρώμα που θα έχει το background του γραφήματος
  theme_light()

#Συμπεράσματα:----

#Αρχικά παρατηρούμε ότι τα πιο πολλά κέντρα των ομάδων βρίσκονται πολύ κοντά στην αρχή των αξόνων.Μόνο τα κέντρα των ομάδων 27,11,71 ξεχωρίζουν αισθητά από τα υπόλοιπα. Επιπλέον βλέπουμε ότι υπάρχουν επικαλύψεις
#μεταξύ των ομάδων

#Στην ομάδα 11 ανήκουν παρατηρήσεις από την χώρα:

unique(locations_and_data.scaled[locations_and_data.scaled$cluster == "11","Location"])

#Από το παραπάνω αποτέλεσμα καταλαβαίνουμε ότι η ομάδα 11 περιέχει μόνο παρατηρήσεις από τις Η.Π.Α

#Στην ομάδα 71 ανήκουν παρατηρήσεις από την χώρα/τις χώρες:

unique(locations_and_data.scaled[locations_and_data.scaled$cluster == "71","Location"])

#Από το παραπάνω αποτέλεσμα καταλαβαίνουμε ότι η ομάδα 71 έχει παρατηρήσεις από τις Η.Π.Α και την Ινδία.

#Στην ομάδα 27 ανήκουν παρατηρήσεις από την χώρα/τις χώρες:

unique(locations_and_data.scaled[locations_and_data.scaled$cluster == "27","Location"])

#Από το παραπάνω αποτέλεσμα καταλαβαίνουμε ότι η ομάδα 27 έχει παρατηρήσεις από την Αργεντινή,Βραζιλία,Ινδία,Μεξικό,Η.Π.Α

#Συνοψίζοντας τα παραπάνω μπορούμε να πούμε ότι οι χώρες Η.Π.Α,Ινδία,Αργεντινή,Βραζιλία,Μεξικό έχουν παρατητήσεις σχετικά με τον ημερήσιο αριθμό κρουσμάτων και θανάτων
#οι οποίες είναι ολότελα διαφορετικές από τις υπόλοιπες του συνόλου δεδομένων.

#Γράφημα stringency_index~new_cases----

ggplot() + geom_point(data.scaled, mapping = aes(x = stringency_index, y = new_cases, 
                                                 colour = cluster)) + 
  geom_point(mapping = aes_string(x = data.kmedians$centers[, "stringency_index"], 
                                  y = data.kmedians$centers[, "new_cases"]), 
             color = "red", size = 4) +
  geom_text(mapping = aes_string(x = data.kmedians$centers[, "stringency_index"],
                                 y = data.kmedians$centers[, "new_cases"],
                                 label = 1:no.clusters),color = "black",
            size = 4) +
  
  ggtitle("Graph for stringency index and new cases") +
  
  theme_light()

#Γράφημα concave stringency_index~new_cases----

ggplot() +
  
  #Βάζουμε τα σημεία με χρώμα. Τα χρώματα είναι όσα και οι συσταδες δηλαδή 145
  geom_point(data.scaled, mapping = aes(x = stringency_index, y = new_cases,colour = cluster)) +
  
  #Βάζουμε τα κέντρα των συστάδων με κόκκινες βούλες
  geom_point(mapping = aes_string(x = data.kmedians$centers[, "stringency_index"],
                                  y = data.kmedians$centers[, "new_cases"]),
             color = "red", size = 4) +
  
  #Βάζουμε στα κέντρα των συστάδων τον αύξοντα αριθμό
  geom_text(mapping = aes_string(x = data.kmedians$centers[, "stringency_index"],
                                 y = data.kmedians$centers[, "new_cases"],
                                 label = 1:no.clusters),color = "black",size = 4) +
  
  #Βάζουμε τα χωρία των συστάδων
  geom_mark_hull(mapping = aes(x = data.scaled$stringency_index, y = data.scaled$new_cases,
                               colour = data.scaled$cluster),concavity = 2.8) +
  
  #Βάζουμε το χρώμα που θα έχει το background του γραφήματος
  theme_light()

#Συμπεράσματα:----

#Από το παραπάνω γράφημα βλέπουμε ότι τα περισσότερα κέντρα των ομάδων είναι συγκεντρωμένα στην ευθεία y = 0.
#Τα μόνα κέντρα που διαφέρουν αισθητά από αυτά που ανήκουν στην y = 0 είναι των ομάδων 2,71,11.

#Στην ομάδα 2 ανήκουν παρατηρήσεις που αντιστοιχούν στην χώρα/στις χώρες:

unique(locations_and_data.scaled[locations_and_data.scaled$cluster == "2","Location"])

#’ρα στην ομάδα 2 υπάρχουν παρατηρήσεις που αντιστοιχούν στις χώρες: Βραζιλία,Γαλλία,Γερμανία,Ινδία,Ολλανδία,Νότια Κορέα,Ισπανία,Βιετναμ

#Στην ομάδα 71 ανήκουν παρατηρήσεις που αντιστοιχούν στην χώρα/στις χώρες:

unique(locations_and_data.scaled[locations_and_data.scaled$cluster == "71","Location"])

#’ρα στην ομάδα 71 υπάρχουν παρατηρήσεις που αντιστοιχούν στις χώρες Ινδία,Η.Π.Α

#Στην ομάδα 11 ανήκουν παρατηρήσεις που αντιστοιχούν στην χώρα/στις χώρες:

unique(locations_and_data.scaled[locations_and_data.scaled$cluster == "11","Location"])

#’ρα στην ομάδα 11 ανήκουν μόνο παρατηρήσεις που αντιστοιχούν στην χώρα Η.Π.Α.

#Συνοψίζοντας τα παραπάνω μπορούμε να πούμε ότι στις χώρες που υπάρχουν στις προαναφερθείσες 3 ομάδες υπάρχουν παρατητήσεις σχετικά με τον ημερήσιο αριθμό κρουσμάτων και τον δεικτη σφοδρότηταςτων κυβερνητικών μέτρων για
#την καταπολέμηση της πανδημίας οι οποίες είναι ολότελα διαφορετικές από τις υπόλοιπες του συνόλου δεδομένων.

#Γράφημα hospital_beds_per_thousand~new_deaths----

ggplot() + geom_point(data.scaled, mapping = aes(x = hospital_beds_per_thousand, y = new_deaths, 
                                                 colour = cluster)) + 
  geom_point(mapping = aes_string(x = data.kmedians$centers[, "hospital_beds_per_thousand"], 
                                  y = data.kmedians$centers[, "new_deaths"]), 
             color = "red", size = 4) +
  geom_text(mapping = aes_string(x = data.kmedians$centers[, "hospital_beds_per_thousand"],
                                 y = data.kmedians$centers[, "new_deaths"],
                                 label = 1:no.clusters),color = "black",
            size = 4) +
  
  ggtitle("Graph for hospital beds per thousand and new deaths") +
  
  theme_light()

#Γράφημα concave hospital_beds_per_thousand~new_deaths----

ggplot() +
  
  #Βάζουμε τα σημεία με χρώμα. Τα χρώματα είναι όσα και οι συσταδες δηλαδή 145
  geom_point(data.scaled, mapping = aes(x = hospital_beds_per_thousand, y = new_deaths,colour = cluster)) +
  
  #Βάζουμε τα κέντρα των συστάδων με κόκκινες βούλες
  geom_point(mapping = aes_string(x = data.kmedians$centers[, "hospital_beds_per_thousand"],
                                  y = data.kmedians$centers[, "new_deaths"]),
             color = "red", size = 4) +
  
  #Βάζουμε στα κέντρα των συστάδων τον αύξοντα αριθμό
  geom_text(mapping = aes_string(x = data.kmedians$centers[, "hospital_beds_per_thousand"],
                                 y = data.kmedians$centers[, "new_deaths"],
                                 label = 1:no.clusters),color = "black",size = 4) +
  
  #Βάζουμε τα χωρία των συστάδων
  geom_mark_hull(mapping = aes(x = data.scaled$hospital_beds_per_thousand, y = data.scaled$new_deaths,
                               colour = data.scaled$cluster),concavity = 2.8) +
  
  #Βάζουμε το χρώμα που θα έχει το background του γραφήματος
  theme_light()

#Συμπεράσματα----

#Παρατηρώντας το παραπάνω γραφήμα μπορούμε να πούμε ότι τα περισσότερα κέντρα βρίσκοντα στο κάτω αριστερά μέρος του γραφήματος.Επιπλέον βλέπουμε ότι τα κέντρα των ομάδων
#27,71,11 διαφέρουν αισθητά από αυτά που είναι στο κάτω αριστερά μέρος του γραφήματος.Από τα παραπάνω γνωρίζουμε ότι η ομάδα 11 περιέχει παρατηρήσεις μόνο από τις Η.Π.Α,ότι η ομάδα 71 περιέχει παρατηρήσεις από τις Η.Π.Α και την Ινδία,ότι η ομάδα
#27 περιέχει παρατηρήσεις από την Αργεντινή,Βραζιλία,Ινδία,Μεξικό,Η.Π.Α.’ρα μπορούμε να πούμε ότι στις χώρες που ανήκουν στις προαναφερθείσες 3 ομάδες υπάρχουν παρατηρήσεις
#σχετικά με τον αριθμό των ημερήσιων θανάτων και τον αριθμό των νοσοκομειακών κρεβατιών ανά χιλιάδες οι οποίες είναι ολότελα διαφορετικές από τις υπόλοιπες του συνόλου
#δεδομένων.

#Γράφημα reproduction_rate~new_cases----

ggplot() + geom_point(data.scaled, mapping = aes(x = reproduction_rate, y = new_cases, 
                                                 colour = cluster)) + 
  geom_point(mapping = aes_string(x = data.kmedians$centers[, "reproduction_rate"], 
                                  y = data.kmedians$centers[, "new_cases"]), 
             color = "red", size = 4) +
  geom_text(mapping = aes_string(x = data.kmedians$centers[, "reproduction_rate"],
                                 y = data.kmedians$centers[, "new_cases"],
                                 label = 1:no.clusters),color = "black",
            size = 4) +
  
  ggtitle("Graph for reproduction rate and new cases") +
  
  theme_light()


#Γράφημα concave reproduction_rate~new_cases----
ggplot() +
  
  #Βάζουμε τα σημεία με χρώμα. Τα χρώματα είναι όσα και οι συσταδες δηλαδή 145
  geom_point(data.scaled, mapping = aes(x = reproduction_rate, y = new_cases,colour = cluster)) +
  
  #Βάζουμε τα κέντρα των συστάδων με κόκκινες βούλες
  geom_point(mapping = aes_string(x = data.kmedians$centers[, "reproduction_rate"],
                                  y = data.kmedians$centers[, "new_cases"]),
             color = "red", size = 4) +
  
  #Βάζουμε στα κέντρα των συστάδων τον αύξοντα αριθμό
  geom_text(mapping = aes_string(x = data.kmedians$centers[, "reproduction_rate"],
                                 y = data.kmedians$centers[, "new_cases"],
                                 label = 1:no.clusters),color = "black",size = 4) +
  
  #Βάζουμε τα χωρία των συστάδων
  geom_mark_hull(mapping = aes(x = data.scaled$reproduction_rate, y = data.scaled$new_cases,
                               colour = data.scaled$cluster),concavity = 2.8) +
  
  #Βάζουμε το χρώμα που θα έχει το background του γραφήματος
  theme_light()

#Αν θες να αλλάξεις τα όρια ενός άξονα χρησιμοποιείς την εντολή:
#coord_cartesian(xlim = c(a,b))

#Αν θες και τα δυο τότε χρησιμοποιείς την εντολή:
#coord_cartesian(xlim = c(a1,b1)) + coord_cartesian(ylim = c(a2,b2))

#Συμπεράσματα----

#Από το παραπάνω γράφημα βλέπουμε ότι τα περισσότερα κέντρα των ομάδων είναι συγκεντρωμένα στην ευθεία y = 0.Επιπλέον βλέπουμε ότι
#μόνο το κέντρο της ομάδας 11 διαφέρει αισθητά από τα κέντρα των υπόλοιπων ομάδων.Γνωρίζουμε ότι στην ομάδα 11 ανήκουν μόνο
#παρατηρήσεις που αντιστοιχούν στην χώρα Η.Π.Α.Αυτό σημαίνει ότι στις Η.Π.Α υπάρχουν παρατηρήσεις σχετικά τον δείκτη R (για
#περισσότερα δες το αρχείο με τις πληροφορίες για το σύνολο δεδομένων) και τον αριθμό των ημερήσιων κρουσμάτων οι οποίες
#είναι ολότελα διαφορετικές από τις υπόλοιπες του συνόλου δεδομένων.

#Cluster Analysis Hierarchical Clustering ----

#Ερώτηση:
#https://www.analyticsvidhya.com/blog/2016/11/an-introduction-to-clustering-and-different-methods-of-clustering/
#Στον παραπάνω σύνδεσμο λέει το εξής:

#These models are very easy to interpret but lacks scalability for handling big datasets. Examples of these
#models are hierarchical clustering algorithm and its variants.

#Αρχικά θα πρέπει να φτιάξω τον πίνακα αποστάσεων. Αυτό γίνεται με την εντολή
#dist

#ΔΕΝ ΥΠΟΛΟΓΙΖΕΤΑΙ ΜΕ ΤΙΠΟΤΑΑΑΑ!!!! Just hold my beer ;)

library(Rclusterpp)

#Reference:http://cran.nexr.com/web/packages/Rclusterpp/Rclusterpp.pdf

#Οδηγίες εγκατάστασης αυτού του πακέτου:

# 1) Φορτώνεις το πακέτο githubinstall με την εντολή library(githubintall)

# 2) githubinstall("Rclusterpp")

data.scaled = scale(final_dataset[,-(1:4)], scale = TRUE)
data.scaled = as.data.frame(data.scaled)

#Εφαρμογή μεθόδου Single Linkage----

data.clust.single = Rclusterpp.hclust(x = data.scaled,method = "single",distance = "euclidean")

#Το δενδρόγραμμα για την μέθοδο Single Linkage----

#Κάνουμε το παρακάτω γράφημα για να δούμε σε ποιο ύψος θα "κόψουμε"

plot(data.clust.single, labels = FALSE)

#Πιστεύω πως το 3.3 είναι ένα καλό ύψος να κόψουμε

abline(h = 3.3,col = "red")

library(dendextend)

length(unique(cutree(as.dendrogram(data.clust.single),h = 3.3)))

#’ρα με βάση αυτό το ύψος έχουμε 30 ομάδες

library(randomcoloR)# για να μπορώ να έχω τα 30 πιο "διακριτά" χρώματα

# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")

op = par(bg = "#EFEFEF")

A2Rplot(data.clust.single, k = 30, boxes = FALSE, col.up = "gray50", main = "Dendrogram for method Single Linkage",
        show.labels = FALSE,col.down = distinctColorPalette(30))

par(op)

#Θα βρούμε το διάνυσμα με το που ανήκει η κάθε ομάδα

families.single = cutree(data.clust.single, k = 30)[data.clust.single$order]

#Φτιάχνουμε την αναπαράσταση σε μορφή πίνακα με το πόσα στοιχεία έχει η κάθε ομάδα

table(families.single)

#Εφαρμογή μεθόδου Complete Linkage----

data.clust.complete = Rclusterpp.hclust(x = data.scaled,method = "complete",distance = "euclidean")

#Το δενδρόγραμμα για την μέθοδο Complete Linkage----

#Κάνουμε το παρακάτω γράφημα για να δούμε σε ποιο ύψος θα "κόψουμε"

plot(data.clust.complete,labels = FALSE)

#plot(as.dendrogram(data.clust.complete), type = "rectangle", ylab = "Height") δεν τελειώνει ποτέ

#Πιστεύω ότι είναι καλό να το κόψω στο 23

abline(h = 23,col = "red")

length(unique(cutree(as.dendrogram(data.clust.complete),h = 23)))

#’ρα με βάση αυτό έχουμε 9 ομάδες

# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")

op = par(bg = "#EFEFEF")

A2Rplot(data.clust.complete, k = 9, boxes = FALSE, col.up = "gray50", main = "Dendrogram for method Complete Linkage",
        show.labels = FALSE, col.down = distinctColorPalette(9)) #τα 9 πιο ευδιάκριτα χρώματα

par(op)

#Θα βρούμε το διάνυσμα με το που ανήκει η κάθε ομάδα

families.complete = cutree(data.clust.complete, k = 9)[data.clust.complete$order]

#Φτιάχνουμε τον πίνακα με το πόσες παρατηρήσεις έχει η κάθε ομάδα

table(families.complete)

#Εφαρμογή μεθόδου Ward----

data.clust.ward = Rclusterpp.hclust(x=data.scaled,method="ward",distance="euclidean")

#Το δενδρόγραμμα για την μέθοδο Ward----

#Κάνουμε το παρακάτω γράφημα για να δούμε σε ποιο ύψος θα "κόψουμε"

plot(data.clust.ward,labels = FALSE) #τελικά το έβγαλε

#Πιστεύω ότι είναι καλό να το κόψω στο 96000

abline(h = 96000,col = "red")

length(unique(cutree(as.dendrogram(data.clust.ward),h = 96000))) #όταν το τρέξεις αυτός πρέπει να έχεις φορτώσει την βιβλιοθήκη dendextend

#’ρα με βάση αυτό έχουμε 5 ομάδες

# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")

op = par(bg = "#EFEFEF")

A2Rplot(data.clust.ward, k = 5, boxes = FALSE, col.up = "gray50", main = "Dendrogram for method Ward",
        show.labels = FALSE, col.down = distinctColorPalette(5)) #τα 5 πιο ευδιάκριτα χρώματα

par(op)

#Θα βρούμε το διάνυσμα με το που ανήκει η κάθε ομάδα

families.ward = cutree(data.clust.ward, k = 5)[data.clust.ward$order]

#Φτιάχνουμε τον πίνακα με το πόσες παρατηρήσεις έχει η κάθε ομάδα

table(families.ward)

#Εφαρμογή μεθόδου Weighted Average Linkage----

data.clust.weighted.average.linkage = Rclusterpp.hclust(x=data.scaled,method="average",distance="euclidean")

plot(data.clust.weighted.average.linkage,labels = FALSE)

#Εγώ λέω να το κόψω στο 13

abline(h = 13,col = "red")

length(unique(cutree(as.dendrogram(data.clust.weighted.average.linkage),h = 13)))

#’ρα με βάση αυτό το ύψος έχουμε 10 ομάδες

# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")

op = par(bg = "#EFEFEF")

A2Rplot(data.clust.weighted.average.linkage, k = 10, boxes = FALSE, col.up = "gray50", main = "Dendrogram for method Weighted Average Linkage",
        show.labels = FALSE,col.down = distinctColorPalette(10))

par(op)

#Εύρεση παρατηρήσεων που ανήκουν στην κάθε ομάδα που προκύπτει με την μέθοδο Weighted Average Linakge----

#Θα βρούμε το διάνυσμα που μας δίνει την πληροφορία για το που ανήκει η κάθε παρατήρηση

families.weighted.average.linkage = cutree(data.clust.weighted.average.linkage, k = 10)[data.clust.weighted.average.linkage$order]

#Εμφανίζουμε σε μορφή πίνακα το πλήθος παρατηρήσεων που έχει η κάθε ομάδα

table(families.weighted.average.linkage)

#Χρησιμοποιώντας δεδομένα προσομείωσης έχει βρεθεί ότι η μέθοδος του κοντινότερου γείτονα έχει την χειρότερη
#απόδοση,ενώ οι μέθοδοι Ward και Weighted Average Linkage κάνουν την καλύτερη ομαδοποίηση.

#Θα βρω το ποσοστό των παρατηρήσεων που ανήκει σε κάθε χώρα για κάθε ομάδα στην ομαδοποίηση που γίνεται με την μέθοδο Weighted Average Linkage----

final_dataset.star=cbind.data.frame(final_dataset,families.weighted.average.linkage)

#Για την πρώτη ομάδα

first.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 1,"location"])

#Φτιάχνουμε ένα data frame που έχει τις χώρες και το ποσοστό των παρατηρήσεων που έχει κάθε χωρα στην πρώτη ομαδα

locations.and.percentages.group1.ave = data.frame(location = first.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group1.ave)) {
  
  country = locations.and.percentages.group1.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group1.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 1)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

#Για την δεύτερη ομάδα

second.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 2,"location"])

#Φτιάχνουμε ένα data frame που έχει τις χώρες και το ποσοστό των παρατηρήσεων που έχει κάθε χωρα στην δεύτερη ομαδα

locations.and.percentages.group2.ave = data.frame(location = second.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group2.ave)) {
  
  country = locations.and.percentages.group2.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group2.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 2)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

#Για την τρίτη ομάδα

third.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 3,"location"])

#Φτιάχνουμε ένα data frame που έχει τις χώρες και το ποσοστό των παρατηρήσεων που έχει κάθε χωρα στην τρίτη ομαδα

locations.and.percentages.group3.ave = data.frame(location = third.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group3.ave)) {
  
  country = locations.and.percentages.group3.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group3.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 3)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

#Για την τέταρτη ομάδα

forth.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 4,"location"])

#Φτιάχνουμε ένα data frame που έχει τις χώρες και το ποσοστό των παρατηρήσεων που έχει κάθε χωρα στην τέταρτη ομαδα

locations.and.percentages.group4.ave = data.frame(location = forth.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group4.ave)) {
  
  country = locations.and.percentages.group4.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group4.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 4)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

#Για την πέμπτη ομάδα

fifth.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 5,"location"])

#Φτιάχνουμε ένα data frame που έχει τις χώρες και το ποσοστό των παρατηρήσεων που έχει κάθε χωρα στην πέμπτη ομαδα

locations.and.percentages.group5.ave = data.frame(location = fifth.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group5.ave)) {
  
  country = locations.and.percentages.group5.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group5.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 5)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

#Για την έκτη ομάδα

sixth.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 6,"location"])

#Φτιάχνουμε ένα data frame που έχει τις χώρες και το ποσοστό των παρατηρήσεων που έχει κάθε χωρα στην έκτη ομαδα

locations.and.percentages.group6.ave = data.frame(location = sixth.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group6.ave)) {
  
  country = locations.and.percentages.group6.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group6.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 6)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

#Για την έβδομη ομάδα

seventh.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 7,"location"])

#Φτιάχνουμε ένα data frame που έχει τις χώρες και το ποσοστό των παρατηρήσεων που έχει κάθε χωρα στην έβδομη ομαδα

locations.and.percentages.group7.ave = data.frame(location = seventh.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group7.ave)) {
  
  country = locations.and.percentages.group7.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group7.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 7)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

#Για την όγδοη ομάδα

eighth.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 8,"location"])

#Φτιάχνουμε ένα data frame που έχει τις χώρες και το ποσοστό των παρατηρήσεων που έχει κάθε χωρα στην όγδοη ομαδα

locations.and.percentages.group8.ave = data.frame(location = eighth.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group8.ave)) {
  
  country = locations.and.percentages.group8.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group8.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 8)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

#Για την έννατη ομάδα

ninth.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 9,"location"])

#Φτιάχνουμε ένα data frame που έχει τις χώρες και το ποσοστό των παρατηρήσεων που έχει κάθε χωρα στην έννατη ομαδα

locations.and.percentages.group9.ave = data.frame(location = ninth.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group9.ave)) {
  
  country = locations.and.percentages.group9.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group9.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 9)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

#Για την δέκατη ομάδα

tenth.group.of.countries.ave = unique(final_dataset.star[final_dataset.star$families.weighted.average.linkage == 10,"location"])

#Φτιάχνουμε ένα data frame που έχει τις χώρες και το ποσοστό των παρατηρήσεων που έχει κάθε χωρα στην δέκατη ομαδα

locations.and.percentages.group10.ave = data.frame(location = tenth.group.of.countries.ave, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group10.ave)) {
  
  country = locations.and.percentages.group10.ave$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group10.ave$percentages[i] = (sum(data_sub$families.weighted.average.linkage == 10)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

#Βλέπουμε ότι οι παρατηρήσεις όλων των χωρών εκτός από αυτές που αντιστοιχούν στην Ζιμπαμπουε μπήκαν όλες στην πρώτη ομάδα.
#Οι παρατηρήσεις της Ζιμπάμπουε που αντιστοιχούν στο χρονικό διάστημα 28/5/20 με 7/11/20 μπήκαν στην πρώτη ομάδα. Όλες οι
#υπόλοιπες μπήκαν στις υπόλοιπες 9 ομάδες

#Θα βρω το ποσοστό των παρατηρήσεων που ανήκει σε κάθε χώρα για κάθε ομάδα στην ομαδοποίηση που γίνεται με την μέθοδο Ward----

final_dataset.star=cbind.data.frame(final_dataset,families.ward)

#Για την πρώτη ομάδα

first.group.of.countries.ward = unique(final_dataset.star[final_dataset.star$families.ward == 1,"location"])

#Φτιάχνουμε ένα data frame που έχει τις χώρες και το ποσοστό των παρατηρήσεων που έχει κάθε χωρα στην πρώτη ομαδα

locations.and.percentages.group1.ward = data.frame(location = first.group.of.countries.ward, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group1.ward)) {
  
  country = locations.and.percentages.group1.ward$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group1.ward$percentages[i] = (sum(data_sub$families.ward == 1)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

#Για την δεύτερη ομάδα

second.group.of.countries.ward = unique(final_dataset.star[final_dataset.star$families.ward == 2,"location"])

#Φτιάχνουμε ένα data frame που έχει τις χώρες και το ποσοστό των παρατηρήσεων που έχει κάθε χωρα στην δεύτερη ομαδα

locations.and.percentages.group2.ward = data.frame(location = second.group.of.countries.ward, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group2.ward)) {
  
  country = locations.and.percentages.group2.ward$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group2.ward$percentages[i] = (sum(data_sub$families.ward == 2)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

#Για την τρίτη ομάδα

third.group.of.countries.ward = unique(final_dataset.star[final_dataset.star$families.ward == 3,"location"])

#Φτιάχνουμε ένα data frame που έχει τις χώρες και το ποσοστό των παρατηρήσεων που έχει κάθε χωρα στην τρίτη ομαδα

locations.and.percentages.group3.ward = data.frame(location = third.group.of.countries.ward, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group3.ward)) {
  
  country = locations.and.percentages.group3.ward$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group3.ward$percentages[i] = (sum(data_sub$families.ward == 3)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

#Για την τέταρτη ομάδα

forth.group.of.countries.ward = unique(final_dataset.star[final_dataset.star$families.ward == 4,"location"])

#Φτιάχνουμε ένα data frame που έχει τις χώρες και το ποσοστό των παρατηρήσεων που έχει κάθε χωρα στην τέταρτη ομαδα

locations.and.percentages.group4.ward = data.frame(location = forth.group.of.countries.ward, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group4.ward)) {
  
  country = locations.and.percentages.group4.ward$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group4.ward$percentages[i] = (sum(data_sub$families.ward == 4)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

#Για την πέμπτη ομάδα

fifth.group.of.countries.ward = unique(final_dataset.star[final_dataset.star$families.ward == 5,"location"])

#Φτιάχνουμε ένα data frame που έχει τις χώρες και το ποσοστό των παρατηρήσεων που έχει κάθε χωρα στην πέμπτη ομαδα

locations.and.percentages.group5.ward = data.frame(location = fifth.group.of.countries.ward, percentages = NA)

for (i in 1:nrow(locations.and.percentages.group5.ward)) {
  
  country = locations.and.percentages.group5.ward$location[i]
  
  data_sub = final_dataset.star[final_dataset.star$location == country,]
  
  locations.and.percentages.group5.ward$percentages[i] = (sum(data_sub$families.ward == 5)/nrow(data_sub) * 100) #για να βγει σε μορφή ποσοστού
  
}

#Από ότι βλέπουμε στην πρώτη ομάδα έχουν μπει όλες οι παρατηρήσεις των χωρών: Χιλή,Κίνα,Κολομβία,Κόστα Ρίκα,Κροατία,Κύπρος,Τσεχία,Δανία,Τσιμπουτί,Δομινικανή Δημοκρατία,
#Εκουαδορ,Αίγυπτος,Ελ Σαλβαδορ,Εστονία,Σουαζιλάνδη,Αιθιοπία,Φίτζι,Φιλανδία,Γαλλία,Γκάμπον,Γκάμπια,Γεωργία,Γερμανία,Γκάνα,Ελλάδα,Γρενάδα,Γουατεμάλα,Γουινέα,Γουιάνα, Αιτή,
#Ονδούρα,Ουγκαρία,Ισλανδία,Ινδία,Ινδονησία,Ιράν,Ιρακ,Ιρλανδία,Ισραήλ,Ιταλία,Τζαμαικα,Ιαπωνία,Ιορδανία,Καζακσταν,Κένυα,Κυβέτ,Κιργιζία,Λάος,Λετονία,Λίβανο,Λιβερία,Λιβύη,
#Λιθουανία,Λουξεμβούργο,Μαγαασκάρη,Μαλαουί,Μαλεσία,Μαλί,Μάλτα,Μαυρίκιος,Μεξικό,Μολδαβία,Μονγκολία,Μαρόκο,Μοζαμβίκε,Βιρμανία,Νεπάλ.

#Από ότι βλέπουμε στην δεύτερη ομάδα έχουν μπει όλες οι παρατηρήσεις των χωρών: Αφγανιστάν,Αλβανία,Αλγερία,Αργεντινή,Αυστραλία,Αυστρία,Αζερμπαιτζάν,Μπαχάμες,Μπαχρέιν,
#Μπανγκλαντέζ,Μπαρμπάντος,Λευκορωσία,Βέλγιο,Μπελίζ,Μπενίν,Μπουτάν,Μπολιβία,Βοζνία Ερζεγοβίνη,Μποτσουάνα,Βραζιλία,Μπρουνέι,Βουλγαρία,
#Μπουρκίνα Φάσο,Μπουρούντι,Καμπότζη,Καμερούν,Καναδάς,Πράσινο Ακρωτήρι.

#Από ότι βλέπουμε στην τρίτη ομάδα έχουν μπει όλες οι παρατηρήσεις των χωρών: Νορβηγία,Ομάν,Πακιστάν,Παναμάς,Παραγουάι,Περού,Φιλλιπίνες,Πολωνία,Πορτογαλία,Κατάρ,Ρουμανία,
#Ρωσία,Σαουδική Αραβία,Σευχέλλες,Σινγκαπούρη,Σλοβακία,Σλοβενία,Νότια Αμερική,Νότια Κορέα,Ισπανία,Σιρι Λάνκα,Σουδάν,Σουριναμ,Σουηδία,Ελβετία,Τατζικιστάν,Τανζανία,Ταιλάνδη,
#Τίμορ,Τόνγκο,Τρινινταντ και Τομπαγκο,Τυνισία,Τουρκία,Ουγκάντα,Ουκρανία,Ηνωμένα Αραβικά Εμιράτα,Ηνωμένο Βασίλειο,Η.Π.Α,Ουρουγουάι,
#Ουζμπεκιστάν,Βενεζουέλα,Βιετνάμ,Υεμένη,Ζαμπία,Ζιμπάμπουε.

#Από ότι βλέπουμε στην τέταρτη ομάδα έχουν μπει όλες οι παρατηρήσεις της Νέας Ζηλανδίας.

#Βλέπουμε ότι ένα πολύ μεγάλο μέρος των παρατηρήσεων της ΚεντροΑφρικανικής Δημοκρατίας (81% περίπου) έχει μπει στην πρώτη ομάδα.Οι υπόλοιπες παρατηρήσεις έχουν
#μπει στην δεύτερη ομάδα.Επιπλέον ένα πολύ μικρό μέρος των παρατηρήσεων (περίπου 2%) της Ολανδίας έχει μπει στην πρώτη ομάδα και οι υπόλοιπες μπήκαν στην τέταρτη.
#Ένα πολύ μεγάλο μέρος των παρατηρήσεων της Νιγηρίας (περίπου 97%) μπήκε στην τρίτη ομάδα και οι υπόλοιπες μπήκαν στην πέμπτη ομάδα.Τέλος,βλέπουμε ότι
#ένα πολύ μικρό μέρος των παρατηρήσεων (περίπου 1%) της χώρας Νικαραγουά μπήκε στην τέταρτη ομάδα και οι υπόλοιπες μπήκαν στην πέμπτη ομάδα.

#Προετοιμασία για Discriminant Analysis----

#Θα βάλω σε κάθε χώρα από έναν αριθμό.Δηλαδή θα εφαρμόσω Discriminant Analysis με 145 ομάδες

final_dataset.star = final_dataset

final_dataset.star$group_star = NA

for(i in 1:length(list_of_countries)){
  
  country = list_of_countries[i]

  start = 1
  
  while (final_dataset.star$location[start]!= country) {
    
    start = start + 1
  }
  
  while (final_dataset.star$location[start] == country & start<=nrow(final_dataset.star)) {
    #την δεύτερη συνθήκη την έβαλα γιατί όταν φτάνει στο τέλος και πάει να ελένξει
    #ελέγχει για μια τιμή που δεν ανήκει στο dataset αφου τότε start = nrow(final_dataset.star)
    final_dataset.star$group_star[start] = i
    
    start = start + 1
    
  }
}

#Πολύ πρόχειρος έλεγχος για την κανονικότητα----

library("dgof")

lst.ks = lapply(1:ncol(final_dataset.star[,-c(1:4,21)]), function(i)
  ks.test(final_dataset.star[,-(1:4)][, i], "pnorm"))

#Βλέπουμε ότι καμμιά μεταβλητή δεν ακολουθεί Κανονινκή Κατανομή. Οπότε είναι αδύνατο οι πολυδιάστατες παρατηρήσεις να προέρχονται
#από πολυδιάστατη Κανονική Κατανομή.’ρα δεν μπορούμε να προχωρήσουμε σε LDA ή QDA.

library(DFA.CANCOR)

HOMOGENEITY(data = final_dataset.star, groups = "group_star",variables = colnames(final_dataset.star[,5:20]))

#Το p-value είναι μηδεν άρα δεν έχουμε ομοσκεδαστικότητα των πινάκων διακυμάνσεων-συνδιακυμάνσεων

#Δημιουργία λίστας με τα διανύσματα μέσων----

list_of_means = list()

for (i in 1:length(list_of_countries)) {
  
  data_sub = final_dataset[final_dataset$location == list_of_countries[i],]
  
  list_of_means[[i]] = colMeans(data_sub[,5:20])
  
}

#Δημιουργία λίστας με τους πίνακες διασπορών - συνδιασπορών----

list_of_cov_matrices = list()

for (i in 1:length(list_of_countries)) {
  
  data_sub = final_dataset[final_dataset$location == list_of_countries[i],]
  
  list_of_cov_matrices[[i]] = cov(data_sub[,5:20])
  
}

#Φτιάχνουμε ένα διάνυμσα αστο dataset που θα περιέχει το που κατατάσσει η μέθοδος την κάθε παρατήρηση

#Παρουσιάζουμε μερικούς πίνακες διασπορών-συνδιαπορών οι οποίοι δεν αντιστρέφονται----

solve(matrix(unlist(list_of_cov_matrices[1]), ncol = 16, byrow = TRUE))

solve(matrix(unlist(list_of_cov_matrices[2]), ncol = 16, byrow = TRUE))

solve(matrix(unlist(list_of_cov_matrices[3]), ncol = 16, byrow = TRUE))

#Πάμε να δούμε που υπάρχει πολυσυγγραμικότητα----

model_all = lm(group_star ~ ., data = final_dataset.star[,5:21])

library(mctest)

omcdiag(x = final_dataset.star[,5:20],y = final_dataset.star$group_star,mod = model_all)

#Η διακρίνουσα του πίνακα X'X είναι πολύ κοντά στο μηδέν.
#Επιπλέον, βλέπουμε ότι η τιμή του Χ^2 είναι πάρα πολύ μεγάλη το οποίο υποδειλώνει ότι
#υπάρχει πρόβλημα πολυσυγραμικότητας

imcdiag(x = final_dataset.star[,5:20],y = final_dataset.star$group_star,mod = model_all)

k = ncol(final_dataset.star[,5:20])

qf(1-0.05,k-1,n - k)

#Επιλέγουμε ποιες μεταβλητές θα βγάλουμε λόγω πολυσυγραμμικότητας----

#Με βάση τον παραπάνω πίνακα και το 5% ποσοστιαίο σημείο της αντίστοιχης κατανομής F
#βλέπουμε ότι οι μεταβλητές: life_expectancy,total_cases,total_deaths,,median_age,
#aged_70_older,gdp_per_capita,human_development_index,hospital_beds_per_thousand

#Αργότερα είδαμε ότι πρέπει να βγάλουμε και άλλες

var_to_remove = c("life_expectancy","total_cases","total_deaths","median_age",
                  "aged_70_older","gdp_per_capita","human_development_index",
                  "hospital_beds_per_thousand","population","population_density",
                  "diabetes_prevalence","cardiovasc_death_rate")

final_dataset.star = final_dataset.star[,!names(final_dataset.star) %in% var_to_remove]

#Υπολογίζουμε ξανά τους πίνακες διακύμανσης- συνδιακύμανσης----

list_of_cov_matrices = list()

for (i in 1:length(list_of_countries)) {
  
  data_sub = final_dataset.star[final_dataset.star$location == list_of_countries[i],]
  
  list_of_cov_matrices[[i]] = cov(data_sub[,5:8])
  
}

#Ελέγχουμε ότι δεν υπάρχει κάποιος που δεν αντιστρέφεται----

for (i in 1:length(list_of_cov_matrices)) {
  
  solve(matrix(unlist(list_of_cov_matrices[i]), ncol = 4, byrow = TRUE))
  
}

#Τώρα είμαστε εντάξει

#Υπολογίζουμε και τις μέσες τιμές----

list_of_means = list()

for (i in 1:length(list_of_countries)) {
  
  data_sub = final_dataset.star[final_dataset.star$location == list_of_countries[i],]
  
  list_of_means[[i]] = colMeans(data_sub[,5:8])
  
}

#Εφαρμόζουμε με τον χέρι την Διακρίνουσα Ανάλυση με βάση τον κανόνα
#της ελάχιστης απόστασης

final_dataset.star$discr_group = NA

for (i in 1:nrow(final_dataset.star)) {# το i εκφράζει τον αύξοντα αριθμό της παρατήρησης
  
  #Ορίζουμε το w ως την i-οστή παρατήρηση
  
  w = as.numeric(final_dataset.star[i,5:8])
  
  #Έστω ότι η μικρότερη απόσταση επιτυγχάνεται στον πρώτο πληθυσμό (δεν έχουμε βάλλει την ρίζε γιατί
  #η απόσταση είναι πάντα θετική οπότε δεν έχουμε θέμα)
  
  g = 1 #Το g εκφράζει το που βρέθηκε η μικρότερη απόσταση
  
  min1 = t((w - unlist(list_of_means[1])))%*%solve(matrix(unlist(list_of_cov_matrices[1]), ncol = 4, byrow = TRUE))%*%(w - unlist(list_of_means[1]))
  
  #Πάμε να δούμε αν υπάρχει απόσταση που είναι πιο μικρή σε άλλη ομάδα
  
  for (j in 2:length(list_of_countries)) {#το j εκφράζει την ομάδα
    
    if (t((w - unlist(list_of_means[j])))%*%solve(matrix(unlist(list_of_cov_matrices[j]), ncol = 4, byrow = TRUE))%*%(w - unlist(list_of_means[j]))< min1){
      
      g = j #άλλαξε την ομάδα
      
      min1 = t((w - unlist(list_of_means[j])))%*%solve(matrix(unlist(list_of_cov_matrices[j]), ncol = 4, byrow = TRUE))%*%(w - unlist(list_of_means[j]))
      
      #αυτό το κάναμε με σκοπό στην επόμενη επανάληψη να το συγρκίνουμε με την πιο μικρή απόσταση που έχουμε βρει εκείνη την στιγμή
    }
    
  }
  
  final_dataset.star$discr_group[i] = g
  
}

#Υπολογίζουμε την ακρίβεια του μοντέλου----

((sum(final_dataset.star$group_star == final_dataset.star$discr_group))/(nrow(final_dataset.star)))*100

#Εμφανίζουμε το πόσες παρατηρήσεις μπήκαν σε κάθε πληθυσμό με βάση την Διακρίνουσα Ανάλυση----

table(final_dataset.star$discr_group)

#Υπολογίζουμε πόσες ομάδες έχουν πλήθος παρατηρήσεων μεγαλύτερο από 10 * p----

#όπου p το πλήθος των μεταβλητών που χρησιμοποιήσαμε στην Διακρίνουσα Ανάλυση

h = 0

for (i in 1:length(list_of_countries)) {
  
  data_sub = final_dataset.star[final_dataset.star$location == list_of_countries[i],]
  
  if (nrow(data_sub) > 10 * 4){
    
    h = h + 1
  }

}

#Βρίσκουμε τις αποστάσεις όλων των μέσων των ομάδων----

#Πρέπει να βρούμε όλες τις αποστάσεις μεταξύ των 145 μέσων για να δούμε
#αν πέφτουμε στην περίπτωση που έχει υπογραμμιστεί στο αρχείο

df.means = as.data.frame(do.call(rbind, list_of_means))

dist.mat = dist(as.matrix(df.means),diag = TRUE)

#Κάνουμε ένα ιστόγραμμα των αποστάσεων των μέσω των ομάδων----

hist(dist.mat)

#Αναπαράσταση των αποστάσεων των μέσων( πιο delicaτος τρόπος)----

dist_mi = 1/dist.mat # one over, as qgraph takes similarity matrices as input
library(qgraph)
jpeg('example_forcedraw.jpg', width=1000, height=1000, unit='px') #το αποθηκεύει στο documents
qgraph(dist_mi, layout='spring', vsize=3)
dev.off()

#Όπου υπάρχουν παχιές γραμμές σημαίνει ότι οι αποστάσεις είναι πάρα πολύ μικρές. ’ρα βλέπουμε
#ότι υπάρχουν πολλά ζεύγη ομάδων που έχουν πολύ παρόμοιους μέσους

#Με βάση αυτό που έχουμε υπογραμμίσει στο αρχείο καταλαβαίνουμε ότι ο λόγος που η Διακρίνουσα
#Ανάλυση δίνει πολύ κακά αποτελέσματα είναι ότι υπάρχουν πολλές ομάδες που έχουν πολύ κοντινούς
#μέσους.






