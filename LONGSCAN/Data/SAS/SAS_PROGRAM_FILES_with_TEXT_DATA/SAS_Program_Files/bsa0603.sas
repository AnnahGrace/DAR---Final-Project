/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\bsa0603.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = bsa0603 ;

DATA library.&dataset ;
INFILE datafile LRECL=77;
INPUT
   id $ 1-7                 visit  8                 center $ 9-10         
   bsa1  11                 bsa2  12                 bsa3  13              
   bsa4  14                 bsa5  15                 bsa6  16              
   bsa7  17                 bsa8  18                 bsa9  19              
   bsa10  20                bsa11  21                bsa12  22             
   bsa13  23                bsa14  24                bsa15  25             
   bsa16  26                bsa17  27                bsa18  28             
   bsa19  29                bsa20  30                bsa21  31             
   bsa22  32                bsa23  33                bsa24  34             
   bsa25  35                bsa26  36                bsa27  37             
   bsa28  38                bsa29  39                bsa30  40             
   bsa31  41                bsa32  42                bsa33  43             
   bsa34  44                bsa35  45                bsa36  46             
   bsa37  47                bsa38  48                bsa39  49             
   bsa40  50                bsa41  51                bsa42  52             
   bsa43  53                bsa44  54                bsa45  55             
   bsa46  56                bsa47  57                bsa48  58             
   bsa49  59                bsa50  60                bsa51  61             
   bsa52  62                bsa53  63                bsa54 $ 64-66         
   bsa55 DATE11.          ;                       

LABEL
   id = "longscan subject id"
   visit = "visit number"
   center = "field center"
   bsa1 = "nervousness or shaking inside"
   bsa2 = "faintness or dizziness"
   bsa3 = "feels someone else controls thoughts"
   bsa4 = "blames others for most troubles"
   bsa5 = "trouble remembering things"
   bsa6 = "feeling easily annoyed or irritated"
   bsa7 = "pains in the heart or chest"
   bsa8 = "feeling afraid in open spaces"
   bsa9 = "thoughts of ending life"
   bsa10 = "feels most people cannot be trusted"
   bsa11 = "poor appetite"
   bsa12 = "suddenly scared for no reason"
   bsa13 = "uncontrollable temper outbursts"
   bsa14 = "feels lonely even when with people"
   bsa15 = "feels blocked in getting things done"
   bsa16 = "feeling lonely"
   bsa17 = "feeling blue"
   bsa18 = "feeling no interest in things"
   bsa19 = "feeling fearful"
   bsa20 = "feelings being hurt easily"
   bsa21 = "feels people don^t like you/unfriendly"
   bsa22 = "feeling inferior to others"
   bsa23 = "nausea or upset stomach"
   bsa24 = "feel others watch/talk about you"
   bsa25 = "trouble falling asleep"
   bsa26 = "checks/double checks own actions"
   bsa27 = "difficulty making decisions"
   bsa28 = "afraid of buses/trains/subways"
   bsa29 = "trouble getting your breath"
   bsa30 = "hot or cold spells"
   bsa31 = "things/places/activities frighten: avoid"
   bsa32 = "mind going blank"
   bsa33 = "numbness or tingling"
   bsa34 = "should be punished for sins"
   bsa35 = "feeling hopeless about the future"
   bsa36 = "trouble concentrating"
   bsa37 = "feel weak in parts of your body"
   bsa38 = "feeling tense or keyed up"
   bsa39 = "thoughts of death or dying"
   bsa40 = "urge to beat/injure/harm someone"
   bsa41 = "urge to break or smash things"
   bsa42 = "feeling self conscious with others"
   bsa43 = "feeling uneasy in crowds"
   bsa44 = "never feels close to another person"
   bsa45 = "spells of teror or panic"
   bsa46 = "gets into frequent arguments"
   bsa47 = "feel nervous when you are left alone"
   bsa48 = "others don^t give you credit"
   bsa49 = "feels so restless can^t sit still"
   bsa50 = "feelings of worthlessness"
   bsa51 = "feels people take advantage if allowed"
   bsa52 = "feelings of guilt"
   bsa53 = "thinks something is wrong with mind"
   bsa54 = "interviewer initials"
   bsa55 = "date of interview" ;

FORMAT
   bsa55 DATE9. ;
      

LENGTH
   visit 3                  bsa1 3                   bsa2 3                
   bsa3 3                   bsa4 3                   bsa5 3                
   bsa6 3                   bsa7 3                   bsa8 3                
   bsa9 3                   bsa10 3                  bsa11 3               
   bsa12 3                  bsa13 3                  bsa14 3               
   bsa15 3                  bsa16 3                  bsa17 3               
   bsa18 3                  bsa19 3                  bsa20 3               
   bsa21 3                  bsa22 3                  bsa23 3               
   bsa24 3                  bsa25 3                  bsa26 3               
   bsa27 3                  bsa28 3                  bsa29 3               
   bsa30 3                  bsa31 3                  bsa32 3               
   bsa33 3                  bsa34 3                  bsa35 3               
   bsa36 3                  bsa37 3                  bsa38 3               
   bsa39 3                  bsa40 3                  bsa41 3               
   bsa42 3                  bsa43 3                  bsa44 3               
   bsa45 3                  bsa46 3                  bsa47 3               
   bsa48 3                  bsa49 3                  bsa50 3               
   bsa51 3                  bsa52 3                  bsa53 3 ;

         

RUN ;
