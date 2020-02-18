/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\ahsb1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = ahsb1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=46;
INPUT
   CENTER $ 1-2             ID $ 3-9                 VISIT  10-11          
   AHSB1  12                AHSB3  13                AHSB4  14             
   AHSB5  15                AHSB6  16                AHSB7  17             
   AHSB8  18                AHSB9  19                AHSB10  20            
   AHSB11  21               AHSB12  22               AHSB12A  23           
   AHSB12B  24              AHSB12C  25              AHSB12D  26           
   AHSB12E  27              AHSB12F  28              AHSB12G  29           
   AHSB12H  30              AHSB12I  31              AHSB12J  32           
   AHSB12K  33              AHSB12L  34              AHSB12M  35           
   AHSB12N  36              AHSB12O  37              AHSB12P  38           
   AHSB2A  39               AHSB2B  40               AHSB2C  41            
   AHSB2D  42               AHSB2E  43               AHSB5A  44            
   AHSB6A  45               AHSB7A  46             ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   VISIT = "VISIT NUMBER"
   AHSB1 = "How much has health worried you"
   AHSB3 = "How would you say your health is"
   AHSB4 = "Past yr-wk/>wk stop work etc-illness"
   AHSB5 = "Problem with anxiety"
   AHSB6 = "Problem with depression"
   AHSB7 = "Problem with fears/phobia"
   AHSB8 = "Last 12 mths/take pres. meds/ADD"
   AHSB9 = "Had STD other than HIV/AIDS"
   AHSB10 = "Had hepatitis"
   AHSB11 = "Do you have HIV/AIDS"
   AHSB12 = "Past yr:long-term chronic health prblm"
   AHSB12A = "Health prblm-allergies"
   AHSB12B = "Health prblm-asthma"
   AHSB12C = "Health prblm-skin conditions"
   AHSB12D = "Health prblm-speech problems"
   AHSB12E = "Health prblm-headaches/migraines"
   AHSB12F = "Health prblm-cerebral palsy"
   AHSB12G = "Health prblm-diabetes"
   AHSB12H = "Health prblm-sickcle cell"
   AHSB12I = "Health prblm-epilepsy"
   AHSB12J = "Health prblm-hearing prblm"
   AHSB12K = "Health prblm-mononucleosis"
   AHSB12L = "Health prblm-chronic fatigue"
   AHSB12M = "Health prblm-aches/pain in back"
   AHSB12N = "Health prblm-high blood pressure"
   AHSB12O = "Health prblm-heart problems"
   AHSB12P = "Health prblm-liver problems"
   AHSB2A = "Condition interferes/strenuous actvities"
   AHSB2B = "Condition interferes/going to schl"
   AHSB2C = "Condition interferes/household chores"
   AHSB2D = "Condition interferes/personal care"
   AHSB2E = "Condition interferes/making friends"
   AHSB5A = "Last 12 mths/take pres. meds/anxiety"
   AHSB6A = "Last 12 mths/take  pres.meds/depression"
   AHSB7A = "Last 12 mths/taken pres. meds/phobia" ;

LENGTH
   VISIT 3                  AHSB1 3                  AHSB3 3               
   AHSB4 3                  AHSB5 3                  AHSB6 3               
   AHSB7 3                  AHSB8 3                  AHSB9 3               
   AHSB10 3                 AHSB11 3                 AHSB12 3              
   AHSB12A 3                AHSB12B 3                AHSB12C 3             
   AHSB12D 3                AHSB12E 3                AHSB12F 3             
   AHSB12G 3                AHSB12H 3                AHSB12I 3             
   AHSB12J 3                AHSB12K 3                AHSB12L 3             
   AHSB12M 3                AHSB12N 3                AHSB12O 3             
   AHSB12P 3                AHSB2A 3                 AHSB2B 3              
   AHSB2C 3                 AHSB2D 3                 AHSB2E 3              
   AHSB5A 3                 AHSB6A 3                 AHSB7A 3 ;

        

RUN ;
