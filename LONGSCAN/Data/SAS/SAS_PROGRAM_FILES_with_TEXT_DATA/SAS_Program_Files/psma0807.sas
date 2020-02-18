/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\psma0807.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = psma0807 ;

DATA library.&dataset ;
INFILE datafile LRECL=117;
INPUT
   ID $ 1-7                 PSMAZ1 $ 8               PSMAZ2 $ 9            
   CENTER $ 10-11           VISIT  12-13             PSMA1  14             
   PSMA1A  15               PSMA1B  16               PSMA1C  17            
   PSMA2  18                PSMA2A  19               PSMA2B  20            
   PSMA2C  21               PSMA3  22                PSMA3A  23            
   PSMA3B  24               PSMA3C  25               PSMA4  26             
   PSMA4A  27               PSMA4B  28               PSMA4C  29            
   PSMA5  30                PSMA5A  31               PSMA5B  32            
   PSMA5C  33               PSMA6  34                PSMA6A  35            
   PSMA6B  36               PSMA6C  37               PSMA7  38             
   PSMA7A  39               PSMA7B  40               PSMA7C  41            
   PSMA8  42                PSMA8A  43               PSMA8B  44            
   PSMA8C  45               PSMA9  46                PSMA9A  47            
   PSMA9B  48               PSMA9C  49               PSMA10  50            
   PSMA10A  51              PSMA10B  52              PSMA10C  53           
   PSMA11  54               PSMA11A  55              PSMA11B  56           
   PSMA11C  57              PSMA12  58               PSMA12A  59           
   PSMA12B  60              PSMA12C  61              PSMA13  62            
   PSMA13A  63              PSMA13B  64              PSMA13C  65           
   PSMA14  66               PSMA14A  67              PSMA14B  68           
   PSMA14C  69              PSMA15  70               PSMA15A  71           
   PSMA15B  72              PSMA15C  73              PSMA16  74            
   PSMA16A  75              PSMA16B  76              PSMA16C  77           
   PSMA17  78               PSMA17A  79              PSMA17B  80           
   PSMA17C  81              PSMA18  82               PSMA18A  83           
   PSMA18B  84              PSMA18C  85              PSMA19  86            
   PSMA19A  87              PSMA19B  88              PSMA19C  89           
   PSMA20  90               PSMA20A  91              PSMA20B  92           
   PSMA20C  93              PSMA21  94               PSMA21B  95           
   PSMA21C  96              PSMA22  97               PSMA22A  98           
   PSMA22B  99              PSMA22C  100             PSMA23  101           
   PSMA23A  102             PSMA23B  103             PSMA23C  104          
   PSMA24  105              PSMA24A  106             PSMA24B  107          
   PSMA24C  108             PSMA25  109              PSMA25A  110          
   PSMA25B  111             PSMA25C  112             PSMA26  113           
   PSMA26A  114             PSMA26B  115             PSMA26C  116          
   PSMA27  117            ;                       

LABEL
   ID = "LONGSCAN SUBJECT ID"
   PSMAZ1 = "PSMAZ1"
   PSMAZ2 = "PSMAZ2"
   CENTER = "FIELD CENTER"
   VISIT = "VISIT NUMBER"
   PSMA1 = "Ever:Parents blamed you for problems"
   PSMA1A = "<EL SCHL:Parents blamed you for problems"
   PSMA1B = ">EL SCHL:Parents blamed you for problems"
   PSMA1C = "Last YR:Parents blamed you for problems"
   PSMA2 = "Ever:Parent called you names/teased you"
   PSMA2A = "<EL SCHL:Parent called you names/teased"
   PSMA2B = ">EL SCHL:Parent called you names/teased"
   PSMA2C = "Last YR:Parent called you names/teased"
   PSMA3 = "Ever:Punished by no sleep/eat/drink"
   PSMA3A = "<EL SCHL:Punished by no sleep/eat/drink"
   PSMA3B = ">EL SCHL:Punished by no sleep/eat/drink"
   PSMA3C = "Last YR:Punished by no sleep/eat/drink"
   PSMA4 = "Ever:Parent left w/o telling where"
   PSMA4A = "<EL SCHL:Parent left w/o telling where"
   PSMA4B = ">EL SCHL:Parent left w/o telling where"
   PSMA4C = "Last YR:Parent left w/o telling where"
   PSMA5 = "Ever:Made feel couldn't do right"
   PSMA5A = "<EL SCHL:Made feel couldn't do right"
   PSMA5B = ">EL SCHL:Made feel couldn't do right"
   PSMA5C = "Last YR:Made feel couldn't do right"
   PSMA6 = "Ever:Punished you in unusual way"
   PSMA6A = "<EL SCHL:Punished you in unusual way"
   PSMA6B = ">EL SCHL:Punished you in unusual way"
   PSMA6C = "Last YR:Punished you in unusual way"
   PSMA7 = "Ever:Didn't care if safe/healthy"
   PSMA7A = "<EL SCHL:Didn't care if safe/healthy"
   PSMA7B = ">EL SCHL:Didn't care if safe/healthy"
   PSMA7C = "Last YR:Didn't care if safe/healthy"
   PSMA8 = "Ever:Parent threatened to hurt you"
   PSMA8A = "<EL SCHL:Parent threatened to hurt you"
   PSMA8B = ">EL SCHL:Parent threatened to hurt you"
   PSMA8C = "Last YR:Parent threatened to hurt you"
   PSMA9 = "Ever:Parent threatened to kill you"
   PSMA9A = "<EL SCHL:Parent threatened to kill you"
   PSMA9B = ">EL SCHL:Parent threatened to kill you"
   PSMA9C = "Last YR:Parent threatened to kill you"
   PSMA10 = "Ever:Threat to abandon you forever"
   PSMA10A = "<EL SCHL:Threat to abandon you forever"
   PSMA10B = ">EL SCHL:Threat to abandon you forever"
   PSMA10C = "Last YR:Threat to abandon you forever"
   PSMA11 = "Ever:Threat to kick you out of home"
   PSMA11A = "<EL SCHL:Threat to kick you out of home"
   PSMA11B = ">EL SCHL:Threat to kick you out of home"
   PSMA11C = "Last YR:Threat to kick you out of home"
   PSMA12 = "Ever:Parent tried to kill self/other"
   PSMA12A = "<EL SCHL:Parent tried to kill self/other"
   PSMA12B = ">EL SCHL:Parent tried to kill self/other"
   PSMA12C = "Last YR:Parent tried to kill self/other"
   PSMA13 = "Ever:Made you feel didn't love you"
   PSMA13A = "<EL SCHL:Made you feel didn't love you"
   PSMA13B = ">EL SCHL:Made you feel didn't love you"
   PSMA13C = "Last YR:Made you feel didn't love you"
   PSMA14 = "Ever:Tried to stop having friends"
   PSMA14A = "<EL SCHL:Tried to stop having friends"
   PSMA14B = ">EL SCHL:Tried to stop having friends"
   PSMA14C = "Last YR:Tried to stop having friends"
   PSMA15 = "Ever:Parent seemed crazy"
   PSMA15A = "<EL SCHL:Parent seemed crazy"
   PSMA15B = ">EL SCHL:Parent seemed crazy"
   PSMA15C = "Last YR:Parent seemed crazy"
   PSMA16 = "Ever:Had you care for self or other"
   PSMA16A = "<EL SCHL:Had you care for self or other"
   PSMA16B = ">EL SCHL:Had you care for self or other"
   PSMA16C = "Last YR:Had you care for self or other"
   PSMA17 = "Ever:Made you steal/sex/carry drugs"
   PSMA17A = "<EL SCHL:Made you steal/sex/carry drugs"
   PSMA17B = ">EL SCHL:Made you steal/sex/carry drugs"
   PSMA17C = "Last YR:Made you steal/sex/carry drugs"
   PSMA18 = "Ever:Parent was drunk or high"
   PSMA18A = "<EL SCHL:Parent was drunk or high"
   PSMA18B = ">EL SCHL:Parent was drunk or high"
   PSMA18C = "Last YR:Parent was drunk or high"
   PSMA19 = "Ever:Threat to hurt important person"
   PSMA19A = "<EL SCHL:Threat to hurt important person"
   PSMA19B = ">EL SCHL:Threat to hurt important person"
   PSMA19C = "Last YR:Threat to hurt important person"
   PSMA20 = "Ever:Threat to hurt important thing"
   PSMA20A = "<EL SCHL:Threat to hurt important thing"
   PSMA20B = ">EL SCHL:Threat to hurt important thing"
   PSMA20C = "Last YR: Threat to hurt important thing"
   PSMA21 = "Ever:Kept home when not sick to help"
   PSMA21B = ">EL SCHL:Kept home when not sick to help"
   PSMA21C = "Last YR:Kept home when not sick to help"
   PSMA22 = "Ever:Wouldnt let you get medicl help"
   PSMA22A = "<EL SCHL:Wouldnt let you get medicl help"
   PSMA22B = ">EL SCHL:Wouldnt let you get medicl help"
   PSMA22C = "Last YR:Wouldnt let you get medicl help"
   PSMA23 = "Ever:Wouldnt let you get counseling"
   PSMA23A = "<EL SCHL:Wouldnt let you get counseling"
   PSMA23B = ">EL SCHL:Wouldnt let you get counseling"
   PSMA23C = "Last YR:Wouldnt let you get counseling"
   PSMA24 = "Ever:Blamed you for others problems"
   PSMA24A = "<EL SCHL:Blamed you for others problems"
   PSMA24B = ">EL SCHL:Blamed you for others problems"
   PSMA24C = "Last YR:Blamed you for others problems"
   PSMA25 = "Ever:Parents locked you out of house"
   PSMA25A = "<EL SCHL:Parents locked you out of house"
   PSMA25B = ">EL SCHL:Parents locked you out of house"
   PSMA25C = "Last YR:Parents locked you out of house"
   PSMA26 = "Ever:Embarrassed you by putting down"
   PSMA26A = "<EL SCHL:Embarrassed you by putting down"
   PSMA26B = ">EL SCHL:Embarrassed you by putting down"
   PSMA26C = "Last YR:Embarrassed you by putting down"
   PSMA27 = "How much feel fault for what's happened" ;

LENGTH
   VISIT 3                  PSMA1 3                  PSMA1A 3              
   PSMA1B 3                 PSMA1C 3                 PSMA2 3               
   PSMA2A 3                 PSMA2B 3                 PSMA2C 3              
   PSMA3 3                  PSMA3A 3                 PSMA3B 3              
   PSMA3C 3                 PSMA4 3                  PSMA4A 3              
   PSMA4B 3                 PSMA4C 3                 PSMA5 3               
   PSMA5A 3                 PSMA5B 3                 PSMA5C 3              
   PSMA6 3                  PSMA6A 3                 PSMA6B 3              
   PSMA6C 3                 PSMA7 3                  PSMA7A 3              
   PSMA7B 3                 PSMA7C 3                 PSMA8 3               
   PSMA8A 3                 PSMA8B 3                 PSMA8C 3              
   PSMA9 3                  PSMA9A 3                 PSMA9B 3              
   PSMA9C 3                 PSMA10 3                 PSMA10A 3             
   PSMA10B 3                PSMA10C 3                PSMA11 3              
   PSMA11A 3                PSMA11B 3                PSMA11C 3             
   PSMA12 3                 PSMA12A 3                PSMA12B 3             
   PSMA12C 3                PSMA13 3                 PSMA13A 3             
   PSMA13B 3                PSMA13C 3                PSMA14 3              
   PSMA14A 3                PSMA14B 3                PSMA14C 3             
   PSMA15 3                 PSMA15A 3                PSMA15B 3             
   PSMA15C 3                PSMA16 3                 PSMA16A 3             
   PSMA16B 3                PSMA16C 3                PSMA17 3              
   PSMA17A 3                PSMA17B 3                PSMA17C 3             
   PSMA18 3                 PSMA18A 3                PSMA18B 3             
   PSMA18C 3                PSMA19 3                 PSMA19A 3             
   PSMA19B 3                PSMA19C 3                PSMA20 3              
   PSMA20A 3                PSMA20B 3                PSMA20C 3             
   PSMA21 3                 PSMA21B 3                PSMA21C 3             
   PSMA22 3                 PSMA22A 3                PSMA22B 3             
   PSMA22C 3                PSMA23 3                 PSMA23A 3             
   PSMA23B 3                PSMA23C 3                PSMA24 3              
   PSMA24A 3                PSMA24B 3                PSMA24C 3             
   PSMA25 3                 PSMA25A 3                PSMA25B 3             
   PSMA25C 3                PSMA26 3                 PSMA26A 3             
   PSMA26B 3                PSMA26C 3                PSMA27 3 ;

        

RUN ;
