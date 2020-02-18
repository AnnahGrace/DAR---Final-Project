/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\ocds1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = ocds1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=88;
INPUT
   CENTER $ 1-2             ID $ 3-9                 VISIT  10-11          
   YOC001  12               YOC002  13               YOC003  14            
   YOC005  15               YOC006  16               YOC007  17            
   YOC008  18               YOCO1A1Y  19             YOCO1A2Y  20          
   YOCO1A3Y  21             YOCOA4  22               YOCO2A1Y  23          
   YOCO2A3Y  24             YOCO3A1Y  25             YOCO3A2Y  26          
   YOCO3A3Y  27             YOCC1A1Y  28             YOCC1A2Y  29          
   YOCC2A1Y  30             YOCC2A2Y  31             YOCC3A1Y  32          
   YOCC3A2Y  33             YOCC4A1Y  34             YOCC4A2Y  35          
   YOCO1A1M  36             YOCO1A2M  37             YOCO1A3M  38          
   YOCO2A1M  39             YOCO2A3M  40             YOCO3A1M  41          
   YOCO3A2M  42             YOCO3A3M  43             YOCC1A1M  44          
   YOCC1A2M  45             YOCC2A1M  46             YOCC2A2M  47          
   YOCC3A1M  48             YOCC3A2M  49             YOCC4A1M  50          
   YOCC4A2M  51             YOCC1CY  52              YOCC2CY  53           
   YOCC3CY  54              YOCC4CY  55              YOCC1CM  56           
   YOCC2CM  57              YOCC3CM  58              YOCC4CM  59           
   YOCE  60                 YOCOY  61                YOCOM  62             
   YOCCY  63                YOCCM  64                YOCY  65              
   YOCM  66                 YOCO3A2Z  67             YOCC4A1Z  68          
   YOCO3AZ  69              YOCC4AZ  70              YOCO3A2N  71          
   YOCO3AN  72              YOCC4A1N  73             YOCC4AN  74           
   YOCO3Z  75               YOCOZ  76                YOCO3N  77            
   YOCON  78                YOCC4Z  79               YOCCZ  80             
   YOCC4N  81               YOCCN  82                YOCZ  83              
   YOCN  84                 YOCCRITY  85-86          YOCCRITM  87          
   YOCSYMP  88            ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   VISIT = "VISIT NUMBER"
   YOC001 = "Last yr: worried things have germs"
   YOC002 = "Last yr: thoughts about doing bad thing"
   YOC003 = "Last yr:othr thghts/couldn't get rid of"
   YOC005 = "Last yr: washed hands etc. daily"
   YOC006 = "Check on things over and over again"
   YOC007 = "Had to count/do things over and over"
   YOC008 = "Last yr:do other thing over and over"
   YOCO1A1Y = "Recurrent and persistent thoughts about germs"
   YOCO1A2Y = "Not simply excessive worries about germs"
   YOCO1A3Y = "Attempts to ignore or suppress thoughts about germs"
   YOCOA4 = "Recognizes that obsession product of own mind"
   YOCO2A1Y = "Recurrent and persistent thoughts about doing something bad"
   YOCO2A3Y = "Attempts to ignore or suppress thoughts about doing something bad"
   YOCO3A1Y = "Recurrent and persistent thoughts - other"
   YOCO3A2Y = "Not simply excessive worries - other"
   YOCO3A3Y = "Attempts to ignore or suppress thoughts - other"
   YOCC1A1Y = "Repetitive behaviors - washing"
   YOCC1A2Y = "Washing aimed at preventing or reducing stress"
   YOCC2A1Y = "Repetitive behaviors - checking"
   YOCC2A2Y = "Checking aimed at preventing or reducing stress"
   YOCC3A1Y = "Repetitive behaviors - counting"
   YOCC3A2Y = "Counting aimed at preventing or reducing stress"
   YOCC4A1Y = "Repetitive behaviors - other"
   YOCC4A2Y = "Other behavior aimed at preventing or reducing stress"
   YOCO1A1M = "Recurrent and persistent thoughts about germs"
   YOCO1A2M = "Not simply excessive worries about germs"
   YOCO1A3M = "Attempts to ignore or suppress thoughts about germs"
   YOCO2A1M = "Recurrent and persistent thoughts about doing something bad"
   YOCO2A3M = "Attempts to ignore or suppress thoughts about doing something bad"
   YOCO3A1M = "Recurrent and persistent thoughts - other"
   YOCO3A2M = "Not simply excessive worries - other"
   YOCO3A3M = "Attempts to ignore or suppress thoughts - other"
   YOCC1A1M = "Repetitive behaviors - washing"
   YOCC1A2M = "Washing aimed at preventing or reducing stress"
   YOCC2A1M = "Repetitive behaviors - checking"
   YOCC2A2M = "Checking aimed at preventing or reducing stress"
   YOCC3A1M = "Repetitive behaviors - counting"
   YOCC3A2M = "Counting aimed at preventing or reducing stress"
   YOCC4A1M = "Repetitive behaviors - other"
   YOCC4A2M = "Other behavior aimed at preventing or reducing stress"
   YOCC1CY = "Distress, time-consuming, or interfere - washing"
   YOCC2CY = "Distress, time-consuming, or interfere - checking"
   YOCC3CY = "Distress, time-consuming, or interfere - counting"
   YOCC4CY = "Distress, time-consuming, or interfere - other"
   YOCC1CM = "Distress, time-consuming, or interfere - washing"
   YOCC2CM = "Distress, time-consuming, or interfere - checking"
   YOCC3CM = "Distress, time-consuming, or interfere - counting"
   YOCC4CM = "Distress, time-consuming, or interfere - other"
   YOCE = "duration - 2 weeks"
   YOCOY = "Year: diagnosis for obsession"
   YOCOM = "Month: diagnosis for obsession"
   YOCCY = "Year: diagnosis for compulsion"
   YOCCM = "Month: diagnosis for compulsion"
   YOCY = "Year: diagnosis for obsession-compulsion disorder"
   YOCM = "Month: diagnosis for obsession-compulsion disorder"
   YOCO3A2Z = "Possible Counting aimed at preventing or reducing stress"
   YOCC4A1Z = "Possible Repetitive behaviors - other"
   YOCO3AZ = "Possible Distress, time-consuming, or interfere - counting"
   YOCC4AZ = "Possible Distress, time-consuming, or interfere - other"
   YOCO3A2N = "Possible Not simply excessive worries - other"
   YOCO3AN = "Possible Distress, time-consuming, or interfere - counting"
   YOCC4A1N = "Possible Repetitive behaviors - other"
   YOCC4AN = "Possible Distress, time-consuming, or interfere - other"
   YOCO3Z = "Intermediate variable possible OCD past year"
   YOCOZ = "Year: Possible diagnosis for obsession"
   YOCO3N = "Intermediate variable possible OCD past month"
   YOCON = "Month: Possible diagnosis for obsession"
   YOCC4Z = "Intermediate variable possible OCD past year"
   YOCCZ = "Year: Possible diagnosis for compulsion"
   YOCC4N = "Intermediate variable possible OCD past month"
   YOCCN = "Intermediate variable possible OCD past month"
   YOCZ = "Year: Possible diagnosis for obsession-compulsion disorder"
   YOCN = "Month: Possible diagnosis for obsession-compulsion disorder"
   YOCCRITY = "Youth Obsessive Compulsive Disorder Criteria Count - Last Year"
   YOCCRITM = "Youth Obsessive Compulsive Disorder Criteria Count - Last Month"
   YOCSYMP = "Youth Obsessive Compulsive Disorder Symptom Count" ;

LENGTH
   VISIT 3                  YOC001 3                 YOC002 3              
   YOC003 3                 YOC005 3                 YOC006 3              
   YOC007 3                 YOC008 3                 YOCO1A1Y 3            
   YOCO1A2Y 3               YOCO1A3Y 3               YOCOA4 3              
   YOCO2A1Y 3               YOCO2A3Y 3               YOCO3A1Y 3            
   YOCO3A2Y 3               YOCO3A3Y 3               YOCC1A1Y 3            
   YOCC1A2Y 3               YOCC2A1Y 3               YOCC2A2Y 3            
   YOCC3A1Y 3               YOCC3A2Y 3               YOCC4A1Y 3            
   YOCC4A2Y 3               YOCO1A1M 3               YOCO1A2M 3            
   YOCO1A3M 3               YOCO2A1M 3               YOCO2A3M 3            
   YOCO3A1M 3               YOCO3A2M 3               YOCO3A3M 3            
   YOCC1A1M 3               YOCC1A2M 3               YOCC2A1M 3            
   YOCC2A2M 3               YOCC3A1M 3               YOCC3A2M 3            
   YOCC4A1M 3               YOCC4A2M 3               YOCC1CY 3             
   YOCC2CY 3                YOCC3CY 3                YOCC4CY 3             
   YOCC1CM 3                YOCC2CM 3                YOCC3CM 3             
   YOCC4CM 3                YOCE 3                   YOCOY 3               
   YOCOM 3                  YOCCY 3                  YOCCM 3               
   YOCY 3                   YOCM 3                   YOCO3A2Z 3            
   YOCC4A1Z 3               YOCO3AZ 3                YOCC4AZ 3             
   YOCO3A2N 3               YOCO3AN 3                YOCC4A1N 3            
   YOCC4AN 3                YOCO3Z 3                 YOCOZ 3               
   YOCO3N 3                 YOCON 3                  YOCC4Z 3              
   YOCCZ 3                  YOCC4N 3                 YOCCN 3               
   YOCZ 3                   YOCN 3                   YOCCRITY 3            
   YOCCRITM 3               YOCSYMP 3 ;

       

RUN ;
