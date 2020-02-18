/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\tobs1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = tobs1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=67;
INPUT
   CENTER $ 1-2             ID $ 3-9                 VISIT  10-11          
   YNI001  12               YNI002  13               YNI003  14            
   YNI004  15               YNI006  16               YNI008  17            
   YNI009  18               YNI010  19               YNI011  20            
   YNI012  21               YNI013  22               YNI014  23            
   YNI015  24               YNI016  25               YNI017  26            
   YNI018  27               YNI020  28               YNI021  29            
   YNI022  30               YNI023  31               YNI024  32            
   YNI025  33               YNI026  34               YNI028  35            
   YNI029  36               YNI030  37               YNI032  38            
   YNI033  39               YNI034  40               YNI035  41            
   YNI038  42               YNI039  43               YNI040  44            
   YNI041  45               YNI017A  46              YNI017B  47           
   YNI017C  48              YNI017D  49              YNI017E  50           
   YNI017F  51              YNI017H  52              YNI017I  53           
   YNI031  54               YNIDA1  55               YNIDA2A  56           
   YNIDA2B  57              YNIDA2  58               YNIDA3  59            
   YNIDA4  60               YNIDA5  61               YNIDA6  62            
   YNIDA7  63               YNIDA  64                YNID  65              
   YNIDCRIT  66             YNISYMP  67            ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   VISIT = "VISIT NUMBER"
   YNI001 = "Lifetime: ever smoked cigarettes"
   YNI002 = "Lifetime: ever used snuff/chewing TOBBco"
   YNI003 = "Last yr: smoked once a wk for a mth/more"
   YNI004 = "Last yr: how often when smkd cigar most"
   YNI006 = "When used snuff/chw TOBBc how often used"
   YNI008 = "Last yr: smked/chewed more than meant to"
   YNI009 = "Last yr: tried to quit smoking/TOBBcco"
   YNI010 = "Last yr: needed to smoke/use snuff more"
   YNI011 = "Last yr: often chain-smoked"
   YNI012 = "Last yr: often used 1 dip after another"
   YNI013 = "Last yr: not stop desire to smoke"
   YNI014 = "Last yr: cut down on things due to smoke"
   YNI015 = "Last yr: physical problems due to smokng"
   YNI016 = "Unable to do sports due to smoking/snuff"
   YNI017 = "Last yr: felt bad if you cut smoking"
   YNI018 = "Last yr: smoked to prevent feeling sick"
   YNI020 = "Last yr- did family/friends get upset"
   YNI021 = "Did smoking etc prevent family things"
   YNI022 = "Did smoking prevent things with others"
   YNI023 = "When prblms. worst-homework etc hard"
   YNI024 = "Did smoking upset teacher/boss"
   YNI025 = "When prblms.worst-caring for self hard"
   YNI026 = "Last yr: seen someone in hospital"
   YNI028 = "Last 12mth-ever smoked once a wk per mth"
   YNI029 = "Last 12mth-ever smoked everday for 1 mth"
   YNI030 = "Since 1st smoke needed more cigarette"
   YNI032 = "Tried to stop smoking but couldn't"
   YNI033 = "Ever chain-smke/use one dip aftr another"
   YNI034 = "Ever give up doing things due to smoking"
   YNI035 = "Smoking ever cause phys/health problem"
   YNI038 = "Ever see doctor etc. because of smoking"
   YNI039 = "Ever go to group meeting due to smoking"
   YNI040 = "Did smoking bother you/make you unhappy"
   YNI041 = "Ever did these things same 12-mth period"
   YNI017A = "Last yr: felt more angry when cut smkng"
   YNI017B = "Last yr: felt anxious when cut smoking"
   YNI017C = "Last yr: felt sad when cut smoking"
   YNI017D = "Last yr: felt restless when cut smoking"
   YNI017E = "Last yr: had trbl sleepng when cut smkng"
   YNI017F = "Last yr: trbl concentrtng when cut smkng"
   YNI017H = "Last yr: gained weight when cut smoking"
   YNI017I = "Last yr: ate more when cut smoking"
   YNI031 = "Often felt sick when stopped smoking"
   YNIDA1 = "Tolerance"
   YNIDA2A = "Withdrawal syndrome"
   YNIDA2B = "Substance taken to relieve or avoid withdrawal symptoms"
   YNIDA2 = "Withdrawal"
   YNIDA3 = "Substance taken in larger amounts or longer"
   YNIDA4 = "Persistent desire or unsuccessful efforts to cut down"
   YNIDA5 = "Great deal of time spent to obtain, use, or recover"
   YNIDA6 = "Activities given up or reduced"
   YNIDA7 = "Use despite physical or psychological problem"
   YNIDA = "Maladaptive pattern of substance use, three or more symptoms"
   YNID = "Diagnosis for nicotine dependence"
   YNIDCRIT = "Youth Nicotine Dependence Criteria Count"
   YNISYMP = "Youth Nicotine Dependence - Symptom Count" ;

LENGTH
   VISIT 3                  YNI001 3                 YNI002 3              
   YNI003 3                 YNI004 3                 YNI006 3              
   YNI008 3                 YNI009 3                 YNI010 3              
   YNI011 3                 YNI012 3                 YNI013 3              
   YNI014 3                 YNI015 3                 YNI016 3              
   YNI017 3                 YNI018 3                 YNI020 3              
   YNI021 3                 YNI022 3                 YNI023 3              
   YNI024 3                 YNI025 3                 YNI026 3              
   YNI028 3                 YNI029 3                 YNI030 3              
   YNI032 3                 YNI033 3                 YNI034 3              
   YNI035 3                 YNI038 3                 YNI039 3              
   YNI040 3                 YNI041 3                 YNI017A 3             
   YNI017B 3                YNI017C 3                YNI017D 3             
   YNI017E 3                YNI017F 3                YNI017H 3             
   YNI017I 3                YNI031 3                 YNIDA1 3              
   YNIDA2A 3                YNIDA2B 3                YNIDA2 3              
   YNIDA3 3                 YNIDA4 3                 YNIDA5 3              
   YNIDA6 3                 YNIDA7 3                 YNIDA 3               
   YNID 3                   YNIDCRIT 3               YNISYMP 3 ;

       

RUN ;
