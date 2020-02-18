/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\sczs1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = sczs1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=98;
INPUT
   CENTER $ 1-2             ID $ 3-9                 VISIT  10-11          
   YSZ001  12               YSZ002  13               YSZ003  14            
   YSZ004  15               YSZ005  16               YSZ006  17            
   YSZ007  18               YSZ008  19               YSZ009  20            
   YSZ010  21               YSZ011  22               YSZ012  23            
   YSZ013  24               YSZ014  25               YSZ021  26            
   YSZ022  27               YSZ023  28               YSZ024  29            
   YSZ023C  30              YSZ024C  31              YSZ024E  32           
   YSZ003A  33              YSZ003B  34              YSZ003C  35           
   YSZ003D  36              YSZ003E  37              YSZ003F  38           
   YSZ004C  39              YSZ005C  40              YSZ005E  41           
   YSZ005F  42              YSZ005H  43              YSZ005I  44           
   YSZ005K  45              YSZ006C  46              YSZ007B  47           
   YSZ008B  48              YSZN01  49               YSZN02  50            
   YSZN03  51               YSZN12  52               YSZN14  53            
   YSZN16  54               YSZN17  55               YSZN19  56            
   YSZN23  57               YSZN25  58               YSZN27  59            
   YSZN33  60               YSZ025  61               YSZ026  62            
   YSZ027  63               YSZ028  64               YSZ033  65            
   YSZ034  66               YSZ035  67               YSZ036  68            
   YSZ037  69               YSZ024F  70              YSZ024H  71           
   YSZ024I  72              YSZ024K  73              YSZ025C  74           
   YSZ026B  75              YSZ027B  76              YSZ033C  77           
   YSZ034C  78              YSZ034E  79              YSZ034F  80           
   YSZ034H  81              YSZ034I  82              YSZ035C  83           
   YSZ036B  84              YSZ037B  85              YSZ034K  86           
   YSZN21  87               YSZA1  88                YSZA2  89             
   YSZA4  90                YSZNOTE  91              YSZA  92              
   YSZB  93                 YSZC  94                 YSZ  95               
   YSZCRIT  96              YSZSYMP  97-98         ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   VISIT = "VISIT NUMBER"
   YSZ001 = "Last yr:ever had vision when awake"
   YSZ002 = "Last yr:heard things"
   YSZ003 = "Last yr:hear voices"
   YSZ004 = "Last yr: did[event]happen for a mth"
   YSZ005 = "Last yr:tell Dr. about experiences"
   YSZ006 = "When u saw/heard things were you on meds"
   YSZ007 = "When u saw/heard thngs- on drgs/alochol"
   YSZ008 = "When u saw/heard th-ings-you phys.sick"
   YSZ009 = "Last yr:believed people were spying on u"
   YSZ010 = "Last yr:thght people were talking abt u"
   YSZ011 = "Last yr:believe someone plotting agnst u"
   YSZ012 = "Last yr:convinced thoughts put in mind"
   YSZ013 = "Last yr:believed special messages sent"
   YSZ014 = "Last yr:convinced you were under control"
   YSZ021 = "Last yr:believed u were being tested"
   YSZ022 = "Last yr:felt book etc was meant for you"
   YSZ023 = "Did you believe [named] things for a mth"
   YSZ024 = "Last yr:tell doctor about beliefs"
   YSZ023C = "Doctor give you meds for nerves"
   YSZ024C = "Did Dr. say meds caused these things"
   YSZ024E = "Believe these things when not on meds"
   YSZ003A = "Did voice come from your body"
   YSZ003B = "Hear voice commenting on you"
   YSZ003C = "Ever hear 2/>2 voices"
   YSZ003D = "Were voices discussing you"
   YSZ003E = "Ever carry 2way conversation with voices"
   YSZ003F = "Ever see who you were talking to"
   YSZ004C = "Did doctor give you any meds"
   YSZ005C = "Did Dr. say meds caused this"
   YSZ005E = "Ever see/hear things when not on meds"
   YSZ005F = "Did Dr. say drugs/alcohol caused this"
   YSZ005H = "Ever see/hear thngs whn no drugs/alcohol"
   YSZ005I = "Did Dr. say things caused by phys.sick"
   YSZ005K = "Ever see/hear things when not phys.sick"
   YSZ006C = "See/hear things when not on this meds"
   YSZ007B = "See/hear things when not on drgs/alochol"
   YSZ008B = "See/hear things when not phys.sick"
   YSZN01 = "Us respondent experince normal"
   YSZN02 = "Is experience described normal"
   YSZN03 = "Is experience described normal"
   YSZN12 = "Is experience/belief normal"
   YSZN14 = "INV response"
   YSZN16 = "Is experience/belief normal"
   YSZN17 = "Was response coded in Q1-14"
   YSZN19 = "Is experience/belief normal"
   YSZN23 = "Is experience/belief normal"
   YSZN25 = "Is experience/belief normal"
   YSZN27 = "Is experience/belief normal"
   YSZN33 = "Is experience/belief normal"
   YSZ025 = "When believed things were you on meds"
   YSZ026 = "Believed thngs were you on drugs/alcohol"
   YSZ027 = "When believed things were u phys sick"
   YSZ028 = "Last yr:bothered by strange smells"
   YSZ033 = "These [name] things happen for a mth"
   YSZ034 = "Tell a doctor about these experiences"
   YSZ035 = "When you smelled etc were you on med"
   YSZ036 = "Smelled etc. when taking drugs/alcohol"
   YSZ037 = "When smelled ect were you phys sick"
   YSZ024F = "Dr. say drgs/alcohol caused these things"
   YSZ024H = "Believe thngs when not on drugs/alcohol"
   YSZ024I = "Dr. say u believd thngs 'cause phys sick"
   YSZ024K = "Have strange beliefs when u weren't sick"
   YSZ025C = "Believed things when u weren't on meds"
   YSZ026B = "Believe thngs when not on drugs/alcohol"
   YSZ027B = "Have these beliefs when u weren't sick"
   YSZ033C = "Did Dr.give you meds for nerves"
   YSZ034C = "Dr. say this was because of meds"
   YSZ034E = "Smell etc when you weren't on this med"
   YSZ034F = "Dr. say smell etc 'cause of drugs/drink"
   YSZ034H = "Smell etc when u weren't takng drgs/drnk"
   YSZ034I = "Dr. say smell etc 'cause phys sick"
   YSZ035C = "Smelled etc. when u weren't taking med"
   YSZ036B = "Smelled etc -u weren't takng drgs/alcohl"
   YSZ037B = "Smelled etc when u weren't sick"
   YSZ034K = "Ever smell/taste these thngs/not sick"
   YSZN21 = "YSZN21"
   YSZA1 = "delusions"
   YSZA2 = "hallucinations"
   YSZA4 = "disorganized or catatonic behavior"
   YSZNOTE = "bizarre delusion or hallucinations"
   YSZA = "characteristic symptoms"
   YSZB = "social/occupational dysfunction"
   YSZC = "duration"
   YSZ = "diagnosis for schizophrenia"
   YSZCRIT = "Youth Schizophrenia Criteria Count"
   YSZSYMP = "Youth Schizophrenia Symptom Count" ;

LENGTH
   VISIT 3                  YSZ001 3                 YSZ002 3              
   YSZ003 3                 YSZ004 3                 YSZ005 3              
   YSZ006 3                 YSZ007 3                 YSZ008 3              
   YSZ009 3                 YSZ010 3                 YSZ011 3              
   YSZ012 3                 YSZ013 3                 YSZ014 3              
   YSZ021 3                 YSZ022 3                 YSZ023 3              
   YSZ024 3                 YSZ023C 3                YSZ024C 3             
   YSZ024E 3                YSZ003A 3                YSZ003B 3             
   YSZ003C 3                YSZ003D 3                YSZ003E 3             
   YSZ003F 3                YSZ004C 3                YSZ005C 3             
   YSZ005E 3                YSZ005F 3                YSZ005H 3             
   YSZ005I 3                YSZ005K 3                YSZ006C 3             
   YSZ007B 3                YSZ008B 3                YSZN01 3              
   YSZN02 3                 YSZN03 3                 YSZN12 3              
   YSZN14 3                 YSZN16 3                 YSZN17 3              
   YSZN19 3                 YSZN23 3                 YSZN25 3              
   YSZN27 3                 YSZN33 3                 YSZ025 3              
   YSZ026 3                 YSZ027 3                 YSZ028 3              
   YSZ033 3                 YSZ034 3                 YSZ035 3              
   YSZ036 3                 YSZ037 3                 YSZ024F 3             
   YSZ024H 3                YSZ024I 3                YSZ024K 3             
   YSZ025C 3                YSZ026B 3                YSZ027B 3             
   YSZ033C 3                YSZ034C 3                YSZ034E 3             
   YSZ034F 3                YSZ034H 3                YSZ034I 3             
   YSZ035C 3                YSZ036B 3                YSZ037B 3             
   YSZ034K 3                YSZN21 3                 YSZA1 3               
   YSZA2 3                  YSZA4 3                  YSZNOTE 3             
   YSZA 3                   YSZB 3                   YSZC 3                
   YSZ 3                    YSZCRIT 3                YSZSYMP 3 ;

       

RUN ;
