/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\odds1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = odds1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=45;
INPUT
   CENTER $ 1-2             ID $ 3-9                 VISIT  10-11          
   YOD001  12               YOD002  13               YOD003  14            
   YOD004  15               YOD005  16               YOD006  17            
   YOD007  18               YOD008  19               YOD009  20            
   YOD010  21               YOD011  22               YOD012  23            
   YODA1Y  24               YODA2Y  25               YODA3Y  26            
   YODA4Y  27               YODA5Y  28               YODA6Y  29            
   YODA7Y  30               YODA8Y  31               YODA1M  32            
   YODA2M  33               YODA3M  34               YODA4M  35            
   YODA5M  36               YODA6M  37               YODA7M  38            
   YODA8M  39               YODY  40                 YODM  41              
   YODCRITY  42             YODCRITM  43             YODSYMP  44-45        
 ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   VISIT = "VISIT NUMBER"
   YOD001 = "Last yr:lost your temper."
   YOD002 = "Last yr:argued with authority"
   YOD003 = "Last yr:done things purposely"
   YOD004 = "Last yr:refused to obey authority"
   YOD005 = "Done things to annoy other people"
   YOD006 = "Last yr:blamed someone for mistakes"
   YOD007 = "Last yr:have you been grouchy"
   YOD008 = "Last yr:been mad at people/things"
   YOD009 = "Last yr:have you gotten angry"
   YOD010 = "Last yr:done mean things to people"
   YOD011 = "Last yr: gotten even wth othr people"
   YOD012 = "Last yr:people complained-swear"
   YODA1Y = "past year: loses temper"
   YODA2Y = "past year: argues with adults"
   YODA3Y = "past year: actively defies or refuses adults"
   YODA4Y = "past year: deliberately annoys people"
   YODA5Y = "past year: blames others"
   YODA6Y = "past year: touchy or easily annoyed"
   YODA7Y = "past year: angry and resentful"
   YODA8Y = "past year: spiteful or vindictive"
   YODA1M = "past month: loses temper"
   YODA2M = "past month: argues with adults"
   YODA3M = "past month: actively defies or refuses adults"
   YODA4M = "past month: deliberately annoys people"
   YODA5M = "past month: blames others"
   YODA6M = "past month: touchy or easily annoyed"
   YODA7M = "past month: angry and resentful"
   YODA8M = "past month: spiteful or vindictive"
   YODY = "past year: diagnosis for ODD"
   YODM = "past month: diagnosis for ODD"
   YODCRITY = "Youth ODD Criteria Count - Last Year"
   YODCRITM = "Youth ODD Criteria Count - Last Month"
   YODSYMP = "Youth ODD - Symptom Count" ;

LENGTH
   VISIT 3                  YOD001 3                 YOD002 3              
   YOD003 3                 YOD004 3                 YOD005 3              
   YOD006 3                 YOD007 3                 YOD008 3              
   YOD009 3                 YOD010 3                 YOD011 3              
   YOD012 3                 YODA1Y 3                 YODA2Y 3              
   YODA3Y 3                 YODA4Y 3                 YODA5Y 3              
   YODA6Y 3                 YODA7Y 3                 YODA8Y 3              
   YODA1M 3                 YODA2M 3                 YODA3M 3              
   YODA4M 3                 YODA5M 3                 YODA6M 3              
   YODA7M 3                 YODA8M 3                 YODY 3                
   YODM 3                   YODCRITY 3               YODCRITM 3            
   YODSYMP 3 ;

       

RUN ;
