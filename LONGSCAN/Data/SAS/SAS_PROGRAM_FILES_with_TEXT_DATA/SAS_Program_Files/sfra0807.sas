/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\sfra0807.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = sfra0807 ;

DATA library.&dataset ;
INFILE datafile LRECL=36;
INPUT
   ID $ 1-7                 CENTER $ 8-9             VISIT  10-11          
   SFRA1  12                SFRA2  13                SFRA3  14             
   SFRA4  15                SFRA5  16                SFRA6  17             
   SFRA7  18                SFRA8  19                SFRA9  20             
   SFRA10  21               SFRA11  22               SFRA12 $ 23-25        
   SFRA13 DATE11.         ;                       

LABEL
   ID = "LONGSCAN SUBJECT ID"
   CENTER = "FIELD CENTER"
   VISIT = "VISIT NUMBER"
   SFRA1 = "Time to talk with child each day"
   SFRA2 = "Things we do each morning"
   SFRA3 = "Child has regular routine after school"
   SFRA4 = "Child sleeps in same place every night"
   SFRA5 = "Child does homework same time each day"
   SFRA6 = "Child has things to ask for at bedtime"
   SFRA7 = "Child goes to bed same time every night"
   SFRA8 = "Family eats dinner same time each night"
   SFRA9 = "Family eats at least one meal together"
   SFRA10 = "Family members check in/out w each other"
   SFRA11 = "Discipline child each time out of line"
   SFRA12 = "Interviewer initials"
   SFRA13 = "Date of interview" ;

FORMAT
   SFRA13 DATE9. ;
     

LENGTH
   VISIT 3                  SFRA1 3                  SFRA2 3               
   SFRA3 3                  SFRA4 3                  SFRA5 3               
   SFRA6 3                  SFRA7 3                  SFRA8 3               
   SFRA9 3                  SFRA10 3                 SFRA11 3 ;

        

RUN ;
