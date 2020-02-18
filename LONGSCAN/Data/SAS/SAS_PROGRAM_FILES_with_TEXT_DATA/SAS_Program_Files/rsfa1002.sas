/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\rsfa1002.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = rsfa1002 ;

DATA library.&dataset ;
INFILE datafile LRECL=31;
INPUT
   CENTER $ 1-2             ID $ 3-9                 VISIT  10-11          
   RSFA1  12                RSFA2A  13               RSFA2B  14            
   RSFA2C  15               RSFA3  16                RSFA4  17             
   RSFA5  18                RSFA6  19                RSFA7  20             
   RSFA8  21                RSFA9  22                RSFA10  23            
   RSFA11  24               RSFA12  25               RSFA13  26            
   RSFA14  27               RSFA15  28               RSFA16  29            
   RSFA17  30               RSFA18  31             ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   VISIT = "VISIT NUMBER"
   RSFA1 = "Adult who can help w/serious problems"
   RSFA2A = "Could go to parent w/serious problem"
   RSFA2B = "Could go to relative w/serious problem"
   RSFA2C = "Could go to other adult w/serious problm"
   RSFA3 = "Adult who encouraged/believed in you"
   RSFA4 = "Has this made a difference in your life"
   RSFA5 = "How important is religion/spirituality"
   RSFA6 = "Times attended religious service past yr"
   RSFA7 = "Been part of a sports team"
   RSFA8 = "Been captain or cocaptain of a team"
   RSFA9 = "Won a sports medal/ribbon/trophy/award"
   RSFA10 = "Been a member of a club at school"
   RSFA11 = "Been officer/leader in club/organization"
   RSFA12 = "Received a school award or prize"
   RSFA13 = "Been on the honor roll"
   RSFA14 = "Been part of a performing arts group"
   RSFA15 = "Been part of a scout troop"
   RSFA16 = "Been part of a volunteer group"
   RSFA17 = "Been part of a church group"
   RSFA18 = "Received volunteer/comm. service award" ;

LENGTH
   VISIT 3                  RSFA1 3                  RSFA2A 3              
   RSFA2B 3                 RSFA2C 3                 RSFA3 3               
   RSFA4 3                  RSFA5 3                  RSFA6 3               
   RSFA7 3                  RSFA8 3                  RSFA9 3               
   RSFA10 3                 RSFA11 3                 RSFA12 3              
   RSFA13 3                 RSFA14 3                 RSFA15 3              
   RSFA16 3                 RSFA17 3                 RSFA18 3 ;

        

RUN ;
