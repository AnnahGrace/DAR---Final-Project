/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\pcct1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = pcct1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=47;
INPUT
   CENTER $ 1-2             ID $ 3-9                 PCCT23 $ 10-12        
   VISIT  13-14             PCCT1  15                PCCT2  16             
   PCCT3  17                PCCT4  18                PCCT5  19             
   PCCT6  20                PCCT7  21                PCCT8  22             
   PCCT9  23                PCCT10  24               PCCT11  25            
   PCCT12  26               PCCT13  27               PCCT14  28            
   PCCT15  29               PCCT16  30               PCCT17  31            
   PCCT18  32               PCCT19  33               PCCT20  34            
   PCCT21  35               PCCT22  36               PCCT24 DATE11.        
 ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   PCCT23 = "Interviewer initials"
   VISIT = "VISIT NUMBER"
   PCCT1 = "Explained why something was wrong"
   PCCT2 = "Put him/her in 'time out'/sent to room"
   PCCT3 = "Shook him/her"
   PCCT4 = "Hit him/her on bottom with object"
   PCCT5 = "Gave him/her something else to do"
   PCCT6 = "Shouted, yelled, or screamed at him/her"
   PCCT7 = "Hit him/her with fist or kicked hard"
   PCCT8 = "Spanked him/her on the bottom with hand"
   PCCT9 = "Grabbed and choked him/her around neck"
   PCCT10 = "Swore or cursed at him/her"
   PCCT11 = "Beat him/her up"
   PCCT12 = "Said you would send him/her away"
   PCCT13 = "Burned or scalded him/her on purpose"
   PCCT14 = "Threatened to spank or hit him/her"
   PCCT15 = "Hit him/her not on bottom with object"
   PCCT16 = "Slapped him/her on the hand arm or leg"
   PCCT17 = "Took away privileges or grounded him/her"
   PCCT18 = "Pinched him/her"
   PCCT19 = "Threatened him/her with a knife or gun"
   PCCT20 = "Threw or knocked him/her down"
   PCCT21 = "Called him/her dumb/lazy or other name"
   PCCT22 = "Slapped him/her on face head or ears"
   PCCT24 = "Date of interview" ;

FORMAT
   PCCT24 DATE9. ;
     

LENGTH
   VISIT 3                  PCCT1 3                  PCCT2 3               
   PCCT3 3                  PCCT4 3                  PCCT5 3               
   PCCT6 3                  PCCT7 3                  PCCT8 3               
   PCCT9 3                  PCCT10 3                 PCCT11 3              
   PCCT12 3                 PCCT13 3                 PCCT14 3              
   PCCT15 3                 PCCT16 3                 PCCT17 3              
   PCCT18 3                 PCCT19 3                 PCCT20 3              
   PCCT21 3                 PCCT22 3 ;

        

RUN ;
