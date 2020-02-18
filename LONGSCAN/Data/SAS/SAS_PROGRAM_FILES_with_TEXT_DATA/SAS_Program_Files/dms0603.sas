/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\dms0603.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = dms0603 ;

DATA library.&dataset ;
INFILE datafile LRECL=19;
INPUT
   id $ 1-7                 visit  8                 center $ 9-10         
   dmpsyaga  11             dmpsyagb  12             dmpsyagc  13          
   dmminasa  14             dmminasb  15             dmminasc  16          
   dmsevasa  17             dmsevasb  18             dmsevasc  19          
 ;                       

LABEL
   id = "longscan subject id"
   visit = "visit number"
   center = "field center"
   dmpsyaga = "psychological aggression: caregiver only"
   dmpsyagb = "psychological aggression: others only"
   dmpsyagc = "psychological aggression: caregiver + others"
   dmminasa = "minor assault: caregiver only"
   dmminasb = "minor assault: others only"
   dmminasc = "minor assault: caregiver + others"
   dmsevasa = "severe assault: caregiver only"
   dmsevasb = "severe assault: others only"
   dmsevasc = "severe assault: caregiver + others" ;

LENGTH
   visit 3                  dmpsyaga 3               dmpsyagb 3            
   dmpsyagc 3               dmminasa 3               dmminasb 3            
   dmminasc 3               dmsevasa 3               dmsevasb 3            
   dmsevasc 3 ;

      

RUN ;
