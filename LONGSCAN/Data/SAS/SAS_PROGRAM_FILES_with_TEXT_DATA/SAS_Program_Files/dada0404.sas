/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\dada0404.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = dada0404 ;

DATA library.&dataset ;
INFILE datafile LRECL=66;
INPUT
   id $ 1-7                 center $ 8-9             visit  10             
   dada1a  11               dada2  12                dada2a $ 13-27        
   dada3  28                dada4  29                dada5  30             
   dada6  31                dada7a  32               dada8  33             
   dada8a $ 34-48           dada9  49                dada10  50            
   dada11  51               dada12  52               dada13 $ 53-55        
   dada14 DATE11.         ;                       

LABEL
   id = "longscan subject id"
   center = "field center"
   visit = "visit number"
   dada1a = "presence of father in child^s life"
   dada2 = "relation of father-fig to child"
   dada2a = "description of other primary father-fig"
   dada3 = "time father-figure #1 spends with child"
   dada4 = "how much father-fig #1 cares about child"
   dada5 = "amt father-fig #1 gives to everyday care"
   dada6 = "financial help of father-fig #1 to child"
   dada7a = "child has more than one father-figure"
   dada8 = "relation of 2nd father-figure to child"
   dada8a = "description of other 2nd father-fig"
   dada9 = "time 2nd father-fig. spends with child"
   dada10 = "howmuch 2nd father-fig cares about child"
   dada11 = "amt father-fig #2 gives to everyday care"
   dada12 = "financial help of father-fig #2 to child"
   dada13 = "interviewer initials"
   dada14 = "date of interview" ;

FORMAT
   dada14 DATE9. ;
     

LENGTH
   visit 3                  dada1a 3                 dada2 3               
   dada3 3                  dada4 3                  dada5 3               
   dada6 3                  dada7a 3                 dada8 3               
   dada9 3                  dada10 3                 dada11 3              
   dada12 3 ;

        

RUN ;
