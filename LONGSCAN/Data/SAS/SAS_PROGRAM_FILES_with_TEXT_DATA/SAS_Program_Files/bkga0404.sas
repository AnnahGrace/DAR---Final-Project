/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\bkga0404.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = bkga0404 ;

DATA library.&dataset ;
INFILE datafile LRECL=45;
INPUT
   id $ 1-7                 center $ 8-9             visit  10             
   bkga1 DATE11.            bkga2a  22               bkga2b  23-24         
   bkga3  25                bkga4  26                bkga5  27             
   bkga6  28                bkga6a  29               bkga7  30             
   bkga8  31                bkga9 $ 32-34            bkga10 DATE11.        
 ;                       

LABEL
   id = "longscan subject id"
   center = "field center"
   visit = "visit number"
   bkga1 = "child:date of birth"
   bkga2a = "child:age-years"
   bkga2b = "child:age-months within years"
   bkga3 = "child:sex"
   bkga4 = "child:race"
   bkga5 = "child:1st language"
   bkga6 = "does child have a 2nd language"
   bkga6a = "child:2nd language"
   bkga7 = "child:birth position"
   bkga8 = "who makes decisions about child"
   bkga9 = "interviewer initials"
   bkga10 = "date of interview" ;

FORMAT
   bkga1 DATE9.             bkga10 DATE9. ;
     

LENGTH
   visit 3                  bkga2a 3                 bkga2b 3              
   bkga3 3                  bkga4 3                  bkga5 3               
   bkga6 3                  bkga6a 3                 bkga7 3               
   bkga8 3 ;

         

RUN ;
