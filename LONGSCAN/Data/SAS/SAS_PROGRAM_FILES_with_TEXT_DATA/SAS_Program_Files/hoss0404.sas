/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\hoss0404.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = hoss0404 ;

DATA library.&dataset ;
INFILE datafile LRECL=12;
INPUT
   id $ 1-7                 center $ 8-9             visit  10             
   hosatot  11-12         ;                       

LABEL
   id = "longscan subject id"
   center = "field center"
   visit = "visit number"
   hosatot = "hosa: total score" ;

LENGTH
   visit 3                  hosatot 3 ;

       

RUN ;
