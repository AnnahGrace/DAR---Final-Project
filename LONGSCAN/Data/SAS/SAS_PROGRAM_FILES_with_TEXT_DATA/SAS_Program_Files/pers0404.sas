/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\pers0404.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = pers0404 ;

DATA library.&dataset ;
INFILE datafile LRECL=27;
INPUT
   id $ 1-7                 center $ 8-9             visit  10             
   lbw  11                  respndt $ 12-27        ;                       

LABEL
   id = "longscan subject id"
   center = "field center"
   visit = "visit number"
   lbw = "pera: low birth weight (1=yes,0=no)"
   respndt = "respondent relationship to child" ;

LENGTH
   visit 3                  lbw 3 ;

           

RUN ;
