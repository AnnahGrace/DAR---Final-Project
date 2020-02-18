/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\lsss0404.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = lsss0404 ;

DATA library.&dataset ;
INFILE datafile LRECL=11;
INPUT
   id $ 1-7                 center $ 8-9             visit  10             
   losspar  11            ;                       

LABEL
   id = "longscan subject id"
   center = "field center"
   visit = "visit number"
   losspar = "lssa: total of q 1-2 (loss of parents)" ;

LENGTH
   visit 3                  losspar 3 ;

       

RUN ;
