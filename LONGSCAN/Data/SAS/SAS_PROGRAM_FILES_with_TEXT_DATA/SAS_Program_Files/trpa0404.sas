/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\trpa0404.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = trpa0404 ;

DATA library.&dataset ;
INFILE datafile LRECL=31;
INPUT
   id $ 1-7                 center $ 8-9             visit  10             
   trpa1  11                trpa2  12                trpa3  13             
   trpa4  14                trpa5  15                trpa6  16             
   trpa7  17                trpa8 $ 18-20            trpa9 DATE11.         
 ;                       

LABEL
   id = "longscan subject id"
   center = "field center"
   visit = "visit number"
   trpa1 = "child liked by classmates"
   trpa2 = "nomin.-like most for play-work prtnr"
   trpa3 = "nomin.-like least for play-work prtnr"
   trpa4 = "nominations-starts arguments and fights"
   trpa5 = "nominations-gets angry easily"
   trpa6 = "nominations-good at leading others"
   trpa7 = "nominations-gets picked on or teased"
   trpa8 = "interviewer initials"
   trpa9 = "date of interview" ;

FORMAT
   trpa9 DATE9. ;
      

LENGTH
   visit 3                  trpa1 3                  trpa2 3               
   trpa3 3                  trpa4 3                  trpa5 3               
   trpa6 3                  trpa7 3 ;

         

RUN ;
