/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\wraa0807.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = wraa0807 ;

DATA library.&dataset ;
INFILE datafile LRECL=17;
INPUT
   ID $ 1-7                 CENTER $ 8-9             VISIT  10-11          
   WRAA1  12                WRAA2  13                WRAA3  14-15          
   WRAA4  16-17           ;                       

LABEL
   ID = "LONGSCAN SUBJECT ID"
   CENTER = "FIELD CENTER"
   VISIT = "VISIT NUMBER"
   WRAA1 = "Type of form"
   WRAA2 = "Grade in school"
   WRAA3 = "Reading Raw Score"
   WRAA4 = "Math Raw Score" ;

LENGTH
   VISIT 3                  WRAA1 3                  WRAA2 3               
   WRAA3 3                  WRAA4 3 ;

         

RUN ;
