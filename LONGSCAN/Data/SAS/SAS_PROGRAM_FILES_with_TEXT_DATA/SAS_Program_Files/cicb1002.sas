/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\cicb1002.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = cicb1002 ;

DATA library.&dataset ;
INFILE datafile LRECL=28;
INPUT
   CENTER $ 1-2             ID $ 3-9                 CICB1 $ 10-12         
   VISIT  13-14             CICB3  15                CICB4  16             
   CICB5  17                CICB2 DATE11.          ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   CICB1 = "Interviewer initials"
   VISIT = "VISIT NUMBER"
   CICB3 = "Location of interview"
   CICB4 = "Is English first language"
   CICB5 = "Child's gender"
   CICB2 = "Date of interview" ;

FORMAT
   CICB2 DATE9. ;
      

LENGTH
   VISIT 3                  CICB3 3                  CICB4 3               
   CICB5 3 ;

         

RUN ;
