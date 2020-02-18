/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\cicc1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = cicc1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=28;
INPUT
   CENTER $ 1-2             ID $ 3-9                 CICC1 $ 10-12         
   VISIT  13-14             CICC3  15                CICC4  16             
   CICC5  17                CICC2 DATE11.          ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   CICC1 = "Interviewer initials"
   VISIT = "VISIT NUMBER"
   CICC3 = "Location of interview"
   CICC4 = "Is English first language"
   CICC5 = "Child's gender"
   CICC2 = "Date of interview" ;

FORMAT
   CICC2 DATE9. ;
      

LENGTH
   VISIT 3                  CICC3 3                  CICC4 3               
   CICC5 3 ;

         

RUN ;
