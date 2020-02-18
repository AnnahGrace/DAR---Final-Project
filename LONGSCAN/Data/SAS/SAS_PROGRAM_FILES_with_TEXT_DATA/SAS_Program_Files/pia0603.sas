/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\pia0603.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = pia0603 ;

DATA library.&dataset ;
INFILE datafile LRECL=122;
INPUT
   id $ 1-7                 visit  8                 center $ 9-10         
   pia1 $ 11-13             pia3b $ 14-37            pia4a $ 38-64         
   pia5a $ 65-105           pia6 $ 106               pia3  107-108         
   pia3a  109               pia4  110                pia5  111             
   pia2 DATE11.           ;                       

LABEL
   id = "longscan subject id"
   visit = "visit number"
   center = "field center"
   pia1 = "interviewer initials"
   pia3b = "describe other female/other male"
   pia4a = "describe first language spoken"
   pia5a = "describe other location"
   pia6 = "computer or paper/pencil"
   pia3 = "respondent-primary relationship to child"
   pia3a = "kinship foster: what is biol relationshp"
   pia4 = "language of administration"
   pia5 = "location of interview"
   pia2 = "date of interview" ;

FORMAT
   pia2 DATE9. ;
       

LENGTH
   visit 3                  pia3 3                   pia3a 3               
   pia4 3                   pia5 3 ;

          

RUN ;
