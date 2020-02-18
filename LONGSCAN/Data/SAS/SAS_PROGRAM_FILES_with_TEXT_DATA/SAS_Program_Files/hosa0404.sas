/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\hosa0404.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = hosa0404 ;

DATA library.&dataset ;
INFILE datafile LRECL=44;
INPUT
   id $ 1-7                 center $ 8-9             visit  10             
   hosa1  11                hosa2  12                hosa3  13             
   hosa4  14                hosa5  15                hosa6  16             
   hosa7  17                hosa8  18                hosa9  19             
   hosa10  20               hosa11  21               hosa12  22            
   hosa13  23               hosa14  24               hosa15  25            
   hosa16  26               hosa17  27               hosa18  28            
   hosa19  29               hosa20  30               hosa21 $ 31-33        
   hosa22 DATE11.         ;                       

LABEL
   id = "longscan subject id"
   center = "field center"
   visit = "visit number"
   hosa1 = "hands tremble"
   hosa2 = "sweating hands or feet"
   hosa3 = "heart beating hard"
   hosa4 = "tired in the morning"
   hosa5 = "trouble getting or staying asleep"
   hosa6 = "upset stomach"
   hosa7 = "nightmares"
   hosa8 = "cold sweats"
   hosa9 = "many ailments"
   hosa10 = "loss of appetite"
   hosa11 = "illness effect work amount"
   hosa12 = "weak all over"
   hosa13 = "dizziness"
   hosa14 = "lose weight when worrying"
   hosa15 = "shortness of breath"
   hosa16 = "healthy enough"
   hosa17 = "smoke"
   hosa18 = "particular health problem"
   hosa19 = "nervousness"
   hosa20 = "nervous breakdown"
   hosa21 = "interviewer initials"
   hosa22 = "date of interview" ;

FORMAT
   hosa22 DATE9. ;
     

LENGTH
   visit 3                  hosa1 3                  hosa2 3               
   hosa3 3                  hosa4 3                  hosa5 3               
   hosa6 3                  hosa7 3                  hosa8 3               
   hosa9 3                  hosa10 3                 hosa11 3              
   hosa12 3                 hosa13 3                 hosa14 3              
   hosa15 3                 hosa16 3                 hosa17 3              
   hosa18 3                 hosa19 3                 hosa20 3 ;

        

RUN ;
