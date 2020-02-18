/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\esia0404.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = esia0404 ;

DATA library.&dataset ;
INFILE datafile LRECL=44;
INPUT
   id $ 1-7                 center $ 8-9             visit  10             
   esia1  11                esia2  12                esia3  13             
   esia4  14                esia5  15                esia6  16             
   esia7  17                esia8  18                esia9  19             
   esia10  20               esia11  21               esia12  22            
   esia13  23               esia14  24               esia15  25            
   esia16  26               esia17  27               esia18  28            
   esia19  29               esia20  30               esia21 $ 31-33        
   esia22 DATE11.         ;                       

LABEL
   id = "longscan subject id"
   center = "field center"
   visit = "visit number"
   esia1 = "too many responsibilities"
   esia2 = "take care other family mem."
   esia3 = "owing money-getting credit"
   esia4 = "problems with child's behavior"
   esia5 = "not enough money for necessities"
   esia6 = "not enough time"
   esia7 = "problems with transportation"
   esia8 = "problems with job or unemployment"
   esia9 = "disagreements re:  disciplining child"
   esia10 = "problems with housing"
   esia11 = "concerns re: others' health"
   esia12 = "concerns re: child in school-day care"
   esia13 = "problems with friends-neighbors"
   esia14 = "concerns re: children's health"
   esia15 = "problems getting along with family"
   esia16 = "problems with marital status"
   esia17 = "feeling safe in neighborhood"
   esia18 = "difficulties with children's father"
   esia19 = "problems holding a job"
   esia20 = "trouble finding employment"
   esia21 = "interviewer initials"
   esia22 = "date of interview" ;

FORMAT
   esia22 DATE9. ;
     

LENGTH
   visit 3                  esia1 3                  esia2 3               
   esia3 3                  esia4 3                  esia5 3               
   esia6 3                  esia7 3                  esia8 3               
   esia9 3                  esia10 3                 esia11 3              
   esia12 3                 esia13 3                 esia14 3              
   esia15 3                 esia16 3                 esia17 3              
   esia18 3                 esia19 3                 esia20 3 ;

        

RUN ;
