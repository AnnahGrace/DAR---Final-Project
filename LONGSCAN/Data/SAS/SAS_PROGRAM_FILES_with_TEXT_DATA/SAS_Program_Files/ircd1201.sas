/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\ircd1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = ircd1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=72;
INPUT
   CENTER $ 1-2             ID $ 3-9                 IRCD14 $ 10-12        
   VISIT  13-14             IRCD1  15                IRCD2  16             
   IRCD3A  17               IRCD3B  18               IRCD3C  19            
   IRCD3D  20               IRCD3E  21               IRCD3F  22            
   IRCD3G  23               IRCD4  24                IRCD5  25             
   IRCD6  26                IRCD7A  27               IRCD7B  28            
   IRCD7C  29               IRCD7D  30               IRCD8  31             
   IRCD9  32                IRCD9A  33               IRCD9B  34            
   IRCD9C  35               IRCD9D  36               IRCD9E  37            
   IRCD9F  38               IRCD9G  39               IRCD9H  40            
   IRCD9I  41               IRCD9J  42               IRCD9K  43            
   IRCD9L  44               IRCD9M  45               IRCD9N  46            
   IRCD9O  47               IRCD9P  48               IRCD9Q  49            
   IRCD9R  50               IRCD9S  51               IRCD9T  52            
   IRCD9U  53               IRCD9V  54               IRCD10  55            
   IRCD10A  56              IRCD11  57-59            IRCD12  60            
   IRCD13  61               IRCD15 DATE11.         ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   IRCD14 = "Interviewer initials"
   VISIT = "VISIT NUMBER"
   IRCD1 = "How was the interview administered"
   IRCD2 = "Why was full A-CASI not administered"
   IRCD3A = "Not attentive at all/very attentive"
   IRCD3B = "Anxious/relaxed"
   IRCD3C = "Very tired/fresh and rested"
   IRCD3D = "Poorly groomed/well-groomed"
   IRCD3E = "Resistant/cooperative"
   IRCD3F = "Underweight/overweight"
   IRCD3G = "Depressed/cheerful"
   IRCD4 = "Child's comfort with the computer"
   IRCD5 = "How well did the youth seem to read"
   IRCD6 = "Interview environment"
   IRCD7A = "Interviewer present during interview"
   IRCD7B = "Teacher/school pers. present during int."
   IRCD7C = "Parent/Caretaker present during int."
   IRCD7D = "Other present during interview"
   IRCD8 = "Concerns about validity of interview"
   IRCD9 = "Concern about validity of instrument"
   IRCD9A = "Concern with WRAB"
   IRCD9B = "Concern with YDEA"
   IRCD9C = "Concern with NRIA"
   IRCD9D = "Concern with SCBB"
   IRCD9E = "Concern with EMPA"
   IRCD9F = "Concern with AHSA"
   IRCD9G = "Concern with TSCB"
   IRCD9H = "Concern with ASEC"
   IRCD9I = "Concern with PMCA"
   IRCD9J = "Concern with QRPA"
   IRCD9K = "Concern with ANMA"
   IRCD9L = "Concern with AWVA/HWVB"
   IRCD9M = "Concern with YPVA"
   IRCD9N = "Concern with PAAA"
   IRCD9O = "Concern with YPAA"
   IRCD9P = "Concern with YSAA"
   IRCD9Q = "Concern with RBFA"
   IRCD9R = "Concern with TADA"
   IRCD9S = "Concern with DELA"
   IRCD9T = "Concern with FEQB"
   IRCD9U = "Concern with YSSA"
   IRCD9V = "Concern with RSFB"
   IRCD10 = "Was the interview completed"
   IRCD10A = "Why was the interview not completed"
   IRCD11 = "How many minutes did the interview take"
   IRCD12 = "How many breaks were taken"
   IRCD13 = "Other comments or thoughts"
   IRCD15 = "Date of Interview" ;

FORMAT
   IRCD15 DATE9. ;
     

LENGTH
   VISIT 3                  IRCD1 3                  IRCD2 3               
   IRCD3A 3                 IRCD3B 3                 IRCD3C 3              
   IRCD3D 3                 IRCD3E 3                 IRCD3F 3              
   IRCD3G 3                 IRCD4 3                  IRCD5 3               
   IRCD6 3                  IRCD7A 3                 IRCD7B 3              
   IRCD7C 3                 IRCD7D 3                 IRCD8 3               
   IRCD9 3                  IRCD9A 3                 IRCD9B 3              
   IRCD9C 3                 IRCD9D 3                 IRCD9E 3              
   IRCD9F 3                 IRCD9G 3                 IRCD9H 3              
   IRCD9I 3                 IRCD9J 3                 IRCD9K 3              
   IRCD9L 3                 IRCD9M 3                 IRCD9N 3              
   IRCD9O 3                 IRCD9P 3                 IRCD9Q 3              
   IRCD9R 3                 IRCD9S 3                 IRCD9T 3              
   IRCD9U 3                 IRCD9V 3                 IRCD10 3              
   IRCD10A 3                IRCD11 4                 IRCD12 3              
   IRCD13 3 ;

        

RUN ;
