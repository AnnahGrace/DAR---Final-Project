/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\asua0404.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = asua0404 ;

DATA library.&dataset ;
INFILE datafile LRECL=90;
INPUT
   id $ 1-7                 center $ 8-9             visit  10             
   asua1a  11               asua1b  12-13            asua1c  14-15         
   asua1d  16-17            asua2a  18               asua2b  19-20         
   asua2c  21-22            asua2d  23-24            asua3a  25            
   asua3b  26               asua3c  27               asua3d  28            
   asua3e  29               asua3f  30               asua3g  31            
   asua3h  32               asua3i  33               asua3j  34            
   asua3j1 $ 35-54          asua4  55                asua5  56-57          
   asua6 $ 58               asua7  59-60             asua8  61             
   asua9a  62               asua9b  63               asua9c  64-65         
   asua10  66               asua11a  67              asua11b  68-70        
   asua11c  71-73           asua11d  74-76           asua12 $ 77-79        
   asua13 DATE11.         ;                       

LABEL
   id = "longscan subject id"
   center = "field center"
   visit = "visit number"
   asua1a = "respondent sought self-help"
   asua1b = "code #1 for self help sought by resp"
   asua1c = "code #2 for self help sought by resp"
   asua1d = "code #3 for self help sought by resp"
   asua2a = "respd. consider help for emtnl prob."
   asua2b = "code 1 help considerd by resp emtnl prob"
   asua2c = "code 2 help considerd by resp emtnl prob"
   asua2d = "code 3 help considerd by resp emtnl prob"
   asua3a = "respd. saw mental health prof."
   asua3b = "respd. saw health care professional"
   asua3c = "respd. saw social services worker"
   asua3d = "respd. saw work counselor"
   asua3e = "respd. saw substance abuse couns."
   asua3f = "respd. saw lawyer-probation off."
   asua3g = "respd. saw preacher-relig. couns."
   asua3h = "respd. saw natural healer"
   asua3i = "respd. saw self help group"
   asua3j = "respd. saw other provider"
   asua3j1 = "identity of other provider"
   asua4 = "# of providers mentioned"
   asua5 = "# of visits to providers"
   asua6 = "provider most often seen"
   asua7 = "# of visits to primary provider"
   asua8 = "how satisfied with primary provider"
   asua9a = "hosp. for mental-emotional prob."
   asua9b = "# of times hosp. for mental-emot. prob."
   asua9c = "# days in hospital mental-emot. prob"
   asua10 = "res. treatment prgm for subst. abuse"
   asua11a = "medications currently taking"
   asua11b = "medication code #1"
   asua11c = "medication code #2"
   asua11d = "medication code #3"
   asua12 = "interviewer initials"
   asua13 = "date of interview" ;

FORMAT
   asua13 DATE9. ;
     

LENGTH
   visit 3                  asua1a 3                 asua1b 3              
   asua1c 3                 asua1d 3                 asua2a 3              
   asua2b 3                 asua2c 3                 asua2d 3              
   asua3a 3                 asua3b 3                 asua3c 3              
   asua3d 3                 asua3e 3                 asua3f 3              
   asua3g 3                 asua3h 3                 asua3i 3              
   asua3j 3                 asua4 3                  asua5 3               
   asua7 3                  asua8 3                  asua9a 3              
   asua9b 3                 asua9c 3                 asua10 3              
   asua11a 3                asua11b 4                asua11c 4             
   asua11d 4 ;

       

RUN ;
