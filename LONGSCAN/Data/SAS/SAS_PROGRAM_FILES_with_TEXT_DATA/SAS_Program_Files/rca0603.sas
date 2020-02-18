/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\rca0603.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = rca0603 ;

DATA library.&dataset ;
INFILE datafile LRECL=1059;
INPUT
   id $ 1-7                 visit  8                 center $ 9-10         
   rca7a2 $ 11-43           rca7a3 $ 44-112          rca7b2 $ 113-144      
   rca7b3 $ 145-213         rca7c2 $ 214-282         rca7d2 $ 283-313      
   rca7d3 $ 314-382         rca7e2 $ 383-411         rca7e3 $ 412-480      
   rca7f2 $ 481-507         rca7f3 $ 508-576         rca7g2 $ 577-604      
   rca7g3 $ 605-673         rca7h2 $ 674-742         rca8a1 $ 743-814      
   rca8a2 $ 815-886         rca8a3 $ 887-958         rca8a4 $ 959-1030     
   rca9 $ 1031-1033         rca1  1034               rca2  1035            
   rca3  1036               rca4  1037               rca5  1038            
   rca6  1039               rca7a1  1040             rca7b1  1041          
   rca7c1  1042             rca7d1  1043             rca7e1  1044          
   rca7f1  1045             rca7g1  1046             rca7h1  1047          
   rca8a  1048              rca10 DATE11.          ;                       

LABEL
   id = "longscan subject id"
   visit = "visit number"
   center = "field center"
   rca7a2 = "describe why sna not valid: line #1"
   rca7a3 = "describe why sna not valid: line #2"
   rca7b2 = "describe why ffa not valid: line #1"
   rca7b3 = "describe why ffa not valid: line #2"
   rca7c2 = "describe why ala not valid"
   rca7d2 = "describe why wva not valid: line #1"
   rca7d3 = "describe why wva not valid: line #2"
   rca7e2 = "describe why sxa not valid: line #1"
   rca7e3 = "describe why sxa not valid: line #2"
   rca7f2 = "describe why tsc not valid: line #1"
   rca7f3 = "describe why tsc not valid: line #2"
   rca7g2 = "describe why bia not valid: line #1"
   rca7g3 = "describe why bia not valid: line #2"
   rca7h2 = "describe why tva not valid"
   rca8a1 = "notable info regarding interview line 1"
   rca8a2 = "notable info regarding interview line 2"
   rca8a3 = "notable info regarding interview line 3"
   rca8a4 = "notable info regarding interview line 4"
   rca9 = "interviewer initials"
   rca1 = "rate neatness of child"
   rca2 = "rate restedness of child"
   rca3 = "rate attractiveness of child"
   rca4 = "did child understand directions"
   rca5 = "did child attend to tasks/questions"
   rca6 = "rate privacy of interview"
   rca7a1 = "sna valid?"
   rca7b1 = "ffa valid?"
   rca7c1 = "ala valid?"
   rca7d1 = "wva valid?"
   rca7e1 = "sxa valid?"
   rca7f1 = "tsc valid?"
   rca7g1 = "bia valid?"
   rca7h1 = "tva valid?"
   rca8a = "any notable info regarding this intervw?"
   rca10 = "date of interview" ;

FORMAT
   rca10 DATE9. ;
      

LENGTH
   visit 3                  rca1 3                   rca2 3                
   rca3 3                   rca4 3                   rca5 3                
   rca6 3                   rca7a1 3                 rca7b1 3              
   rca7c1 3                 rca7d1 3                 rca7e1 3              
   rca7f1 3                 rca7g1 3                 rca7h1 3              
   rca8a 3 ;

         

RUN ;
