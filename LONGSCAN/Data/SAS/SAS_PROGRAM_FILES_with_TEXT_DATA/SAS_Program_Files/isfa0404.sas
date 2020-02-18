/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\isfa0404.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = isfa0404 ;

DATA library.&dataset ;
INFILE datafile LRECL=325;
INPUT
   id $ 1-7                 center $ 8-9             visit  10             
   isfa1  11                isfa2  12-13             isfa2a $ 14-33        
   isfa3 $ 34-63            isfa4a  64               isfa4b  65            
   isfa4c  66               isfa4d  67               isfa5  68             
   isfa6  69-70             isfa6a $ 71-89           isfa7 $ 90-119        
   isfa8a  120              isfa8b  121              isfa8c  122           
   isfa8d  123              isfa9  124               isfa10  125-126       
   isfa10a $ 127-146        isfa11 $ 147-176         isfa12a  177          
   isfa12b  178             isfa12c  179             isfa12d  180          
   isfa13  181              isfa14 $ 182-211         isfa15a  212          
   isfa15b  213             isfa15c  214             isfa15d  215          
   isfa16  216              isfa17 $ 217-246         isfa18a  247          
   isfa18b  248             isfa18c  249             isfa18d  250          
   isfa19  251              isfa20a $ 252-271        isfa20b $ 272-291     
   isfa20c $ 292-311        isfa21 $ 312-314         isfa22 DATE11.        
 ;                       

LABEL
   id = "longscan subject id"
   center = "field center"
   visit = "visit number"
   isfa1 = "special-helpful adult in life"
   isfa2 = "most helpfl adult's relationshp to child"
   isfa2a = "identity of most helpful adult"
   isfa3 = "how most helpful has helped"
   isfa4a = "most helpful shows caring"
   isfa4b = "most helpful explains things"
   isfa4c = "most helpful spends time with child"
   isfa4d = "most helpful helps get necessities"
   isfa5 = "#2 helpful adult"
   isfa6 = "relationship of adult #2 to child"
   isfa6a = "identity of helpful adult #2"
   isfa7 = "how adult #2 is helpful"
   isfa8a = "#2 adult shows caring"
   isfa8b = "#2 adult explains things"
   isfa8c = "#2 adult spends time with child"
   isfa8d = "#2 adult helps get necessities"
   isfa9 = "third helpful adult"
   isfa10 = "relationship of adult #3 to child"
   isfa10a = "identity of third helpful adult"
   isfa11 = "how 3rd helpful adult is helpful"
   isfa12a = "3rd helpful adult shows caring"
   isfa12b = "3rd helpful adult explains-helps probs."
   isfa12c = "3rd helpful adult spends time with child"
   isfa12d = "3rd helpful adult helps get necessities"
   isfa13 = "child's mothering situation"
   isfa14 = "how mother is helpful"
   isfa15a = "mother shows caring"
   isfa15b = "mother explains things"
   isfa15c = "mother spends time with child"
   isfa15d = "mother helps get necessities"
   isfa16 = "child's fathering situation"
   isfa17 = "how father is helpful"
   isfa18a = "father shows caring"
   isfa18b = "father explains things"
   isfa18c = "father spends time with child"
   isfa18d = "father helps get necessities"
   isfa19 = "any other helpful adults"
   isfa20a = "identity of any other adult #1"
   isfa20b = "identity of any other adult #2"
   isfa20c = "identity of any other adult #3"
   isfa21 = "interviewer initials"
   isfa22 = "date of interview" ;

FORMAT
   isfa22 DATE9. ;
     

LENGTH
   visit 3                  isfa1 3                  isfa2 3               
   isfa4a 3                 isfa4b 3                 isfa4c 3              
   isfa4d 3                 isfa5 3                  isfa6 3               
   isfa8a 3                 isfa8b 3                 isfa8c 3              
   isfa8d 3                 isfa9 3                  isfa10 3              
   isfa12a 3                isfa12b 3                isfa12c 3             
   isfa12d 3                isfa13 3                 isfa15a 3             
   isfa15b 3                isfa15c 3                isfa15d 3             
   isfa16 3                 isfa18a 3                isfa18b 3             
   isfa18c 3                isfa18d 3                isfa19 3 ;

        

RUN ;
