/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\nria1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = nria1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=94;
INPUT
   CENTER $ 1-2             ID $ 3-9                 VISIT  10-11          
   NRIA1A  12               NRIA1B  13               NRIA1C  14            
   NRIA2A  15               NRIA2B  16               NRIA2C  17            
   NRIA3  18                NRIA3A  19               NRIA3B  20            
   NRIA3C  21               NRIA3D  22               NRIA4A  23            
   NRIA4B  24               NRIA4C  25               NRIA4D  26            
   NRIA4E  27               NRIA4F  28               NRIA5A  29            
   NRIA5B  30               NRIA5C  31               NRIA5D  32            
   NRIA5E  33               NRIA5F  34               NRIA6A  35            
   NRIA6B  36               NRIA6C  37               NRIA6D  38            
   NRIA6E  39               NRIA6F  40               NRIA7A  41            
   NRIA7B  42               NRIA7C  43               NRIA7D  44            
   NRIA7E  45               NRIA7F  46               NRIA8A  47            
   NRIA8B  48               NRIA8C  49               NRIA8D  50            
   NRIA8E  51               NRIA8F  52               NRIA9A  53            
   NRIA9B  54               NRIA9C  55               NRIA9D  56            
   NRIA9E  57               NRIA9F  58               NRIA10A  59           
   NRIA10B  60              NRIA10C  61              NRIA10D  62           
   NRIA10E  63              NRIA10F  64              NRIA11A  65           
   NRIA11B  66              NRIA11C  67              NRIA11D  68           
   NRIA11E  69              NRIA11F  70              NRIA12A  71           
   NRIA12B  72              NRIA12C  73              NRIA12D  74           
   NRIA12E  75              NRIA12F  76              NRIA13A  77           
   NRIA13B  78              NRIA13C  79              NRIA13D  80           
   NRIA13E  81              NRIA13F  82              NRIA14A  83           
   NRIA14B  84              NRIA14C  85              NRIA14D  86           
   NRIA14E  87              NRIA14F  88              NRIA15A  89           
   NRIA15B  90              NRIA15C  91              NRIA15D  92           
   NRIA15E  93              NRIA15F  94            ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   VISIT = "VISIT NUMBER"
   NRIA1A = "Female friend/how old"
   NRIA1B = "Female friend/how long"
   NRIA1C = "Female friend/go to same school"
   NRIA2A = "Male friend/how old"
   NRIA2B = "Male friend/how long"
   NRIA2C = "Male friend/go to same school"
   NRIA3 = "Do you have boy/grild friend"
   NRIA3A = "Is this person boy or girl"
   NRIA3B = "How old is this person"
   NRIA3C = "How long have you been friends"
   NRIA3D = "Do you go to the same school"
   NRIA4A = "G:time spend with female friend"
   NRIA4B = "B:time spend with male friend"
   NRIA4C = "G: time spend with male friend"
   NRIA4D = "Time spend with female friend"
   NRIA4E = "Time spend with boyfriend"
   NRIA4F = "Time spend with girlfriend"
   NRIA5A = "G: how upset/mad with female friend"
   NRIA5B = "B: how upset/mad with male friend"
   NRIA5C = "G: how upset with male friend/not br"
   NRIA5D = "B: how upset with female friend/not sr"
   NRIA5E = "How upset/mad with your boyfriend"
   NRIA5F = "How upset/mad with your girlfriend"
   NRIA6A = "G: how satisfied with female friend"
   NRIA6B = "B: how satisfied with male friend"
   NRIA6C = "G: how satisfied with male friend"
   NRIA6D = "B: how satisfied with female friend"
   NRIA6E = "How satisfied with your boyfriend"
   NRIA6F = "How satisfied with your girlfriend"
   NRIA7A = "G: How much do you tell female friend"
   NRIA7B = "B: How much do you tell male friend"
   NRIA7C = "G: How much do you tell male friend"
   NRIA7D = "B: How much do you tell female friend"
   NRIA7E = "How much do you tell your boyfriend"
   NRIA7F = "How much do you tell your girlfriend"
   NRIA8A = "G: how much play around female friend"
   NRIA8B = "B: how much play around male friend"
   NRIA8C = "G: how much play around male friend"
   NRIA8D = "B: how much play around female friend"
   NRIA8E = "How much play around wth your boyfriend"
   NRIA8F = "How much play around wth your girlfriend"
   NRIA9A = "G: how much disagree with female friend"
   NRIA9B = "B: how much disagree with male friend"
   NRIA9C = "G: how much disagree with male friend"
   NRIA9D = "B: how much disagree with female friend"
   NRIA9E = "How much disagree with your boyfriend"
   NRIA9F = "How much disagree with your girlfriend"
   NRIA10A = "G: how much share secrets/female friend"
   NRIA10B = "B: how much share secrets/male friend"
   NRIA10C = "G: how much share secrets/male friend"
   NRIA10D = "B: how much share secrets/female friend"
   NRIA10E = "How much share secrets/your boyfriend"
   NRIA10F = "How much share secrets/your girlfriend"
   NRIA11A = "G: how happy with female friend"
   NRIA11B = "B: how happy with male friend"
   NRIA11C = "G: how happy with male friend"
   NRIA11D = "B: how happy with female friend"
   NRIA11E = "How happy are you with your boyfriend"
   NRIA11F = "How happy are you with your girlfriend"
   NRIA12A = "G: how often go places wth female friend"
   NRIA12B = "B: how often go places with male friend"
   NRIA12C = "G: how often go places with male friend"
   NRIA12D = "B: how often go places wth female friend"
   NRIA12E = "How often go places/your boyfriend"
   NRIA12F = "How often go places/your girlfriend"
   NRIA13A = "G: how much argue with female friend"
   NRIA13B = "B: how much argue with male friend"
   NRIA13C = "G: how much argue with male friend"
   NRIA13D = "B: how much argue with female friend"
   NRIA13E = "How much argue with your boyfriend"
   NRIA13F = "How much argue with your girlfriend"
   NRIA14A = "G: how good relationship/female friend"
   NRIA14B = "B: how good relationship/male friend"
   NRIA14C = "G: how good relationship/ male friend"
   NRIA14D = "B: how good relationship/female friend"
   NRIA14E = "How good relationship/your boyfriend"
   NRIA14F = "How good relationship/your girlfriend"
   NRIA15A = "G: how much talk things/female friend"
   NRIA15B = "B: how much talk things/male friend"
   NRIA15C = "G: how much talk things/male friend"
   NRIA15D = "B: how much talk things/female friend"
   NRIA15E = "How much talk about things/boyfriend"
   NRIA15F = "How much talk things/girlfriend" ;

LENGTH
   VISIT 3                  NRIA1A 3                 NRIA1B 3              
   NRIA1C 3                 NRIA2A 3                 NRIA2B 3              
   NRIA2C 3                 NRIA3 3                  NRIA3A 3              
   NRIA3B 3                 NRIA3C 3                 NRIA3D 3              
   NRIA4A 3                 NRIA4B 3                 NRIA4C 3              
   NRIA4D 3                 NRIA4E 3                 NRIA4F 3              
   NRIA5A 3                 NRIA5B 3                 NRIA5C 3              
   NRIA5D 3                 NRIA5E 3                 NRIA5F 3              
   NRIA6A 3                 NRIA6B 3                 NRIA6C 3              
   NRIA6D 3                 NRIA6E 3                 NRIA6F 3              
   NRIA7A 3                 NRIA7B 3                 NRIA7C 3              
   NRIA7D 3                 NRIA7E 3                 NRIA7F 3              
   NRIA8A 3                 NRIA8B 3                 NRIA8C 3              
   NRIA8D 3                 NRIA8E 3                 NRIA8F 3              
   NRIA9A 3                 NRIA9B 3                 NRIA9C 3              
   NRIA9D 3                 NRIA9E 3                 NRIA9F 3              
   NRIA10A 3                NRIA10B 3                NRIA10C 3             
   NRIA10D 3                NRIA10E 3                NRIA10F 3             
   NRIA11A 3                NRIA11B 3                NRIA11C 3             
   NRIA11D 3                NRIA11E 3                NRIA11F 3             
   NRIA12A 3                NRIA12B 3                NRIA12C 3             
   NRIA12D 3                NRIA12E 3                NRIA12F 3             
   NRIA13A 3                NRIA13B 3                NRIA13C 3             
   NRIA13D 3                NRIA13E 3                NRIA13F 3             
   NRIA14A 3                NRIA14B 3                NRIA14C 3             
   NRIA14D 3                NRIA14E 3                NRIA14F 3             
   NRIA15A 3                NRIA15B 3                NRIA15C 3             
   NRIA15D 3                NRIA15E 3                NRIA15F 3 ;

       

RUN ;
