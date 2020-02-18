/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\sruc1201.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = sruc1201 ;

DATA library.&dataset ;
INFILE datafile LRECL=92;
INPUT
   CENTER $ 1-2             ID $ 3-9                 SRUC23 $ 10-12        
   VISIT  13-14             SRUC1A  15               SRUC1B  16            
   SRUC2A  17               SRUC2B  18               SRUC3A  19            
   SRUC3B  20               SRUC4A  21               SRUC4B  22            
   SRUC5A  23               SRUC5B  24               SRUC6A  25            
   SRUC6B  26               SRUC7A  27               SRUC7B  28            
   SRUC8A  29               SRUC8B  30               SRUC9A  31            
   SRUC9B  32               SRUC10A  33              SRUC10B  34           
   SRUC10B1  35             SRUC10B2  36             SRUC10C  37           
   SRUC10D  38              SRUC10E  39              SRUC10F  40           
   SRUC10G  41              SRUC10H  42              SRUC10I  43           
   SRUC10J  44              SRUC10K  45              SRUC10N1  46          
   SRUC10N2  47             SRUC10N3  48             SRUC10O  49           
   SRUC10O1  50             SRUC11A  51              SRUC11B  52           
   SRUC12A  53              SRUC12B  54              SRUC13A  55           
   SRUC13B  56              SRUC14A  57              SRUC14B  58           
   SRUC15A  59              SRUC15B  60              SRUC16A  61           
   SRUC16B  62              SRUC16D  63-64           SRUC17A  65           
   SRUC17B  66              SRUC17D  67-68           SRUC18A  69           
   SRUC18B  70              SRUC19A  71              SRUC19B  72           
   SRUC20A  73              SRUC20B  74              SRUC21A  75           
   SRUC21B  76              SRUC22A  77              SRUC22B  78           
   SRUC22C1  79             SRUC22C2  80-81          SRUC24 DATE11.        
 ;                       

LABEL
   CENTER = "FIELD CENTER"
   ID = "LONGSCAN SUBJECT ID"
   SRUC23 = "Interviewers Initials"
   VISIT = "VISIT NUMBER"
   SRUC1A = "Needed help finding job last year"
   SRUC1B = "Got help finding a job"
   SRUC2A = "Needed legal aid in last year"
   SRUC2B = "Get legal aid"
   SRUC3A = "Needed transportation help last yr"
   SRUC3B = "Get agency based help w/transportation"
   SRUC4A = "Needed ovrnite emergncy childcare"
   SRUC4B = "Get overnight emergency care for child"
   SRUC5A = "Needed parenting classes"
   SRUC5B = "Take parenting classes"
   SRUC6A = "Needed/wanted self-help/support group"
   SRUC6B = "Participated in self-help /support group"
   SRUC7A = "Needed battered women's shelter"
   SRUC7B = "Get help from battered women's shelter"
   SRUC8A = "Needed homeless shelter last year"
   SRUC8B = "Stay at homeless shelter"
   SRUC9A = "Needed alcohol/drug counsl lastyr"
   SRUC9B = "Got alcohol/drug counselng for yourself"
   SRUC10A = "Needed DSS services in last year"
   SRUC10B = "Got assistance from DSS"
   SRUC10B1 = "Lost DSS services during past year"
   SRUC10B2 = "DSS serv lost for time limit/oth rules"
   SRUC10C = "Help with ongoing financial assistance"
   SRUC10D = "Help with food or money for food"
   SRUC10E = "Help with health care or h.c.insurance"
   SRUC10F = "Help with rent, utilities, clothing"
   SRUC10G = "In-home services"
   SRUC10H = "Help with care or activities for child"
   SRUC10I = "Emergency care for children"
   SRUC10J = "Job training"
   SRUC10K = "Other services"
   SRUC10N1 = "Went to school in order to get services"
   SRUC10N2 = "Worked in order to get services"
   SRUC10N3 = "Volunteered in order to get services"
   SRUC10O = "Lost welfare services during the past yr"
   SRUC10O1 = "Services lost for time limits/othr rules"
   SRUC11A = "Needed out-of-home placement for child"
   SRUC11B = "Child placed out of home by agency"
   SRUC12A = "Needed out-home placement for othr child"
   SRUC12B = "Another child placed out home by agency"
   SRUC13A = "Hlth care chkup/immunz for child needed"
   SRUC13B = "Child got hlth care checkup/immunizn"
   SRUC14A = "Child needed dental care last year"
   SRUC14B = "Child got dental care"
   SRUC15A = "Needed help w learning/develpmnt prob"
   SRUC15B = "Got help for learning/develpmntl problm"
   SRUC16A = "Child needed counseling or therapy"
   SRUC16B = "Child got counseling or therapy"
   SRUC16D = "# of child visits to counselor/therapist"
   SRUC17A = "Felt you needed counsel/therapy last yr"
   SRUC17B = "Got counseling or therapy for yourself"
   SRUC17D = "# of respondent visits to therapist"
   SRUC18A = "Other fam membrs needed therapy"
   SRUC18B = "Other fam membrs got counsel/therapy"
   SRUC19A = "Child needed mentor-type program"
   SRUC19B = "Child got help from mentor-type program"
   SRUC20A = "Child needed med care:serious hlth prob"
   SRUC20B = "Child got med care:serious hlth prob"
   SRUC21A = "Chld needed med care for accident/injury"
   SRUC21B = "Child got accidnt/injury med care"
   SRUC22A = "Child needed hosp overnight psychol"
   SRUC22B = "Child was in hospital overnight psychol"
   SRUC22C1 = "# times child hospitalizd for psych prob"
   SRUC22C2 = "# days hospitalized for psych prob"
   SRUC24 = "Date of Interview" ;

FORMAT
   SRUC24 DATE9. ;
     

LENGTH
   VISIT 3                  SRUC1A 3                 SRUC1B 3              
   SRUC2A 3                 SRUC2B 3                 SRUC3A 3              
   SRUC3B 3                 SRUC4A 3                 SRUC4B 3              
   SRUC5A 3                 SRUC5B 3                 SRUC6A 3              
   SRUC6B 3                 SRUC7A 3                 SRUC7B 3              
   SRUC8A 3                 SRUC8B 3                 SRUC9A 3              
   SRUC9B 3                 SRUC10A 3                SRUC10B 3             
   SRUC10B1 3               SRUC10B2 3               SRUC10C 3             
   SRUC10D 3                SRUC10E 3                SRUC10F 3             
   SRUC10G 3                SRUC10H 3                SRUC10I 3             
   SRUC10J 3                SRUC10K 3                SRUC10N1 3            
   SRUC10N2 3               SRUC10N3 3               SRUC10O 3             
   SRUC10O1 3               SRUC11A 3                SRUC11B 3             
   SRUC12A 3                SRUC12B 3                SRUC13A 3             
   SRUC13B 3                SRUC14A 3                SRUC14B 3             
   SRUC15A 3                SRUC15B 3                SRUC16A 3             
   SRUC16B 3                SRUC16D 3                SRUC17A 3             
   SRUC17B 3                SRUC17D 3                SRUC18A 3             
   SRUC18B 3                SRUC19A 3                SRUC19B 3             
   SRUC20A 3                SRUC20B 3                SRUC21A 3             
   SRUC21B 3                SRUC22A 3                SRUC22B 3             
   SRUC22C1 3               SRUC22C2 3 ;

      

RUN ;
