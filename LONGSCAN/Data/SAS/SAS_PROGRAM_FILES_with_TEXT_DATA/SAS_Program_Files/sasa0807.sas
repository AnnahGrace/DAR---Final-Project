/* This SAS program was automatically generated by Stat/Transfer ver.12.0.63.0712 */

/* ENCODING CP1252 */

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile 'c:\temp\LONGSCAN_SRI_2014\sasa0807.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library 'c:\temp\longscan_sri_2014\' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = sasa0807 ;

DATA library.&dataset ;
INFILE datafile LRECL=231;
INPUT
   ID $ 1-7                 CENTER $ 8-9             VISIT  10-11          
   SASA1A  12               SASA1B  13               SASA1C  14            
   SASA1D  15               SASA1E  16               SASA1F  17            
   SASA1G  18               SASA1H  19               SASA1I  20            
   SASA1J  21               SASA1K  22               SASA2A  23            
   SASA2B  24               SASA2C  25               SASA2D  26            
   SASA2E  27               SASA2F  28               SASA2G  29            
   SASA2H  30               SASA2I  31               SASA2J  32            
   SASA2K  33               SASA3A  34               SASA3B  35            
   SASA3C  36               SASA3D  37               SASA3E  38            
   SASA3F  39               SASA3G  40               SASA3H  41            
   SASA3I  42               SASA3J  43               SASA3K  44            
   SASA4A  45               SASA4B  46               SASA4C  47            
   SASA4D  48               SASA4E  49               SASA4F  50            
   SASA4G  51               SASA4H  52               SASA4I  53            
   SASA4J  54               SASA4K  55               SASA5A  56            
   SASA5B  57               SASA5C  58               SASA5D  59            
   SASA5E  60               SASA5F  61               SASA5G  62            
   SASA5H  63               SASA5I  64               SASA5J  65            
   SASA5K  66               SASA6A  67               SASA6B  68            
   SASA6C  69               SASA6D  70               SASA6E  71            
   SASA6F  72               SASA6G  73               SASA6H  74            
   SASA6I  75               SASA6J  76               SASA6K  77            
   SASA7A  78               SASA7B  79               SASA7C  80            
   SASA7D  81               SASA7E  82               SASA7F  83            
   SASA7G  84               SASA7H  85               SASA7I  86            
   SASA7J  87               SASA7K  88               SASA8A  89            
   SASA8B  90               SASA8C  91               SASA8D  92            
   SASA8E  93               SASA8F  94               SASA8G  95            
   SASA8H  96               SASA8I  97               SASA8J  98            
   SASA8K  99               SASA9A  100              SASA9B  101           
   SASA9C  102              SASA9D  103              SASA9E  104           
   SASA9F  105              SASA9G  106              SASA9H  107           
   SASA9I  108              SASA9J  109              SASA9K  110           
   SASA10A  111             SASA10B  112             SASA10C  113          
   SASA10D  114             SASA10E  115             SASA10F  116          
   SASA10G  117             SASA10H  118             SASA10I  119          
   SASA10J  120             SASA10K  121             SASA11A  122          
   SASA11B  123             SASA11C  124             SASA11D  125          
   SASA11E  126             SASA11F  127             SASA11G  128          
   SASA11H  129             SASA11I  130             SASA11J  131          
   SASA11K  132             SASA12A  133             SASA12B  134          
   SASA12C  135             SASA12D  136             SASA12E  137          
   SASA12F  138             SASA12G  139             SASA12H  140          
   SASA12I  141             SASA12J  142             SASA12K  143          
   SASA13A  144             SASA13B  145             SASA13C  146          
   SASA13D  147             SASA13E  148             SASA13F  149          
   SASA13G  150             SASA13H  151             SASA13I  152          
   SASA13J  153             SASA13K  154             SASA14A  155          
   SASA14B  156             SASA14C  157             SASA14D  158          
   SASA14E  159             SASA14F  160             SASA14G  161          
   SASA14H  162             SASA14I  163             SASA14J  164          
   SASA14K  165             SASA15A  166             SASA15B  167          
   SASA15C  168             SASA15D  169             SASA15E  170          
   SASA15F  171             SASA15G  172             SASA15H  173          
   SASA15I  174             SASA15J  175             SASA15K  176          
   SASA16A  177             SASA16B  178             SASA16C  179          
   SASA16D  180             SASA16E  181             SASA16F  182          
   SASA16G  183             SASA16H  184             SASA16I  185          
   SASA16J  186             SASA16K  187             SASA17A  188          
   SASA17B  189             SASA17C  190             SASA17D  191          
   SASA17E  192             SASA17F  193             SASA17G  194          
   SASA17H  195             SASA17I  196             SASA17J  197          
   SASA17K  198             SASA18A  199             SASA18B  200          
   SASA18C  201             SASA18D  202             SASA18E  203          
   SASA18F  204             SASA18G  205             SASA18H  206          
   SASA18I  207             SASA18J  208             SASA18K  209          
   SASA19A  210             SASA19B  211             SASA19C  212          
   SASA19D  213             SASA19E  214             SASA19F  215          
   SASA19G  216             SASA19H  217             SASA19I  218          
   SASA19J  219             SASA19K  220             SASA20A  221          
   SASA20B  222             SASA20C  223             SASA20D  224          
   SASA20E  225             SASA20F  226             SASA20G  227          
   SASA20H  228             SASA20I  229             SASA20J  230          
   SASA20K  231           ;                       

LABEL
   ID = "LONGSCAN SUBJECT ID"
   CENTER = "FIELD CENTER"
   VISIT = "VISIT NUMBER"
   SASA1A = "Mother:Think about sexual things with"
   SASA1B = "Mother:How much feel at fault"
   SASA1C = "Mother:Told anyone about"
   SASA1D = "Mother:Talked w/other parent"
   SASA1E = "Mother:Talked w/adult frnd/rel"
   SASA1F = "Mother:Talked w/adult at school"
   SASA1G = "Mother:Talked w/professional"
   SASA1H = "Mother:Talked w/another adult"
   SASA1I = "Mother:Talked w/another child"
   SASA1J = "Mother:When told other parent"
   SASA1K = "Mother:Glad you told parent"
   SASA2A = "Stepmother:Think about sex.things with"
   SASA2B = "Stepmother:How much feel at fault"
   SASA2C = "Stepmother:Told anyone about"
   SASA2D = "Stepmother:Talked w/other parent"
   SASA2E = "Stepmother:Talked w/adult frnd/rel"
   SASA2F = "Stepmother:Talked w/adult at school"
   SASA2G = "Stepmother:Talked w/professional"
   SASA2H = "Stepmother:Talked w/another adult"
   SASA2I = "Stepmother:Talked w/another child"
   SASA2J = "Stepmother:When told other parent"
   SASA2K = "Stepmother:Glad you told parent"
   SASA3A = "Foster mother:Think about sex.things/w"
   SASA3B = "Foster mother:How much feel at fault"
   SASA3C = "Foster mother:Told anyone about"
   SASA3D = "Foster mother:Talked w/other parent"
   SASA3E = "Foster mother:Talked w/adult frnd/rel"
   SASA3F = "Foster mother:Talked w/adult at school"
   SASA3G = "Foster mother:Talked w/professional"
   SASA3H = "Foster mother:Talked w/another adult"
   SASA3I = "Foster mother:Talked w/another child"
   SASA3J = "Foster mother:When told other parent"
   SASA3K = "Foster mother:Glad you told parent"
   SASA4A = "Fathers grlfrnd:Think about sex.things/w"
   SASA4B = "Fathers grlfrnd:How much feel at fault"
   SASA4C = "Fathers grlfrnd:Told anyone about"
   SASA4D = "Fathers grlfrnd:Talked w/other parent"
   SASA4E = "Fathers grlfrnd:Talked w/adult frnd/rel"
   SASA4F = "Fathers grlfrnd:Talked w/adult at school"
   SASA4G = "Fathers grlfrnd:Talked w/professional"
   SASA4H = "Fathers grlfrnd:Talked w/another adult"
   SASA4I = "Fathers grlfrnd:Talked w/another child"
   SASA4J = "Fathers grlfrnd:When told other parent"
   SASA4K = "Fathers grlfrnd:Glad you told parent"
   SASA5A = "Sister:Think about sexual things with"
   SASA5B = "Sister:How much feel at fault"
   SASA5C = "Sister:Told anyone about"
   SASA5D = "Sister:Talked w/other parent"
   SASA5E = "Sister:Talked w/adult frnd/rel"
   SASA5F = "Sister:Talked w/adult at school"
   SASA5G = "Sister:Talked w/professional"
   SASA5H = "Sister:Talked w/another adult"
   SASA5I = "Sister:Talked w/another child"
   SASA5J = "Sister:When told other parent"
   SASA5K = "Sister:Glad you told parent"
   SASA6A = "F.adult rel.:Think about sex.things with"
   SASA6B = "F.adult rel.:How much feel at fault"
   SASA6C = "F.adult rel.:Told anyone about"
   SASA6D = "F.adult rel.:Talked w/other parent"
   SASA6E = "F.adult rel.:Talked w/adult frnd/rel"
   SASA6F = "F.adult rel.:Talked w/adult at school"
   SASA6G = "F.adult rel.:Talked w/professional"
   SASA6H = "F.adult rel.:Talked w/another adult"
   SASA6I = "F.adult rel.:Talked w/another child"
   SASA6J = "F.adult rel.:When told other parent"
   SASA6K = "F.adult rel.:Glad you told parent"
   SASA7A = "Other f.adult:Think about sex. things/w"
   SASA7B = "Other f.adult:How much feel at fault"
   SASA7C = "Other f.adult:Told anyone about"
   SASA7D = "Other f.adult:Talked w/other parent"
   SASA7E = "Other f.adult:Talked w/adult frnd/rel"
   SASA7F = "Other f.adult:Talked w/adult at school"
   SASA7G = "Other f.adult:Talked w/professional"
   SASA7H = "Other f.adult:Talked w/another adult"
   SASA7I = "Other f.adult:Talked w/another child"
   SASA7J = "Other f.adult:When told other parent"
   SASA7K = "Other f.adult:Glad you told parent"
   SASA8A = "F. stranger:Think about sex.things with"
   SASA8B = "F. stranger:How much feel at fault"
   SASA8C = "F. stranger:Told anyone about"
   SASA8D = "F. stranger:Talked w/other parent"
   SASA8E = "F. stranger:Talked w/adult frnd/rel"
   SASA8F = "F. stranger:Talked w/adult at school"
   SASA8G = "F. stranger:Talked w/professional"
   SASA8H = "F. stranger:Talked w/another adult"
   SASA8I = "F. stranger:Talked w/another child"
   SASA8J = "F. stranger:When told other parent"
   SASA8K = "F. stranger:Glad you told parent"
   SASA9A = "Older girl:Think about sex.things with"
   SASA9B = "Older girl:How much feel at fault"
   SASA9C = "Older girl:Told anyone about"
   SASA9D = "Older girl:Talked w/other parent"
   SASA9E = "Older girl:Talked w/adult frnd/rel"
   SASA9F = "Older girl:Talked w/adult at school"
   SASA9G = "Older girl:Talked w/professional"
   SASA9H = "Older girl:Talked w/another adult"
   SASA9I = "Older girl:Talked w/another child"
   SASA9J = "Older girl:When told other parent"
   SASA9K = "Older girl:Glad you told parent"
   SASA10A = "Girl same age:Think about sex.things/w"
   SASA10B = "Girl same age:How much feel at fault"
   SASA10C = "Girl same age:Told anyone about"
   SASA10D = "Girl same age:Talked w/other parent"
   SASA10E = "Girl same age:Talked w/adult frnd/rel"
   SASA10F = "Girl same age:Talked w/adult at school"
   SASA10G = "Girl same age:Talked w/professional"
   SASA10H = "Girl same age:Talked w/another adult"
   SASA10I = "Girl same age:Talked w/another child"
   SASA10J = "Girl same age:When told other parent"
   SASA10K = "Girl same age:Glad you told parent"
   SASA11A = "Father:Think about sexual things with"
   SASA11B = "Father:How much feel at fault"
   SASA11C = "Father:Told anyone about"
   SASA11D = "Father:Talked w/other parent"
   SASA11E = "Father:Talked w/adult frnd/rel"
   SASA11F = "Father:Talked w/adult at school"
   SASA11G = "Father:Talked w/professional"
   SASA11H = "Father:Talked w/another adult"
   SASA11I = "Father:Talked w/another child"
   SASA11J = "Father:When told other parent"
   SASA11K = "Father:Glad you told parent"
   SASA12A = "Stepfather:Think about sex.things with"
   SASA12B = "Stepfather:How much feel at fault"
   SASA12C = "Stepfather:Told anyone about"
   SASA12D = "Stepfather:Talked w/other parent"
   SASA12E = "Stepfather:Talked w/adult frnd/rel"
   SASA12F = "Stepfather:Talked w/adult at school"
   SASA12G = "Stepfather:Talked w/professional"
   SASA12H = "Stepfather:Talked w/another adult"
   SASA12I = "Stepfather:Talked w/another child"
   SASA12J = "Stepfather:When told other parent"
   SASA12K = "Stepfather:Glad you told parent"
   SASA13A = "Foster father:Think about sex.things/w"
   SASA13B = "Foster father:How much feel at fault"
   SASA13C = "Foster father:Told anyone about"
   SASA13D = "Foster father:Talked w/other parent"
   SASA13E = "Foster father:Talked w/adult frnd/rel"
   SASA13F = "Foster father:Talked w/adult at school"
   SASA13G = "Foster father:Talked w/professional"
   SASA13H = "Foster father:Talked w/another adult"
   SASA13I = "Foster father:Talked w/another child"
   SASA13J = "Foster father:When told other parent"
   SASA13K = "Foster father:Glad you told parent"
   SASA14A = "Mothers boyfrnd:Think about sex.things/w"
   SASA14B = "Mothers boyfrnd:How much feel at fault"
   SASA14C = "Mothers boyfrnd:Told anyone about"
   SASA14D = "Mothers boyfrnd:Talked w/other parent"
   SASA14E = "Mothers boyfrnd:Talked w/adult frnd/rel"
   SASA14F = "Mothers boyfrnd:Talked w/adult at school"
   SASA14G = "Mothers boyfrnd:Talked w/professional"
   SASA14H = "Mothers boyfrnd:Talked w/another adult"
   SASA14I = "Mothers boyfrnd:Talked w/another child"
   SASA14J = "Mothers boyfrnd:When told other parent"
   SASA14K = "Mothers boyfrnd:Glad you told parent"
   SASA15A = "Brother:Think about sexual things with"
   SASA15B = "Brother:How much feel at fault"
   SASA15C = "Brother:Told anyone about"
   SASA15D = "Brother:Talked w/other parent"
   SASA15E = "Brother:Talked w/adult frnd/rel"
   SASA15F = "Brother:Talked w/adult at school"
   SASA15G = "Brother:Talked w/professional"
   SASA15H = "Brother:Talked w/another adult"
   SASA15I = "Brother:Talked w/another child"
   SASA15J = "Brother:When told other parent"
   SASA15K = "Brother:Glad you told parent"
   SASA16A = "M.adult rel.:Think about sex.things with"
   SASA16B = "M.adult rel.:How much feel at fault"
   SASA16C = "M.adult rel.:Told anyone about"
   SASA16D = "M.adult rel.:Talked w/other parent"
   SASA16E = "M.adult rel.:Talked w/adult frnd/rel"
   SASA16F = "M.adult rel.:Talked w/adult at school"
   SASA16G = "M.adult rel.:Talked w/professional"
   SASA16H = "M.adult rel.:Talked w/another adult"
   SASA16I = "M.adult rel.:Talked w/another child"
   SASA16J = "M.adult rel.:When told other parent"
   SASA16K = "M.adult rel.:Glad you told parent"
   SASA17A = "Other m.adult:Think about sex.things/w"
   SASA17B = "Other m.adult:How much feel at fault"
   SASA17C = "Other m.adult:Told anyone about"
   SASA17D = "Other m.adult:Talked w/other parent"
   SASA17E = "Other m.adult:Talked w/adult frnd/rel"
   SASA17F = "Other m.adult:Talked w/adult at school"
   SASA17G = "Other m.adult:Talked w/professional"
   SASA17H = "Other m.adult:Talked w/another adult"
   SASA17I = "Other m.adult:Talked w/another child"
   SASA17J = "Other m.adult:When told other parent"
   SASA17K = "Other m.adult:Glad you told parent"
   SASA18A = "M.adult strangr:Think about sex.things/w"
   SASA18B = "M.adult strangr:How much feel at fault"
   SASA18C = "M.adult strangr:Told anyone about"
   SASA18D = "M.adult strangr:Talked w/other parent"
   SASA18E = "M.adult strangr:Talked w/adult frnd/rel"
   SASA18F = "M.adult strangr:Talked w/adult at school"
   SASA18G = "M.adult strangr:Talked w/professional"
   SASA18H = "M.adult strangr:Talked w/another adult"
   SASA18I = "M.adult strangr:Talked w/another child"
   SASA18J = "M.adult strangr:When told other parent"
   SASA18K = "M.adult strangr:Glad you told parent"
   SASA19A = "Older boy:Think about sexual things with"
   SASA19B = "Older boy:How much feel at fault"
   SASA19C = "Older boy:Told anyone about"
   SASA19D = "Older boy:Talked w/other parent"
   SASA19E = "Older boy:Talked w/adult frnd/rel"
   SASA19F = "Older boy:Talked w/adult at school"
   SASA19G = "Older boy:Talked w/professional"
   SASA19H = "Older boy:Talked w/another adult"
   SASA19I = "Older boy:Talked w/another child"
   SASA19J = "Older boy:When told other parent"
   SASA19K = "Older boy:Glad you told parent"
   SASA20A = "Boy same age:Think about sex.things with"
   SASA20B = "Boy same age:How much feel at fault"
   SASA20C = "Boy same age:Told anyone about"
   SASA20D = "Boy same age:Talked w/other parent"
   SASA20E = "Boy same age:Talked w/adult frnd/rel"
   SASA20F = "Boy same age:Talked w/adult at school"
   SASA20G = "Boy same age:Talked w/professional"
   SASA20H = "Boy same age:Talked w/another adult"
   SASA20I = "Boy same age:Talked w/another child"
   SASA20J = "Boy same age:When told other parent"
   SASA20K = "Boy same age:Glad you told parent" ;

LENGTH
   VISIT 3                  SASA1A 3                 SASA1B 3              
   SASA1C 3                 SASA1D 3                 SASA1E 3              
   SASA1F 3                 SASA1G 3                 SASA1H 3              
   SASA1I 3                 SASA1J 3                 SASA1K 3              
   SASA2A 3                 SASA2B 3                 SASA2C 3              
   SASA2D 3                 SASA2E 3                 SASA2F 3              
   SASA2G 3                 SASA2H 3                 SASA2I 3              
   SASA2J 3                 SASA2K 3                 SASA3A 3              
   SASA3B 3                 SASA3C 3                 SASA3D 3              
   SASA3E 3                 SASA3F 3                 SASA3G 3              
   SASA3H 3                 SASA3I 3                 SASA3J 3              
   SASA3K 3                 SASA4A 3                 SASA4B 3              
   SASA4C 3                 SASA4D 3                 SASA4E 3              
   SASA4F 3                 SASA4G 3                 SASA4H 3              
   SASA4I 3                 SASA4J 3                 SASA4K 3              
   SASA5A 3                 SASA5B 3                 SASA5C 3              
   SASA5D 3                 SASA5E 3                 SASA5F 3              
   SASA5G 3                 SASA5H 3                 SASA5I 3              
   SASA5J 3                 SASA5K 3                 SASA6A 3              
   SASA6B 3                 SASA6C 3                 SASA6D 3              
   SASA6E 3                 SASA6F 3                 SASA6G 3              
   SASA6H 3                 SASA6I 3                 SASA6J 3              
   SASA6K 3                 SASA7A 3                 SASA7B 3              
   SASA7C 3                 SASA7D 3                 SASA7E 3              
   SASA7F 3                 SASA7G 3                 SASA7H 3              
   SASA7I 3                 SASA7J 3                 SASA7K 3              
   SASA8A 3                 SASA8B 3                 SASA8C 3              
   SASA8D 3                 SASA8E 3                 SASA8F 3              
   SASA8G 3                 SASA8H 3                 SASA8I 3              
   SASA8J 3                 SASA8K 3                 SASA9A 3              
   SASA9B 3                 SASA9C 3                 SASA9D 3              
   SASA9E 3                 SASA9F 3                 SASA9G 3              
   SASA9H 3                 SASA9I 3                 SASA9J 3              
   SASA9K 3                 SASA10A 3                SASA10B 3             
   SASA10C 3                SASA10D 3                SASA10E 3             
   SASA10F 3                SASA10G 3                SASA10H 3             
   SASA10I 3                SASA10J 3                SASA10K 3             
   SASA11A 3                SASA11B 3                SASA11C 3             
   SASA11D 3                SASA11E 3                SASA11F 3             
   SASA11G 3                SASA11H 3                SASA11I 3             
   SASA11J 3                SASA11K 3                SASA12A 3             
   SASA12B 3                SASA12C 3                SASA12D 3             
   SASA12E 3                SASA12F 3                SASA12G 3             
   SASA12H 3                SASA12I 3                SASA12J 3             
   SASA12K 3                SASA13A 3                SASA13B 3             
   SASA13C 3                SASA13D 3                SASA13E 3             
   SASA13F 3                SASA13G 3                SASA13H 3             
   SASA13I 3                SASA13J 3                SASA13K 3             
   SASA14A 3                SASA14B 3                SASA14C 3             
   SASA14D 3                SASA14E 3                SASA14F 3             
   SASA14G 3                SASA14H 3                SASA14I 3             
   SASA14J 3                SASA14K 3                SASA15A 3             
   SASA15B 3                SASA15C 3                SASA15D 3             
   SASA15E 3                SASA15F 3                SASA15G 3             
   SASA15H 3                SASA15I 3                SASA15J 3             
   SASA15K 3                SASA16A 3                SASA16B 3             
   SASA16C 3                SASA16D 3                SASA16E 3             
   SASA16F 3                SASA16G 3                SASA16H 3             
   SASA16I 3                SASA16J 3                SASA16K 3             
   SASA17A 3                SASA17B 3                SASA17C 3             
   SASA17D 3                SASA17E 3                SASA17F 3             
   SASA17G 3                SASA17H 3                SASA17I 3             
   SASA17J 3                SASA17K 3                SASA18A 3             
   SASA18B 3                SASA18C 3                SASA18D 3             
   SASA18E 3                SASA18F 3                SASA18G 3             
   SASA18H 3                SASA18I 3                SASA18J 3             
   SASA18K 3                SASA19A 3                SASA19B 3             
   SASA19C 3                SASA19D 3                SASA19E 3             
   SASA19F 3                SASA19G 3                SASA19H 3             
   SASA19I 3                SASA19J 3                SASA19K 3             
   SASA20A 3                SASA20B 3                SASA20C 3             
   SASA20D 3                SASA20E 3                SASA20F 3             
   SASA20G 3                SASA20H 3                SASA20I 3             
   SASA20J 3                SASA20K 3 ;

       

RUN ;
