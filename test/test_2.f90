! A small performance test, to see how the library performs on a few MBs of text.
module regex_test_2
    use iso_fortran_env
    use regex_module

    implicit none
    private

    public :: read_test2
    public :: run_test2

    character(len=*), parameter :: test2data =  &
        "yfufcoubtwfoirqxficrlajyhmgfguiebmrcgtdbgdlfxhuoskhhdhqfyjjgpnownhyevasnwqtdwvtm" // &
        "nvyqyyjctuotmfgdwauyryisoigejkficydqhlgflnydgvdjsdjaymqrhghgfdcwosgmctwrkoochofg" // &
        "sfouwbqkwrhmfqxujtulibdaxxfqamvvoehxpyehvbsodvcdtsnjjarbfvcqmtgujkjnursmeiybmgcc" // &
        "llgnkvnayjbmpighptkptnbonnavhxwowvmmuksnyiorjxgekqakruiekpirpvyvrvivgfarlqfytxln" // &
        "dhafovngcxauseiwguwbukpwuipbvsqpxqwxfnuevkymtahaxntwcynmwqofoeeravddgwixjugfcyem" // &
        "bhrnxmqlnjbbdulmaqgmhogqituxileynevaujeshcbmkmomliyinvtfoclnjuppbxqcmngnferbbtxw" // &
        "oifuvylsbnlpjvlxnfohelupbbxabkanaxqqsnlojgaeaktxpgsexytvotsocvcpfmjqbxhtqpiewewe" // &
        "xjrqneydksycdkxdfiwboqlslhxpqyryoklwcjhgiphnaqwapevlrqffngqlwclekrfcsapqdinqxlta" // &
        "weqadfhgoufisnvnfcftlkhbnlxespvxapfpygighxpvsckhqgflxwlrvlshhahllyjftwebnqkaqugb" // &
        "vbkqqkeituywledgpjqslsaiphsvtiwikkphxygybcthicwpkjtjfuusehcjdvdrywmtijfegwgsstju" // &
        "enracngtmmevveadpwxrwpoifqnluhhocombaprwlvkphnmxheloescietwfdthfqbgrjdhwmrkxrkrx" // &
        "peiercjiburkijxehimjurfvyfgtyivmwweksaslptovyjfdgmrffnhcdslclqmlcieksdkvhsqargjy" // &
        "xjoeqerfbmcsdwxwogpmofyytueunrvrjwxnngtmofojgelucxlgxyrakqvwmmivonfaabbyafmxtxio" // &
        "cyptuyccpmpfgeapyymjmwjubkidqgdcbhlhrjgmrsxrdewrrqjdpqvseumvokfskikfhydxkgvntpgi" // &
        "bdrjnrrksomllfxuffnkitlyiplrntnngobpebbjhfvjtjiqlvwkksodbgjkheaiwxtbpsphkurphkum" // &
        "btweqwmepfplihuubbuimhcnmkxpwawixijhdtnrlmrunffgosnoxrrlfpfijqngltvxtwsfsupggbuw" // &
        "gxvmjxiucjbnmfakshrxymihosmbhpxonccoxetjwxyjbxpieywmklkbnusncglccjqbaxlmidoqbobr" // &
        "kaewbuynsygvcpijlbpbawinbqnkrvvdwhfxwdkjtfgipejhqvqiudkqqagmgbikafbcvdfscxtwncha" // &
        "lqpiewmirknouxmtjhrkirkxrhehvmgqeoarocpgxsobkedkbvvwcjwxnycubuggbjjtojlupeaohkgn" // &
        "jliqcujrqmusxbxfyypnulfnkksqmwhmvulwoqlgpovprdtwcxisowojddwkyreotaqqmmhfcmsvovhl" // &
        "akvjisvrlcxtsxbtphbjpftfafrtogvgldsimijtgsxiurnavjisjwqgfnftqjodclgpwxdokivuflmm" // &
        "eueuwjrkwgeconmjmfxffvaotocuytajdbuywjhvgatdmduedlpuhlptpqjmaygviljckiwcwcarfvix" // &
        "rhrutcdjsypoieksqpkhahybueyheogpwxowreusjdlunevxsjimamjmqmqkdleagjldxddxuspvifnt" // &
        "rsyxrtkxqcbrumhcixmqhmdeupkscriuwvgrulvenpcofqebsdpcxtosuoxsqnwupvtuaphrqymriyit" // &
        "aruhrybemaqlwgsfginbltvpqdqypsclgcsohxsflkbxphblfgtwalqpjolunayhsayiqnmflhtrddju" // &
        "pliihrfuaaidfihcgmvqjnjxawgwiwqmkjrqrnaeoqodmyafkwvdgofnydwumvdixkroutqlwamiaura" // &
        "mxlqntlnqxwodvfbbukufusjvupshlwmuqxsdsxivdanbjyopjmunucfeujdkefdbfxgwutbganghkls" // &
        "miiunliswicdulkxixgsrjdxjrxoavqfvcwguesddjwhoaoxnmxpvkggumnbptfjphpltmbfisjpfiks" // &
        "bpoaccqopjudmthvanrqbpdfpqaihdaeyxkrdcmlgajgdspvtplfwqfrlgifphgnpigqsrmqtdkqspxp" // &
        "uvtnscmdnsyhknjwnbgmrhlvdosmbgihdergufhdagjuulrqutsxtmbyocobgspprnhyvwvnnnxeecda" // &
        "bxqrchqcsppaqvdgtqtgtwnysgypwokytygjnjpvbwtprcltedcxdmsiepmqnlbdglrekcwmcpremtmc" // &
        "xuhpscnwqaskunvtcprebuajvfusdgmwdjfkklfdirpvgtljrgtfjbfsywkbwjusaixkfgbpedvywlut" // &
        "wcsbcsoldenljrwkaxuwladcdjahnehfqksobnirycoyyupwuvcwvuphehclmbshphkpcpyfhprwenet" // &
        "bdtxmpyttonmxsmjtmuoyflkbhwguqnmkcjjxxphtrteowlrtlfxwxrsoxmfcacnisyrorbvuqpaxwvp" // &
        "xiweucqokjtacadrdbgryktpnkcyftlnxlpslifmqwyvqngxtkkdocwauljmnrfjvuqomqwupwvtpqkc" // &
        "ldinghaqjqueikcvykvbwvsxmvfoidrmlbqarllpdivnbbiudskvvgkxvpxphgrnkhosimqktoysexlg" // &
        "tsobasacbttlhpvwjjjowofhxgdsuimibkwcioaraoawjcipbyaafquruailxcxltxoxjvraihlgeyum" // &
        "vebgktjbtjcfblujxaoybdadwiwwuxwirigeseqmleshnhikrtvxbjxrwvxhttbjlsvsyqrweojncdse" // &
        "itrvhyggxwuxrrhdvsdadxoglnajkyqkbtqkvnnnntkvangesryfaeetnaftpieggvbalfgvevjbddkm" // &
        "ppctcgxsrmcxmpnuvtykaimssidfnvggbwththeduhpisogktyhlwshadhvabtqoykmacedtgusmejvi" // &
        "ucrlipspbwaveibpompwxdxbwneqcmytrxnktbohtqfnmsvvovsffbbhfnrrsjsricsyikfnlqfftxbn" // &
        "uatbhovluljigwakvvofbofbgbunrmqmosilpiijmnhpccihhesjgetywgmvuiqhblrfvevqxthblhhf" // &
        "dbmowhixeqhymgwkdjykhqehemtcrgfvlaxtlgbsuspfidarknfsmqvujajpxalcumyefayhtrkwtcwg" // &
        "askygcsmgyesdrqehfkdsvcpxpvxmfycjjluwdbgwdtvimrdsuqntfenvklppnhxuwebhovcrpqggpdf" // &
        "ghypcgohpohokqybvjunweporoafekscabfegwwyrrknmukehnjneeaxfhdbyfcrdtqjrxvdlfmukdmx" // &
        "htcilmeccipvmnpgmdmoimvuekosxpclsnwomypwcwieqmtmxxapeedeuqgcdtlhxemohrerhkethxfd" // &
        "iwyrwsqxqvgtytkwcmuvogbvgrrramstxeddgwgoxtvkuiuhbrgsousyjkpyvdkqdyhvjqcepxatreoh" // &
        "vujujbeabktdpmnttrkrqfrrukkjcslvwqnpeqarhyytxbjcrxmarsiaaxickdyufrcgwoweuojrajlf" // &
        "rnpihjbllpqnuivvfnwvfixthxdyfuthogyxsmxyfctuobbbhxldvdrneqfiwvbfqctaxbtrkgixcjnv" // &
        "baohjgrrytxgerjhmoaqerdaysjdvwjkqluulxgwvxitbtuqwwpilaqflxjgwhecwrylodxwhxchfpxo" // &
        "pxfyvcjeyfxcnkuaplbhyckmbwhvbohbxxrrltytkhnttpsgsqkoijqxjwpvrdjhnjiysxqwqhmkhycb" // &
        "ckgoihnmgejtgfsbgxmibsgvqsymjwegpndvfkpsnaiqqbuprcihnmenjsbpjigiovoptofoddvfkbsl" // &
        "jnskuofnxkdlmsnijoxbkxtegrffiicsuyuswrwwtthxvqyxalknavkmvcpynxrauldioxaqhlawiitu" // &
        "ofunblmgyoiqvqxgufmiguokcaocpcvuctpxdwnixlcpfmhdjudqkdgxscnutwvjbtsvgpoxocudaykp" // &
        "suedtpaowmtnxerucqifyetwomfjwoxvfgedfabaqydlykduylgmvgwqiefqkpbdsnmggoupeycbakbj" // &
        "fhguxhkwfyglojjsoscxjgtmohkbpfjnngletolghmgyajnpdohkjddojdfgsjvxlgfulyieqlkcamax" // &
        "jtjxeubkkuwxswdaakqoolgnmwdmdmdvwkivrpwbuishevpvvljiwsifgjtjpolwpjvjahxsprxbvkfk" // &
        "dqmbmpyjflyvfuiqfyxwuukwmggkuudubsljlditnecynmlwjimxbtijfunonxsmttblhkpallcdgehs" // &
        "tmdxhqafwfjfoaeootjijuhlskruiccbndvuyqdajeeaanfetwabhhgqihkskvhsbceesrjqnajcabsn" // &
        "npgocpdfbadjxpaoiyfwtibjsumvtmawggroyuarelgicwqabcaqmmdjjpcqrkjfbvlarouruekusdvk" // &
        "lmstjpadtqlcolhuipssvxkgstopraulffioukmdtykfkeelkalscfgvmtaenbarcmljugmbnpvkdwkh" // &
        "utwasblyywofcsgdcbfqsyfsirqprghrmmyhwohbdpdpnrifkrdecwaaksswxwldaibrrmnvpurcwieb" // &
        "sskppdgqrjoyuuduosxgyyxnvclsjlenlpivcjojjokfusfqauxhkgqrkegmhtbxeoitqycgywkianki" // &
        "pxturfodwbysysxebkokvbnhjraasoxahvclwlcnmlqoprcuhagiojnaqrkfsldyyuymnlgejvfvwlkw" // &
        "udrswidtrifkomvtddstioqyedqimbmplvarnedwjgrxtriboqqjipsyjppmoomyonjnugvxerwdioci" // &
        "shryljawkrxcrcyijrnybgorckmavnsdbjxtrtdykqrhgccvowvadepqraccfusbppghpcldmvmgsrjj" // &
        "xkdxqqfhykkigpnfssxrcvyecgydiquhylkaxjlvcapyqlwlofcjidooudvpecrpbqlsttkonyrdfmys" // &
        "mdgkimvecsljfddprgilccqcisxgwjmtcchxaskksisrsqghevwenhrjcmgnesxsjmtaastbjjbcecpi" // &
        "fqphjfgxacluyjckrxeewwsqhokdnfdnkxgsludmfkwstphflpjbkxneajhcjduiqwngrppvpnktuvkb" // &
        "ycxlcoihvobjafkruvgtxsiccruaduipchrgiombahbvnobvpjgktuupplvmqkorpgcyrdgomwdaohca" // &
        "dagbnejnwfnuyduyyacocywdstjqcakvcnsrubuughqonaeunjvqhtyhepenlbxnfdcwhuxnqmyqhhus" // &
        "lgsumtrmweggogfumcummtrodtovsugesxwgfwfisuaejakrljqqsnopenrjtorvaitgromtutrcexqk" // &
        "ysclousbppseymovdvgidjriuiynmxibqnkfusktnalivcfmiabstjydogwhkcuctmvhrnimtoyxbvbo" // &
        "plobexcerwdveftccjmkndsbfmbfkvchleirubndjjmutymgenkgjncccmkdonjuwqtesqbavgmktxlf" // &
        "etohasfcydjytggkxpoldhivkwgmbrqvivnvqlttquklpnixqmbmielwhhmmbputwoeixbxtkvrvexwt" // &
        "cuqfnlmbvkwpvykxvnmfmkikdywbeaglfopssfhqhpeetienehvyfvgfhhmsxmiycoddyhqusmcgudke" // &
        "ivpejcphafsshtqlcabsusxmfttsctdukljrradtqpkipuanqwsorwgggassancplihgbvqrrrreempm" // &
        "ssihtvqfggpfixbxrmxvyjoyveaaollynbcviiwvmfiyyouqduklmslqxnmtnqkfmgikwfimmmjtwocv" // &
        "mgeedvhghfhoacxmvdtnmifqbpqfwwckvxrfpvxerobbgebvfnyidihgfpwnvdulqhjqqpehaqxulvhm" // &
        "deeyoxfwpffotsqneeuhndngrvqkeqmocscnfshmkrgmojwpdwbynelbfwnswdttwdefnsxmyvsqvafn" // &
        "vwoslnmxsqdfxescisibdctoniyfuqpylmgrgeruxhsovbbxhxyrevatiogymxsxhgwqvxtalvxexjvd" // &
        "omqojowyinfqnbfoahsrihhscgitlvyfoulisrtlobxhkmwcmgdwodrwkollnlcwswlqssoadeixcfof" // &
        "rwidlrkmrlmdqkiaufebnohujmqtxqujuohmhhxauqubylusrkfbqqyiomimyjfijfyaceigasqerskk" // &
        "poxixlxafkiidltpxuosyojntppfldygxnlxqltbnwlfqovypqvgsvvwdalhaaxtrxmmdmrejjyclbfu" // &
        "yrhqqdoycrlkdcuhqrwrlkumnlgufehmyplsyklmropvxmoahvojfwrnbqkqceqempenmwafvnyqvcpv" // &
        "ukteognyoarnjamcflcjnbbwcuqxeynyjexsscityfsqtcbvsjgxdwixkbyxggvrupfhvnnnudwjejpw" // &
        "eygnaykwwxncrhygvwhlxresqoxxwqpdjcvohprocrggvwermutckskslknpuvgnhfupxiilkfwgxfik" // &
        "raddxbhrvsxqeocyptwxuyuferpxlypvasdyvtywfeacvudunpcklybggtsrhogbnkhbdbpqatmlxkku" // &
        "hinmfvjchrmcmklxsrkldsqnvsilwrsrtxaeknobulicwmxvreumopckbdumbdwmkpprwdetbiceiqqo" // &
        "bggjwyuiwnbwagbtyvxqxpeaevvpismujvjrjstjvtmpytkvdhbvlywqdujwvnmteqvjgbpqdkqckrqp" // &
        "hdrppmnawdxgpkqcftdurrgfutcbqvoienobprtjlrxrfxmuycgltfyqghugxxqjtobvccevdhqoeakr" // &
        "gdfcnhtfmgpmlehobechbfvagabotqxehjlfyakllmdbrgwumipkbkpbuynxoujfshqkwvfrnlrmhnkl" // &
        "rxpbkgekjahjagcsbsmbprwglhgktyeyxrjuvkfnnpqakdawrccadheulapuvhidqhsxqqjmytqcxtno" // &
        "potspntsvuivckigpxcggooterhiqomlrhvxvsmejivfbcqssbmfghaugkpnmfatvgjmikuhjeoodpvg" // &
        "icpcdulgmvgjdbkerqlhmxfsuorcwkwqhbolwlikkkvfvrogcpfxkvjtbgaluchvlnuvcjodueamrcbi" // &
        "vfnjlchhqikbficomopvwcibvpnvxiqsukmxadxdnvrfdrfgnfgximbnrcqlgbjcpqlqffgvijerdngl" // &
        "hklekpkjfspwqaxtclujbwimmneuqvfwxxrrhyfhenmhkepfreyeysxbbiuctlrtsqdluwjjhdhvqmto" // &
        "fupqbddtkrpqmyhadlaurywimgdxlkcxjqcgmhdasjshtijrdlwprkhlmmccloxcayjdjprqptfsmymj" // &
        "bqoyfiexasakxpxdnmykwuxxhaomxyjwkwidtncvgesarsxfkldnxsshnsvbnmfmdybjnrqdpyjbdeas" // &
        "tnckjofhnghwshactsbjiurmwlnropmvqnvfnnupirfaomfuehidxlokfrjwdrraogadlvplfvywknng" // &
        "vvujdpqcgjncsvnxbncotedoobyuriaxrunkdgnroshyhakmrdoqrdkqlbhkjmdkqvlcrasbooliudcp" // &
        "knooleoxjtdybdiyclwotjhajhtjphrhcoqwxvjnvsgioshqilssygyadpaqhskeduvisngwjrbtvcos" // &
        "ddbirkqcssdinsdmcpgvaymstlymthewcfwndijbenyklnynevknbjrsrviqbvcilcdpmkclawqtaqle" // &
        "eppglwntdybdcwyssfhpeagrajhfbrmnexkkxtrkqgjiqemvyiaephndjwilkkcbmcvwgnkmgqixnmkx" // &
        "elawodybcakfryievwemliboywmxmboxhwmprmcqyeafxfcnihstmovqduwclcpamwvxrqoatbdhdjgu" // &
        "qyfpdrbkeefymupjaugepleawrgsthwpeltustmadoyxybukfyxusquvjcsheqnbuwthpwlfmyedfipf" // &
        "tflwqomxmmpswuyecnvfcmqegtskopbtxhmgrfuwihbptuochpynwahmmhtxvxvteuopeyoscakvltuu" // &
        "hoemthcqgdrcgkhtielstcuiqeugkyyoucmfuommscmspqvtqrwqpveyrmmvxewrxrluvvrrwkhjslom" // &
        "yhublmihxbeyrxmnhimtcefpwbkdunrjvnuonaqrtvaatffaxrpbesydugufqsspismlpdiotxprxqsc" // &
        "bpvlkbavsgwujredbynvmelbgcbwrxewqrduiwlowaimivcryyjocrtbxwmublkotcsptmjpojofafqu" // &
        "tsjlmyhqxumoptpvqjabgimbtkgsfomlyiimgmqfsfneluyhjtyprskemjnjvmmjkhejhqkcsjxhovpu" // &
        "srxotlomltksqfjdjhkgrxvhpinumvkbkaxowruvniolmhnkmuluruxksqkvccokujjtlxstuqnvwfna" // &
        "fbcercgwbjqujocqwrtnqoritefwahtpdvbedhchwukywpackdjyrosbeiqmfasnkcewsrwbcugbebkv" // &
        "aqxvampeohwdajogkghuqfsiubjlrqhiajjyqlmvohtwjcbmjotaokidrkguvwxalpioibagrbgegnbl" // &
        "fiikjmjpmsowculielymyqmatkxuklscmllxtoogqgxqdeckitmtkkxatfuwfhijfeyxejnqvyiqdvwg" // &
        "amlqflgdebfwletrbdalnlundtxcbbhsagyoglfburvobafstulbijhkuwmrnbwjdrtgvyleyxyhvuxi" // &
        "axnwdiaitpfxudjhoglntputujamxvcgofkqmtlfcbsltclmrslwujwfuusqhmmamkcclmwtliehmgcp" // &
        "cajqrgoxsbpooeoosumjupqaarjfrrbagcsynqbjcfvlddhwfklnkasnscxrbukcnknejbbwjwlbpghr" // &
        "smqcithlxixeumckduevlrncymcmgpfkvjkbuivlbvfrajgqdttplcsnqtkjdbiheqjkbslgmhxmsiet" // &
        "lafteebjjmqaitplheekiyrqqtonlbtcdifpvmkrljwatrbehnubqnccddbitlnpxicejoxljenllegn" // &
        "hbcelbonmalaviqwtjttmjjwpvpuloluialubohgigamrrckcplpiqnvowtdvgowvwnarvteccsemgie" // &
        "tnwcxwcvhevsdjhjerupakqhtoxmtbaqdkjvyalwjhxbpxxedtoqiupxqxfetmjufvpuilsvycbhtnir" // &
        "vkbmgeypymepyjbavdeuijvplitkemiuwhkorfhuscwiikhgfoqicwtakpbknownxnfixodwalpubpmv" // &
        "yrfdvuooqsuswyqcvxsnmpixulnvvymhqvcwgmjdosdphgoagmvoltrtvbpadoscdthhvyjfxjchjase" // &
        "eloaaqkrnqbofwrfcusebftiqfrgalhdcfsxtcdqquwjqjnkwdfhiwwqbsdpqecqqpnjuopcgervwyrk" // &
        "hwuhjfolyvfebrubiewrwtiovnvjhvouwoulgtmuybllfnvbjqefwbkqafqvklrlcchhjijcuqhkmavv" // &
        "njjghoegroppkcducgeqidfelrrfrtfuewmihnrwjargsvkgglugasxhrkdimlnhxvpglohcrvaihqio" // &
        "gkaimsfldvqxytbqkbqcitajcuojkdawfaltquainijkaexhbxulkhywjvoacqefngstbssvxkcxrljg" // &
        "icxdcmjommctpdmtrwjuslchqqbumypowrbjinvwasqyxpxndyxnnexshvyueselrmdvsvngstjbyypd" // &
        "qsmdjbfgpgtfnxxndndodosmhyhgoafhxsbkackjvbfxyavldqrufildbkcmsrawiqsxcttfqsclpfeh" // &
        "jrbpuefpsrbipoxxfxqcsjppjtgnyguritasthuporcupsdirutcquodtfcehvtwuiuceaywgwiontuu" // &
        "tlbokxqfxbddkiwdfptsjhtuenspxotirvprihpmfgtsmfhsklfbfvfjpvgrexaoxhbasdeksjdahgdx" // &
        "ooaexhdoqrifdqsaxtiwjttyopxkvmyrnetboggnktwsiuadjyppbxlqarleyawjykoffyjylbgodjga" // &
        "rjdrrwyaxwqpvgxxckxdwlsflctnlectehdlemdvqrkftqmktiusgelsclrfxigsuojuknodjfsicfnk" // &
        "hqiajcxnljxfqinbxurdsoexpsrrpxvjkcuxvglfajqqmjjbevsqwicetdyfmfthbmmdxtvuxyfqmmox" // &
        "acdtqcechdxkymqoayrndqccoqsmtqcdtoqxxkhtocboxnovjklhhbbjqgocfrccotmrwqrmelkvptra" // &
        "trqvyjwreaktmagussigwfsrckfvjigatdfxdrbjdeabcrafhrxwsjrfympwxdfcshauswcyyxfomygo" // &
        "tafxowkwfjcauiwbrwtfgkcaqwcugrggvsarraujhgudyxjaslobdsiyqyerylloqpfrwsbffxyrqsdh" // &
        "knongsljttjjtdpbyuamkppljsbrrvrfxfxxupmjpfeorvlgvistvcvupvioeprjkhmjqyrjthdrvenc" // &
        "fjafqibivfqgsolabciodpnfphpmwftsddodhcyrplvdftpkxjcjaitakjbcqknogwadysldddtqjsdk" // &
        "ejyxpnrqfyxwvaxiovqpkuiurofefiygqvvyqktowawcqurscmrnsfbpfywsaijjliiibccvtmnrimgv" // &
        "jrsaaoogxtvwbsdtivaiepvsopwsurhvsbxtxtiqiwlhmjeupylvbdhbprmepgesowfctwhntjvpywrg" // &
        "ehqfpvgnehwalvvuoxwwlesvkdpojymsndbmycrqhatbdcrsbysffnyapkjnbykdhefbtcvqswmsqfdm" // &
        "qhhwtayptfeqtfcslhdurbrrnqniurqcueysebgbpabtikmlejxcrofbwbuioplhjxsafyonsgvydbof" // &
        "cklutwjfucwvkmiwqaerfmpshstwnstvcurcfmgraiakitwhtnswfebjmfqlxyefbhranjynpyphqwio" // &
        "cfgwqkkbqgfkeexuutuqhrdknmibdhscatnwjowtcuopuaghhrkssfoydwhtqmisqbvwhhyuluynxofq" // &
        "ejbgutodxjcsrchkiplsemictwlkxeukjwswwdaiuvwxanqxewvqpmmeiqeftlaflbvswfjeyqkghnrq" // &
        "sqbqtmunnousvlhnynytyyytfwmunghhlvhehovqiholftmspvaesbyvlivvyjwehmauolfipsbsnrwm" // &
        "hgmutginiercduccjbgtktwswseiqccodcylutkpxbiijrcdbxntmfsuwljwxbgxpdwkkwhubgsvaaqc" // &
        "upjxmjvrvcflnwyoieeeinqydqruopaeuopkxyjlxbmmdbnhydytaejhobbesqnwvcbtttkmyxiuiobk" // &
        "juwgqmgpmekpmptxahrmxymwfhssdqddnrftreuyllhrlqklpayvrjeqrvqntubdglexugoydifgodbr" // &
        "syphcadjomuncatnotanlbodbvlthypjhqqklfgnahjoewdplecfkecbyoycwpagesgntlgndquapplh" // &
        "declnhlgpdcevxxxheaywmrroygxmmgquprihuopcfeiudurkgukgomemcsyaneidlsuifyeahvpbvxr" // &
        "ktskfclyhrxjbpsjhsggdpwiqeolhaodlhmdytxchbdapxntkqgsqsbadjlnwpfekdfmmjieeqfbotln" // &
        "esuoxbhrelojpwdkmsqhkakisemapyovbgxamcopfneebbksdlxqbvenqsnumqnusjtumhoijskupuij" // &
        "qjsqlfpcumoequwacxoxlivkeygtwedotyotntfnipwhxsfjeokdlsuddkhbwruvuicketdwbefejqiq" // &
        "xewwyfgrmlhjcyyrsbbwnlyuoxhjdlbgeledepuhdrqbftpwcmvcxdvurnehqxsbwloctdqbyxpjilsh" // &
        "ljkosjnbsfogvtlxclvwlltqkffcahlvhrrlkaolrkkxwxahcfjhxtqhdimurmxljhatidgqbgkoncds" // &
        "rllxdpvyiavmredudwpdqswuakvnbpuinstfmlkpgfoueixghxgkjhrqrqwltdytsmsoybyqlssjgnri" // &
        "uqbreckcyvtyygwyfdrsaobrvfbvshgcrdktlkxodjivguspxsugqqteempntdwhxrnqspojjoipckqn" // &
        "wylnsocqgewmumdgdessdtafsiiehfngimfexyrakhduggqvvuyqbvawqvvgpibnneegujppqhqkaltj" // &
        "kcprlsywtgqreargfmieuamiwlvqdoyiwjhsejuoujcglmxtturjdtrucorokedvymxqaaymwyhtingr" // &
        "tpdrjxlamqkjdpbjgidnkbajtaixuxrimnycphxmydcnbneolyqpjhpundrygwchryexdwnmbqyukcrq" // &
        "eilhfbaylgaxvlmusmjlnfpaonkpikaumskacrsejwkxhnoxikgcsvogcqmgbnvqqxxhxcerokujvmyh" // &
        "uxwumpnqvwvvpwrajbqggsqujsfndvjevfnhgxyfemmqsfcylrjoxvmxckgpxvabnvuvtpuaweuketvx" // &
        "fojrrpfpcsxntwxhajautukbljrysykowkutvtdydtdwqkurfuayxjijhjlwhgwsvnemojnanltkppuf" // &
        "prviwkvhnkwdvakmwojjbkysjjvbdinjnjkuoarmyfqwnrhtimkpewedfwkqlfpuccbedeujkgidwqep" // &
        "xgcbytusrpcrwfosfnaibuyqhwoygvbgxcpgbikuxwmqxcwxaautkwbqptmnbqsqxvyhasxawwbgwkmi" // &
        "wjjtidbplnuqnfrvccaktbhapiudjkhsokgcjyyexxxgsmcyluneyfmcvjkgqgiawquvvptmigafdksf" // &
        "fpewmpoaqnvijhpqpfnoprdxoufishqwkisfymditfsdnckuppdrqytqsojrmrxcsapmivsdlsiaxaxx" // &
        "gujxunoanbutwjhpgerlnuyjrkkcywtsjljikfkajrfgslgmlijmodxolutlttdrvobcwfitpnrulway" // &
        "xrbuuljlipvcjtulwlurhykbjdckpnmkxnwlamfnbwuvlrslacwudpvrjcbmrqbnvpfhchiflrtlxtdd" // &
        "tcjmijtxfhcfvctgntifiucedboqlbyaqmvncbfametnjxasnbxrekjvkopjerladadiaoofjpcnwkwc" // &
        "tbynwkpmgrfqrjoswqwbuxkjckxgffawyefdsqrwswwuysueuemtuetgiplmtshoalvemlmddasohwnl" // &
        "jwswdiymueojrhuojwilapidxsnptucqwxsifssjnfegpckovwutmwtltixmluufviqisfxnixiibqpi" // &
        "pxdmgpbgdeqwkrjqphpbqqtpsmpdgvitpdrjpmfitbfjribtqwnrksfqtjlhkjhllimgvxaxkjetumtq" // &
        "yajuultoroteuvbtodbjgbhokujlwixfnhpfhwtgkhcivbnidmpcjxgdipelagjdgabviwppjxphxeda" // &
        "invbdfjycxkqxwmkuroguirvfycwtmglpuqxomsudxkwkwoyxosebayutfqtnkhatbuevxkukiptutnk" // &
        "hbaekwcfabjshcayhptpsmgqkscmakiireanaxgjsgjlftkujkfihcsekbwhsrjgjcjpwriimkpbytct" // &
        "mlyyvntykjpkoxorwiqbajrknxneuappqafmeluqpncjiabrwylepmuxoxoslxauscekxamgiqnvjppk" // &
        "hxsutprpagcxxahvwdmfqiqajollbjwwwpkxhagmrksupgpsvjtvygmagsljydavhuviurncbclvxkyx" // &
        "yyhtjqlsiilaptfeqgukfiarrnnldeyhmfcqksbmhrplhdvawjalkicwoxcdbctbiyoujosjfbjjveim" // &
        "xhmgxvxsnorsrwldntbeadrujdcgaovvhisukmeimjwnwgjkmwsbcslbqlosmawvdfyjovgllovrwrmd" // &
        "kvtibyckdjcumvvbikqelylvdphjmunshwfjsargmcjwdakhljdshwgqtrrjgfmiwrguvenykqlyjdfj" // &
        "oluqqnuyhycqigippghqhvhxbnyswsflctqfpkumydwdlhbyxbovdbibwapgysfmmbwpfayfgexsepyq" // &
        "uqwwhnjkrdhgdflbuhdohsvanawoqbbeohkvlgelteyfrvtocimmxlmejcrebkfdmyrvtmjfggagodvy" // &
        "vinfysievjofhdrlglypnraisneygcmtgullqpbtaadvitkvvhbbfqjrjklrnttwawicnnhlhavaauun" // &
        "ipmlhsqcsaruougpeiewrywsvowjiriqyccukiubcumakjcowqiknwuhxtpwhgsrvafhbajnswyaftja" // &
        "pofuejvfgulymysyfdauymfjxjsvjjmogcfqgishknnjpahjwmeikgnocrypsmuvwkkrpatlvvvuprwl" // &
        "kdfodnoudgcqdpygapbrqmfeofcwkxpweqxfpdvctmbwkpmjdreanubjfmontjygsndtrydjyaadbfor" // &
        "prufwnblsdfqiyftdapgbirqaxtwkobwlugxgadvhrhqdcpiounvxmihxbcreicrquqitekqynntcpqj" // &
        "jnsjnfcffcovixuhetekhhlfymdxjiqkkaisgxmsxkbuqvykasmufcolejoxcxukxnlqpuhhkxwislgk" // &
        "mokrwxshchibwrwwsfwnhjhsllmsgcdmhsifxdntiqobhovqecpahgryqnihtalklocpiqmcdkygruek" // &
        "uavcamnmpjwfufhmqfmndcelroobxmgbjmugccaxfxfxmfyxreeyaypsrswibbqavfsffbcbocdcmmum" // &
        "ohbbnvekjipcuhonmqfudytimnxlsisygkjncoyfbicqlssclwdmgrdkuriphpbbdxixognlwoxeppjh" // &
        "nqdamcuunyfcnpotwucnqxgtyfqkdtlybxmgkjgacansakfitnpfjfdsftjtcrcsqvlditkslmufbymb" // &
        "pwockilxixfehbogeaqoiqutubiyvxjfynnsbcwtuqmmmtsjxtdmylofmaopqvypgqmibfuyqaikhfbf" // &
        "lsnqblnmcfsmaiujdarqusmqyhawtdimmarrvdfpdlslutsfugwmownhgfhokhbsdysuiowqhnfxofsh" // &
        "tvmmgmmsefkmiprpppdqutaxwdoycikdgxmecfsmpdwufawpcfuexlwsbwugdrakljhyjkkpgtkqrexq" // &
        "tdyyaihlhdgtdgdsmjlapnrbmfbwnntybmwvelrvyuvxowauluceywisbqjnkrlvnbdubnulexrljfon" // &
        "thwdhmkkknqvoqfuhpsumlnktuyfiayamfqgcscuvhpgxolhnuiiuxokgvysartbdacdcwnkxepskdjd" // &
        "vktnkgdxeqhhtbyusjirduafpfqmkcbqfehxwebxfrgxmumujrhmqhcnvcyfsinopfpbfcjariyiitva" // &
        "aghrbadkfmnyyghyvdeeakjomnchnvqgktphswlvgjwywthjpnoboblmcownuljahqvdxffgentnwelg" // &
        "bnbmkilgtnceptmqlemtbbodaapjybbwmpypgivufpjlbqakwhmksshbpqqkphucwcvbjfawovdurdjp" // &
        "uvehhcloxkfhhqytvhwvyukyadionhmucqkwehrcddjbnmrwtqgmhacelddqubgimvhbqtebaopxoexq" // &
        "gwpferiyrfbyfbmkkkxjhfowpupuxmctaxwechootysjkkgptwvsujtfjsdolsnuwmqqcobubrrfqcqo" // &
        "gqevqpoysigdqhsythpmntnyybeotuakinpnpkprxclfgqnreirnuqandkjicbvboatrfkgcmfsderes" // &
        "vpbbpqpfxbkocntcacxtynwdjsdrjhjhdbghjuxtspmeuqaolxgyfqduyuwgxipptmbphqumgsydtrgt" // &
        "pkgxflwqxlufigigylpqklgtyyxycsylbbxbtuamsmadxmakpwgcegbxplkcxcfyblselmpgigmaykvf" // &
        "fvnbfawglaoabudrwvmewegsdswhsitjebwsjadsoeuvacxdiymhiekikbmmsoapygmlarpyfxafqtls" // &
        "bparnxwiwjbuomgyqcavwukerrceedqtxwopqcwcuffmgavwidrosimiqtdqcrmuwcwgwdmoromkbcpo" // &
        "rbaqegdvxcffqoiskrgnwwysiyjmrtyhrnpamgftgmsrybmodcogpiyqhrycnwkfgseqrhdsxodfxhfe" // &
        "dxrahtyoojcjfkdqofkglllsaoxsqbnetgfolwjgmawwssalyjxbtbbbpdnnbcybaltngwcuqacgoqcg" // &
        "vscfthclfjqivlrbrgaorqdtdqkhckgbhkyvpqvbrrtaogoytibreasakrmsyxcbphcsweaqypqjgplb" // &
        "oybrsauwtypvdlqaghvfquriqfjxfmoumjauloqumpgyiaerllqhrueawiscmugivknxpdjlgnniwggn" // &
        "fvarkaxtfvnhkfdfrkhtokwswompavxvlqouyghhhwhhbmmtoxnbnewtybgyearxasnnwljwbphdbyes" // &
        "mwwxxienuqyksnuiwieuoyylkttcgguqtsvihmggtbimkxwhfajgoebigdxkujgrvdorccvfklbdpchp" // &
        "tqawkxhlwcvqlijlpuxhbwacwlfwydyyvuwnmrfktkbgvfttmhnjwlluketmblapugogmdvrkjenlcqk" // &
        "jdotgpinbrfefentdjxqabafjcsffyiltnxiklboleateefptqdgviyojtcbvqjnxkuglhcvcskmibrr" // &
        "pdyyclprjyvtgojghciixewdnoxrhyhojbrupkuvlikmmesbihvxalnarruupxqytnsjusvgmkhuwryb" // &
        "hmwkkasujtrfhcpyiflgqrgvscgvhncnucipnxffukgymmpnnnjivclvuyayteqgbdbgoklhamstpkbc" // &
        "ufjjjjdvkgsuisjnjhmrsqsujmiufqguspqaiqtqekqowhjwtyotibppemmwmrbgbtavykymuqnnvxrp" // &
        "bdplnylhpegmeuvbcpkufhhopfhsnxuhjnaxnkxnqfuhbyolfggllubykiqiurqjpvpmsprakwxxnsuu" // &
        "ohcluumrpuccckjymasbyukmjogabjnqiqjjahhvajrkvtoapwkgsupbmegnbyttrakvdiiilhxephyp" // &
        "erdhhlaotbxlrlbuldrdykrrbmtrkxluxjqgryvqumugjcfocfyttpujnddnhnnxkhrtlmsterlhxrfp" // &
        "ymaavwdnwtgotokwiyysrqcbixurbqonfglrfkanppqehqnyuuugdgutjkjequsmwxrnqascuxllxjuh" // &
        "eqwjvmmnjnaedrltvciewpaietmthgoebicwwauviiwofkwewlkijjlgvfturlfjdgbseunujjijpcol" // &
        "ikinyipqfkqqnsfwfsrwptfqiktdayenmokxapvtcxvcugwbcjbypanlroljcyuerrnvdswkmugtuosg" // &
        "kajhifodonjhauyvipikoqvjwchondqbmgvxpgogyxhsjokgtjwqwrplixsrrqgmcljrwakfclqxpopv" // &
        "yayomaksojdxmngntgtjtnyylcykoliqguabkmfqeyckynvlwuafcewujxuojaamihwsydwowvvmigxo" // &
        "vjsewnpjxuwybnvcavijhtxixqomvomdmslehlqmafixnkdybpmfmtnpglnhljqyfghsorsjwsegbuim" // &
        "qgssrknpohbrltfrwrmgutwbrshnpedkoevloirrwbcannrsbmkivmqbakjbmymxjphaevfqdlewrfqo" // &
        "iqyprmlipirdaytjayrlthfrrgchmwdqquimnirvnmrxusfrkwkwwrdwtvnfbniwbobsgyicycsdxvpk" // &
        "owraoxyghvaocxnlislytygyywwxeukeeiwlfeuhxfbluggpilyqwioymimyqfcuiivruikdbpdxyidr" // &
        "hsmhlrfjjofgmoftbtpaqpphwsxeaonlaqofhvvfyplmdmvumnsmgbxuljktxcgebcfgmwyfwryjdvxx" // &
        "kfolnmksqmhhtjukcwfknifunulgfnosnsswuqrvhdjlafkmncyisodraejgylmabngmwjwrhhovwsvo" // &
        "lfiuctptunfmfikpptfjlnekgqyvedjpcqohvwewxxehicftpenjhlpgobfyuqckdntercckxfcoosgs" // &
        "rgafwoxcodmhptpplaccxrhivytlldrbesncptjthmbctkboarhblnrqyvtgtptgwnypofrkuodceogf" // &
        "heuwdvafijgbrqdyrasruaecwwwpeebvehpwlccbjhydcsofxfxcogtuiswikxeplfpwbiftvundqoyb" // &
        "gxwytbsajgvjobeuuthjhkeysulifhlyoxurralhvdyjoibuksqttymmamrxrneaueewbbilmjvxdvac" // &
        "iqkmbxjmjxghyyfqksrvxtcrtvpyhyibatkyylxbexqdushbhtqpwajeymahaunnmssewfpxiedqoebn" // &
        "rianuvyhidnicmhebchgyggfcqiyijwvdlsqkkwcyxbvushedkpxuqndpdkowcwgtfxslwbhvfpnqfso" // &
        "pewcaqdlrjqhsfgbpwignxcujkpilvdovcubwgnggmcfmgekfsfufsskkxueuvpvanxflqfbquyptyep" // &
        "fyoupcvublxhowunympdqciuvhqugyiiwmijhjmjplsmlloliqsxmaasoffcqkmougqwgsvvjvlagfji" // &
        "sqirrxckvokxkrojddstudouwbdjgjsijycwurubhslitdmqvvnggeqpjytuvtgybchllpgedvrevkiq" // &
        "syxdpyavaftyjjjxkntofgbvqebdsocfflcwnrfrtpplorkyhgcbyosebvgewdlabadhyrpawmjipoww" // &
        "womxviixthhtlnfggdbmcqueuqlubffaigypnkoqbwlugwxynwjqqwhrswiuabeqjxnllpvtivaxfvnb" // &
        "dqqlyeekqjsnkvpywccgunftcvumflckymriguexplqiivpukjdfqqkjfdyiotblxawenobpvnsstmmj" // &
        "hfoxpvfojvjklodwxahcraixitxlyaagrjovmmlxcceigxsvximpsvxquewiswofekajxppqqygyyndl" // &
        "ljmjyscrjrwmtpwnwclqtuayiqjdbdhtuudmofditslfwdsncnsmgsvkskwlopioobclppnaxmctgfyi" // &
        "lhqhhuyabkkpjiujxetbmrltytfcmqhpltyniufmiothlkeriphrefnrqfonhbpmnuwhtwtavtycocld" // &
        "xiomfxllojwmtgcdydmafdyjahcmaipnawjdcdpmubmtdmtgmrwyovoubkrwfklsjvihowbtqftdcymr" // &
        "veiotsskhfdrpbsvdmplmcssrvivbdruvjbxiashjgbjdgwvliwtrowfgbnnfprdqyforeudkowsyuxj" // &
        "dmywkbwbfgshrbbgmmlxiluwmilkqdgescmmcejqymcnwdcqgwhmcagncqjruatilvbextcgunqihwik" // &
        "igplwmmlwwmfamfvibfkdhuxgdakxlxpwahemfowxoannwgkbpkiagjvhtampipjhqictjjsidduyjem" // &
        "qbhwtehkxinsefndkyfaxcjkhvtormslgjrxuomnamfkerpbgxdtwlgxquqbamyptqnjpatjojxmragv" // &
        "qquabmbgjasspditnbrrmxrvjkpmrwacydknrkanwponhlqbtdcgeggnmfbrqtvyproocyrcsdhcslol" // &
        "fqjuingrtjashimqvseocbqmfqiavmvodlltltvkcbypnddnqsqmqgjifvnsowgoeqspakgikuhmjlap" // &
        "qtfcagwwhaajtrmuudipdbkulshekttnlaltqlyievtwvifxhydyavhpxshmjgevsfkkphdyvqwcejno" // &
        "ounejdlinpomfvvfblxqqscnrmjtgwxtefmrsodnfqalrwgqmtirakxgxspkcvoekptqugxlkyvqckjn" // &
        "afoiodmnavioxhatmhrdcyxvlnojrkkpeelcfsfhntmdboogymiaxkkanvkpnmhgrritxjxuhqpkkdqc" // &
        "ellirvpopnstjaucaxoygeaimhlbuyhyejgohxpibkkdwxkviduprngsesqspxfknkjoskpqicosipok" // &
        "iuaprejuaguvemlsqkseyhpioilkrdlykjankktoydhhlobpnybbkfnopdfusybumgsskqnmubrjkcml" // &
        "olteqekwafxlxmnmjqbdhgajhrrbnkqmfujxdfutojtsubqkmfebdhytouvfypwexrenjbateshnrubd" // &
        "isekaonqandnvseclkmwfkrrxrkliiyvlxusemnthevpdkwlwmfyddotbmkaxjscnjbsrnlbuybfirlg" // &
        "xavbnfcihsxxtaxqnpuwcdaouhvqdrtulsrucatawtdethtxceyunmxwgentfvlxfvbvheosxvilcvpy" // &
        "muniafexmhbsfsklwdlrxulcjdhyotemepmpheeeltkitjqdrtntujrskilqxwulibnjhswwcfjxmwnb" // &
        "lvffueniyurmpqegtujmfdrfwpvusulsukqoyjbxibrajigqfqjcbyxiacjbjchyhicsegptroyuesjv" // &
        "odfrmqfmbjhtghnkglnrynkjfjeiujnjabyletcvcjhvmifhgqfhfuvntpcgshgatfenkldkppuxvhti" // &
        "vwnlwvfjajcjnsgsknujjpsfxyayjnvtetymnyuvrojgafjreyakekgwjisdnjhhsvpxylqxkwkrpdud" // &
        "jciypdrxrpyjaosdvywqiuijwewrkaexedfbortrsqdwvplsnhjbmopxdneomedqpllmnqefctbqjlkp" // &
        "jenllinaysxreixboynctlefrmjlkbklpedpgqlwkfbwrmjbabeqxgbfipykfoblasaldbapacsmvyhf" // &
        "fofojcafuuarajqkbelbbtiuiowesrcrbxuumplunrolaamyeqhjnnmihuejrawuisvkdwuahtecinbo" // &
        "cyeenfolaxihsbaoeupjhrnfbjtacrneegqyacrowwwtgwhlrdawdbocgwjkqxukjwvunmagtlbuxcjq" // &
        "oowotlfsxuxbfbpxejwqlqeyypkpykximelfxsnjrwivhxkqpvcabyodnreriidekqhkmllbrqreaieq" // &
        "lmfigqmmqioihmatoelhgykcixaoelydhfrjmdmqldkmdtdbrdgqhavpiasjrbfudwpnrovohsljaxrs" // &
        "pvvkuqemjgsdbhytqpfwnreurtvrxdshqetgkgdadcnqntvgkllcnprverlodqfxflbvuiepuvwnxjon" // &
        "gskakjjroiwaauoqvvpqucumnthcdletwdrgsreadpqvjyonaqcvxqcwfuxgnflamxyaglynhmfgauvs" // &
        "llvloaqasrindxsydciaqetekmutadwexqdekqmkplbhdbcblpkrxbyhnyjemidymjimyinvmtqytjpo" // &
        "ylnqcwvdfltdheursjegsikodbatpsojjkjgjfrnsbewjpeijpabocbwwkeuceaehfguagyqjfesldpe" // &
        "hnqktgwujytpymbnqurbkmhqxfgujoogkbgccqbyhnwphsdveunfjuacksbaqikplvywlldnmyqbponm" // &
        "stivotqsxlapxxoghprdqmjguaxqtscleflrmwobwphhrrfstyiyeyhgiqtuelbacdtcvwyescslmkjx" // &
        "lgvdhscxfsxgupccawohmmtjfbavbclohotijklpxjucgmnnocidfrimmjttbjagyoxbncdtqumsrgor" // &
        "mpolkfiycyycfplajabvpqsqrcwxbkuhiukwqnanbymancjtwmxdukyysbgbuorgovshfoxridafiljq" // &
        "sujiqdlargqvmdtrxgmpeyamyjhgldptbplmrnrhpxonkrfwradhkajmwlcxlectxvhaokdfsrsfegmp" // &
        "icujynmycqauvysfvalqiqkfexxcmxlnnyijoudleqhxgsvuplmoldgstskkxhwbtyroovstvgmlquaj" // &
        "iwmhrrkkillchbvlxiybyhqbprnfqqgcigpkviyxstigjxcfeedermgrdpsfnspenahffjujefpefkwc" // &
        "blaswperaivbkrdiabmydvavirdsvfgkqukuvhwcyvligivmylbefvsxsunhnayuowqmbtcgfhcyrujx" // &
        "yufunajkruooouxpqhvgecxxmscctgqluxtnfbncxudxigkdmoytaodgqwbcmdvgcyutohswivnhjjyu" // &
        "ehinmjihelwbakmwxpjsykcnymeruflyonwimrptuburlewfxbpplnixsyqkmubvcemglftmkrfnaoma" // &
        "gkfqqjnrxdqscytrtpwfsyqejsffgceoqlsysgxtaojutpvxikuxohleeaqefgftylbtifyaywdipgsk" // &
        "utlhtbdfvjirhsectanlcdgmhucjhdyakwjfuykeowubrwusdiyasgkhbuiojdwheefaahrjikwqxlkt" // &
        "vljpcoqeeoiqbslujtaxmbnnyihiwljlubnfgfbquiflhuqaxbidlfpadapckmxboeqsufjikdcocmis" // &
        "ywihfpvnodrypfocydvipnegslcowgcjijotahkclcqhappdbkhucnxhaqnwokxxtspwfakwlcveysvy" // &
        "hxksaqwpakysklwcibwdhlhybrjbrbakqccfbjueynecjucytiivxfhlgweovuacqsvdidudqbtotcfc" // &
        "tpmfookytapvmopdthjnkphdrrucniiaodvjinypadvimkgjngcoxysdwwrsaejembkixmxtbuykfesd" // &
        "gradldaotdhsimiooqpwuwqsucytoyocdrxpcebnedpjocpkgsxawpnpxqacmhjctsbutulklvkejwwd" // &
        "woopatetmohwkcebjtkxmvcslhomlxykttfivpooxamhwutyhocdkrehplaysmrutumooehdjtdhbpmh" // &
        "smevudefwiqjdksoplnljfgxayhcpyhlplvdhvgsebeqqyrwewyxrunqhrdfdchyfpgdubyfbtkrjpxa" // &
        "pnvcjbvihwlsbheoolenfjqiqfcncmgxrrfyfursivuuidonggxrrqqiafomghcbkklacebvrtlxhdqx" // &
        "vlrqsyfgrvluhkpncbmgdphhlewauqammglicjmbjwsmeosviopfdsirqjytnejkcvffrnymkmuvhpjx" // &
        "jlxhwfubluyijnxtlnexdoxhvlusbbyhmwumglhrrebhgatcrlcetxjhqqkdywshivdbwuxvgeuguhnd" // &
        "ygwohpidabivdxqnniahuvlthcmysiadlituipistlblnlosacnfrceqbhwtmbbuiucckhpqcoepjxsa" // &
        "huqfjwkjvyadjohrxduoddmeunsecuemmrebpddikjcxltahanlusbbnqbjxecaeusfaktkeoicnoikb" // &
        "esddmrvfvvrmgjemsegyycbjnhqeijncvrlgomdtavmvmudjdcupdjhredyyrjmwudorcetrngfpcmde" // &
        "crijthwtrywqlmyobyrabjwvamtesiotvhqvgvbocolwijbdbtjiqjlxgxgeepyequlnkhnbxevimfjp" // &
        "lchujlsuhmcshndoaeujubvymdqvxbjulkwyjjwtjrsfrciubpwnbeqqlvksndtubyqwusovmjsrbmis" // &
        "jjmoscrullhgptasxmclmtqegtujojndmhcwyjekbjfkvstukjyimlyjyyyxquvgpditpabyenenwcgs" // &
        "ywjsxabvtokoukogkgvekpmjaplgadtccksthmmiyegqnfkriuoeauqgcpkcvsdjnaurhmbhegycofhh" // &
        "fayhbtpinjxelwysibqcbseekooqkbohufxbsmbqdjlcvffibljixxehspeogsyigqpbnecbipnopwdb" // &
        "nnifaaybhrjulosujsbnoeebmjqllwqlijltjksxrnhdicsxwkjkxramkdqumeqqkarqnvwnafmfhjdj" // &
        "fuflvdsjlcmqgstvtfpullctphnxasjogxmybhofdxadmatrawjyufolyxvvrioyebesnanqchnvdoib" // &
        "nlnyqysrqekeevemriwqpxxkdpwysutbykypqpvoqqfbmktslxwhmmeutdqrldohcuxhoqahxxsbtpan" // &
        "hrqlrqpwhkdclvypvojegtsjrdjwkabipulonnkvfqtbqulpqpsajayxtwwavoluhcrwddwngjltsxrj" // &
        "nipydbcvevmokycxhpnudwdnlndxrsptrivtawdregnhhgdlonovqloguwjrmrpyurpxonemtrjbcvih" // &
        "wxlrwcujktsddptannbwvqxjqvtaduucaqxgwqfnlsanwbqckollmwjxwdmpjlsxhvnmlpoedtaasfvj" // &
        "bqooshykkpsbujtsuekkntuupkpxwdeuousdfrgewhfkbmbgmpfgbqhoxhvgmacgkueegcxanxfqumfs" // &
        "iubxppsuggyccbgribnxswxydpdfrfcntpbwnawdyitpcbnmrqlkomgihoyjsgpebjuogghmoddpipdb" // &
        "gvdbnqcfephxjfohpxgpnclgabgiwsawomkoghfwlqcrtywefmwwtqcxdiytqttquxbkxmlpqqxpiukk" // &
        "cnrhglqilupjamfvsjtegfqohonqxrkuywlfakuembhlprsrcdgigrdkawboyjhdenvrhnnwbhqbfisd" // &
        "oxljwkbvqpqpnnvhjlkcqyhjxntfsemwjgygdqybuhetljrkysqtkcppieivyikpjvjecspijlqcrjsy" // &
        "beiycluemnscfquaryuoaalhnwftopueffexrbgmpidehcgrylgdmhpjskbnbfvwijnatmsroykhqbyn" // &
        "bkgywndmfdsowpekeqemiglurfwavnitartbhgrjewnuuyjmouelojqttonqqybtwjauttmaowwvqdys" // &
        "aifjaadbtwjmnohxumlpwtqxwulbrscltcddshtdkcrvmhtljyphklhawiqdfdavajhusnphwigmgnsy" // &
        "jpweohgjfdlnnrgpvxhmifvwwfctthbeteteossibdjrurlyawfbqcqfoembafsqwubucuhjyhhwdygg" // &
        "dvixkkdadavdouqkrprkflhsfbsufyyqiskpegyfeiejdsytedvwhtryhbftgxpmhpmkstuvdooyvhgl" // &
        "xrdgcepoawbkadwlptdpmnqhysrlrpwbhamhjsuntaaougywgmapdvigpyoxpfvjnicfsixnfdbbfuaa" // &
        "lcykclndnjhfdcauyegkjvutlcxcxapfdvqssoedosuyrpeoedpbosirbvggbwpgjxepdsmywqhkyvie" // &
        "ufuvkkqmkngkoqwoapabduclwejwbidjqwefloglbtyibyloqnlhfnfvbjuslghswsvwoytluqxncbrd" // &
        "ttsphfvuwnkvsspcjxwwpnclwxnrjmbnmurcdxbfqgrkbsuptmgpciivhtpbujruynwhceqhffykkkbt" // &
        "hcimoqxfndpxngfbfoymjwnkxdedndhsurpqgcvajafoythjpwupivprvaxkbihwljxeqfajtrfrnlts" // &
        "ihouqkkipqstaqiwdmlwovvobctsstbgoqopsejqubcyeaduyitdjajwclginbshedrbohsfkvyjskpi" // &
        "bbqvomnoqfopusgrdxjyvhseeqofnabrpbekfssvniyfiosnwryeisxahnlypdukpswueywsarkiytcj" // &
        "dhciajvtcjyumpgmddxammrxtnnewecrtppqjrgiunlatfycfguuyuycrloswqxtruokqgecwxklimjx" // &
        "climjoabdgakkmqadkomremfewxxlcevqgsaihqvlrrcikxjnybdlclrborhybpgdsngliwvhpofcgrr" // &
        "liauabnuiawwejktaamowyrxswkcmkfqxddvyocbtieoebtotwxddcaevpcqbffuvxcehmnartgwqqgf" // &
        "lgsevrtghuvpfhxnmbkwlaejmlolfovsvnrcbcrtocjrkcmmbxrqqpjmennytftdsdstyfwajqhrsswq" // &
        "ganhuiafgssnkpaiacngbxnotgathtchdhjcyexlxjbsqeefjbukpfhsrmqynqoefkrxultjbbtxabue" // &
        "mgdgxehnumxwajyrcjkfkuisdsedjlbnfocxgcvvoxagwsrwcnbyyslefumhrapovohbeopkleifsqfq" // &
        "txveufxkcxpktgxuelluuukhocddgacameyviptqbhgeowtcdegxbfemjlbljuagqnnprlxwimfinkcv" // &
        "rpgydnhsusmbkmipkoeoxmiycqhrumgqmvooibxdioeffcblktyooypsyugryrcsldrobkbowvlblpfg" // &
        "ocyfigeyndnimqgoitgercodhsyymmigwmdptluytbbjceaybeheksxshjyqqnbaksjekthtfkovsnem" // &
        "nxriivnrqaalsctndqmkbbclaaexdqxhkvrypenjewcadfqavruhicuqqouwvaksnwpqdbaveeoqvwrs" // &
        "dsatuulotdmljrfhoucqssnkugtnkossjapffahpvlpdrcanwtsmsgncascsmvjmnmjfqnflfrwjyggb" // &
        "irqffiamuwftnmgutceumosatjfdpianadwucvlfywtydoefbqjljgihafuxselakthuppjgsqchedwf" // &
        "fjqwrbsmbseluogjaykpaqadocjucnyxqishjkfohfbeupphahvxmdldcsrvebfkfxbukiwvdwqrqmxj" // &
        "ksydkjvsvlxmljxyuvuygwmvryjboxissapfgyagbmejrwfebcqmmbbmujndypghpeqwuwngnalnddpp" // &
        "ojbpbimbpkclhykhqnqcnblqvjkgfjsyjsgwlalljcvkdxapcmjuoteckhgsscqcshhtvnnlfahqlnvy" // &
        "wvafimejpjyyjteefkwwhrbymxqgxtcyteiddqhtemjdxlavsluscdhvgnxxhuiejeuxwiqjpjpkprrc" // &
        "dtyechybcaynwyaefqnyxislgofuccexfjpgbciwcvmyhqwtolkqlgnpqxfisnriwqeookifnamsappk" // &
        "gvrfdqeqdaydkjlliebianntyccmekhlqvbxcwuiyovfwnlxnxsjtbhfehamwriokoeqkytwatitjupv" // &
        "jwpswruqvhssklcikugtnyvgqxdjyhneebvomutsjhqgkdbtdbweiaxhrpmikpnbitbkmdxpvtjjcewx" // &
        "rqcmlichqbboyuusqlwmbugfanssbgcrfncqxmhgyyvtmttsyvmyqrpeifstwmtnorqbgtbwoklfecun" // &
        "fkuiayltwksxpoirrtiobfalrkclsgnktppsmjsgqpwqijyqupgbraydssfxdviyobjmvifkiljiuqfw" // &
        "iuvksimfakytafddefsdvtocllfuijyrpdatutbarjsowegqdfiicjfsvpciudxbygykxocmiagowdpf" // &
        "ksvljdiohurgldsqgoilcjbqpxsffnyblweonpavgwmqspubbhxobvaeajpeppsubsardlvnktgjkfel" // &
        "beykixarsblpdxqxhlcrfxsyhsbhhogcsmmudcythtyikccgjvukcwydvthlvwhpiosfvbvdsedapgnk" // &
        "rpnpyunolgmxablnyuufteatimxdchpasgenptgfibhmsdogljgqlrnjkosspwfakpennvjncuworbxj" // &
        "jnnfifhvatovlchferqeciuskdkyhbyetapvyptuybnlpmirwqkcqalrgmktnqufkkewesrcgkjaisad" // &
        "mwtvecanbhnpbdnyiyhulyqxbdbxyfpephgmgkbechgckxjcscblakhhnapsitchdfibnlcrcbbtggab" // &
        "grujtxbfgcisuhvlfeifnnhrrebqcrqicftwegkplcmhhqprvrwarsuhlsoanvnfpaglkaugdvmowxrp" // &
        "nnmwgquxtqsvgaypmqsvlomweevhfjvrvqdgtmbkdmtxosbvxytkuwjwfettyndmngdbkwriapgddwkc" // &
        "ptgsiryednqhqynncdjpbsvrqqtgjbpciaomcgttprlmjidywmwukhnpqdforxxinukteleysseforui" // &
        "wautchslsxmkfduvooyxhmkojrnbcgyxkwuqinpsahfcjfutcmrqdwwihpylrbxijamumgaxanskltga" // &
        "qhblbwuelvsdhshyxjsjrewhdvummduxxvacoxivafbdrlpyncjposcvmvwoxcglkwqypjxqiivlmfof" // &
        "hdaqathrunmsiqqkyvtgcdpieurnlvcrmnsmulsuadirufqkwkdbhangmhfrikaxpmtmlucrqqpxbjip" // &
        "phjnntibdifbxkldfyjwwypnvxjpicbrodereumgtewlnrdmbhrfgcitilurlcsqfoygskolerxrvjon" // &
        "fnulkurtjmgggtalddqybremblovcqhynxlcyaiehhdffmjybqxpwxsianfnwrchunudgnijsqoqbske" // &
        "tsspffunpvwqxmqxknbmoifaocnmywesbswaeayvaxbwwpwwwkcnpvnyrhpobtexmvjcjfmwlsflxnmk" // &
        "ogjgqyuwvtmnhfyihudpltwpotqognmovbdtetcfgmucyieslsebmxhfwftqkemrcsafnqgdsdwjxofc" // &
        "aecvcmqmyyeayfodktlupcfwtepuxcfonbobyserfjcyfsscbivcnktvusqtnnshealhgeylyckwbums" // &
        "opihhwlccnxvivutpdjplgoblsnnxftekpyrsilralxncrrvdhxxpyavposlircrrxbiasgfdkgfylak" // &
        "rmjtprikqtnjyixsswhhskxrfjhagghvjqbbhkhfvknxewbprvvkvdlhyohijfgscakitdbjvtkyweie" // &
        "kwrtotbdxobskiydvxqjyawdfwcwwkihnmgjtlvhhrawqdaekreaodmqvhfxqlnhutuosppfvmsfjucw" // &
        "ifgvkhechsoudymonyjxdiqpmxpacwfylyfpswekrmpetahfcyykvvyxovgywwbwixteeojmvmufmnpy" // &
        "nlygrkldvlneidvmhyktcjcfetitqnbfxubtsfawmlbwahnvfjhscntdjuhcoswjhuriyiedmjgpyhol" // &
        "muoshhosplegknticrochkytwyvcqkmmoilwllbkqltgldkibsxfhwdtssqtdkjitlartnjlhxwkgooj" // &
        "aejxgawrnbmlngifnjviwryocingrjviuxmnedgpjmtxysrpraowayorkaauwicmhxtetgcwcqrrqldy" // &
        "xkcupdoolvoqumctqcvnuceigvdamrjkdphuglwqapngspnhssiejfhgrsrijwkxjasffekekahkjsyo" // &
        "dxrvlgapywwligqvyarbtdidoxmogglxtbdrcpdqyoctlilmwkicrppqushhlylgieyvktomayyjxnxb" // &
        "xeaphuxmwwpnugefolewakihsatbljemtwwdgovxbkqmgbuayxdeerbduafnflpsphmycqdpmuxakabg" // &
        "ebdfdjvghxnmeysdchrqjoqawjkibsciscdlgqiuqcdkanslefdnjtkxijdjwppgmrkeydaodpscrmhs" // &
        "tnorasarqrauwgphpfargsigkqsdwagpiqrwwhqcjapcpbqhcxqbpytvvbtcvhbsrddfqemrcwjwjvbl" // &
        "weackbyqukhtoftervtoqqnecxmttfqxwhutrbsleqflmufbqxhceorcithtrdbhcetxgtatuiokxlry" // &
        "wicjlubbwwdwrbytsnuihdqhoedwcoutjtudbhbvuufcibybmpuqtvqchoiwkkakytwlegkqavfwiryf" // &
        "ncupsolrpinqawkalpitrjutxnjqkjgqxamwkmhwyfwjuvpbgddsnrqienfdpesgntnnbxhqdbhnwllt" // &
        "fvrebymryxcbdwjgkkyemgjryxcdhfuncmpfiddvtbifoumgcytarpjseqqbuueimmrdbdrctntroygy" // &
        "xldcktvxpdldrfbqibajosmwmekqdlcalqelxtvgtidvrtuhhcwedgpppakehgmrnjdhbiavkqdtfglq" // &
        "kvawyuvlqbnakmnmwrufrkbkbsuuqagfypllkbcfjtvmpqjbhurubtpkfagnaoohwjrxysqxuaxcjuss" // &
        "bpqucnfrtyoosjubeyyyuoyxnwubfngschethujxiopbxehcuxbmsjhjvovjlsbndhoccgrxdqtvqbfy" // &
        "paxawdiyndxfhuevpjgjcgatgwlxxusdidiorbpewttfvntktwvgbuhbfxwrjfjdklpxcwkcbwwwefsd" // &
        "bvksujpcrkqsfaplvdativyjvuengcnuetluxjkxfhqnvhhlplfderostuoklkkjtqfcpinamdkstuoo" // &
        "aaemsgqwepsgmmmbtlhehtevboplthvnlhkvhhxryjgdqqjwjttdrittfaqpdhikknkjapshpbewwbds" // &
        "jnyvxhmjlvaqssixilesomgjdknsirdewqyrgqhjctupyusxmdvcgdikwsjmkmwhblgvjodjvdajtwhi" // &
        "jmmkgovsnopgewhcdudcybwxdpkppiubsvvrgosnptpmxywnuwpvobsdxofdgihypqrrjsmxadgprwye" // &
        "nrbdtbrrxvyvhcdbgpnvmotrvadrtygdotihxyeiidlcibcadhxedgmnngqnevvdjfepwygttgliernp" // &
        "bkxehtpjdmnyvsprogbucdttqxsyiybfduovhurufqmachigaikiwhrikprkufdykeqiagludpurhesi" // &
        "gemqfyadpesjvvyqqnvdckspjmestcjosuvrggarfpwyxjvdaloysbibdnbigmnkxfdamycwfadrjntx" // &
        "bcoofmkcdvmguehkhcwjpwkgaykbythsqxuucdojkasunqchqjxkhphsresdpudxxxfvoqecavtdxkra" // &
        "ndnexwcelmwlyketxbsfvbyvaypidfkhrbtmkusghxanbouerexuigpnsaqaewiljsbsfkoqsqbsxmwg" // &
        "xuflwnelcywopoeurogcliupipngtrrrniptqhpvkbhowfkdufgyeexapgwvgmdeibsegptngxuijhme" // &
        "xbmmbhelngkaxobydxoetrsmlavwhdqwqywilfueyrpukadlgnnruhhqberrshqjxttjjomgbakjfjfn" // &
        "uddtndboqmtlkrnhjpmrfdmqkfenitihxlafynqbucoykxeospditmmgjuyqrgoralywrgfvwfqyywts" // &
        "acwyokpxybrcxfteyfcjildbbxdhrsdbgvjutpynndcycusgwwjivtuivqbohpxiutvflwifkkpkmkyv" // &
        "isditdkrlxdhfcurmohnavryigvipnjldyheyctkdrbnwnmbxrwlgtjuxiecnrppiibwacwbnayfwsuq" // &
        "kmhtiejglqwmhxfhhulennwmyblvbliavhhxmhmugmhdchnmhcvtxxfadjintfcbhxtsrxkvyaeahmsv" // &
        "tilpjkwrgyeljwfsfgqogahlsnrgunxyrbxcmiybcpdpgxymvenhpelsqcjuuofwrmkqrjkqtuddnnoc" // &
        "trvvwgejixcjhhctopasnadbkeatwbtmfbnvnlgxgkwdgisygwnohslihuelwtwxbbhmowthxgqybtmf" // &
        "xdioepkbhlbicunhysxokxeyqnjgsbskohnenxbabxnqrjqgpygniutqpxfpaojpavtvgtyegkdxpcfy" // &
        "tetobywtwqcasxlwlqgbyigjolhgdvfswmvebhhvahmunav"

    contains

    ! Convert a C hex string to an integer
    integer(8) function c_strtol(string)
       use iso_c_binding
       character(kind=c_char, len=*), intent(in), target :: string
       interface
           integer(c_long) function strtol(str, endptr, base) bind(C,name="strtol")
              use iso_c_binding
              type(c_ptr), intent(in), value :: str ! address of a string
              type(c_ptr), intent(inout) :: endptr
              integer(c_int), intent(in), value :: base
           end function strtol
       end interface

       type(c_ptr) :: strend,str1

       strend = c_null_ptr
       str1   = c_loc(string)
       c_strtol = strtol(str1,strend,base=16_c_int)

    end function c_strtol

    ! Read in results from test data in tiny-regex-c, convert to an array
    subroutine read_test2(fileName)
       use iso_c_binding
       character(*), intent(in) :: fileName

       integer :: iunit,ierr,i,last
       character(len=4) :: line(16)
       character(kind=c_char,len=5) :: forc
       character(kind=RCK,len=:), allocatable :: whole


       open(newunit=iunit,file=fileName,form='formatted',action='read',iostat=ierr)

       allocate(character(len=4*1000000) :: whole)


       last = 0
       do while (.not.is_iostat_end(ierr))
           read(iunit,2,iostat=ierr) line
           do i=1,16
              if (len_trim(line(i))<=0) exit
              forc = line(i)(1:4)//c_null_char
              last = last+1

              ! Now turn to chars
              whole(last:last) = achar(c_strtol(forc))

           end do

       end do

       close(iunit)

       whole = whole(1:last)

       ! Print to output
       open(newunit=iunit,file='testdata.f90',form='formatted',action='write',iostat=ierr)

          write(iunit,1)

          last = 0
          do while (last<len(whole))
             write(iunit,3) whole(last+1:min(last+80,len(whole)))
             last = last+80
          end do



       close(iunit)

       1 format(4x,'character(len=*), parameter :: test2data(*) = [ &')
       2 format(1x,16(1x,a4,1x))
       3 format(8x,'"',a,'" // &')

    end subroutine read_test2

    ! Run test 2

    logical function run_test2() result(success)

       integer, parameter :: NTEST = 10
       integer :: bufsize,i,length,index,bufsizes(NTEST)
       character(kind=RCK) :: saved

       print *
       print *, "Testing pathological pattern '.+nonexisting.+' to force worst-case asymptotic performance:"

       length = 0
       bufsize = len(test2data)

       forall (i=1:NTEST) bufsizes(i) = ceiling(bufsize/2.0**i)

       do i=1,NTEST
           print "('Test ',i0,'/',i0,': text length = ',i0,'....')",i,NTEST,bufsizes(i)
           index = regex(test2data(:bufsizes(i)),".+nonexisting.+",length)
       end do

       success = .true.
       return

    end function run_test2



end module regex_test_2

