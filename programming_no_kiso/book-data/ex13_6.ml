(* ¤¢¤é¤«¤¸¤á ex10_11.ml, ex12_1.ml ¤òÆÉ¤ß¹þ¤ó¤Ç¤ª¤¯É¬Í×¤¢¤ê *) 
#use "ex10_11.ml" 
#use "ex12_1.ml" 
 
(* ±Ø¤ÎÎã *) 
let eki1 = {namae="ÃÓÂÞ"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="¿·ÂçÄÍ"; saitan_kyori = 1.2; temae_list = ["¿·ÂçÄÍ"; "è¬²ÙÃ«"]} 
let eki3 = {namae="è¬²ÙÃ«"; saitan_kyori = 0.; temae_list = ["è¬²ÙÃ«"]} 
let eki4 = {namae="¸å³Ú±à"; saitan_kyori = infinity; temae_list = []} 
 
(* ÌÜÅª¡§Ì¤³ÎÄê¤Î±Ø q ¤òÉ¬Í×¤Ë±þ¤¸¤Æ¹¹¿·¤·¤¿±Ø¤òÊÖ¤¹ *) 
(* koushin1 : eki_t -> eki_t -> eki_t *) 
let koushin1 p q = match (p, q) with 
  ({namae = pn; saitan_kyori = ps; temae_list = pt}, 
   {namae = qn; saitan_kyori = qs; temae_list = qt}) -> 
    let kyori = get_ekikan_kyori pn qn global_ekikan_list in 
    if kyori = infinity 
    then q 
    else if ps +. kyori < qs 
    then {namae = qn; saitan_kyori = ps +. kyori; temae_list = qn :: pt} 
    else q 
 
(* ¥Æ¥¹¥È *) 
let test1 = koushin1 eki3 eki1 = eki1 
let test2 = koushin1 eki3 eki2 = eki2 
let test3 = koushin1 eki3 eki3 = eki3 
let test4 = koushin1 eki3 eki4 = 
	{namae="¸å³Ú±à"; saitan_kyori = 1.8; temae_list = ["¸å³Ú±à"; "è¬²ÙÃ«"]} 
let test5 = koushin1 eki2 eki1 = 
	{namae="ÃÓÂÞ"; saitan_kyori = 3.0; temae_list = ["ÃÓÂÞ"; "¿·ÂçÄÍ"; "è¬²ÙÃ«"]} 
let test6 = koushin1 eki2 eki2 = eki2 
let test7 = koushin1 eki2 eki3 = eki3 
let test8 = koushin1 eki2 eki4 = eki4 
