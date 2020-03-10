(* ¤¢¤é¤«¤¸¤á ex12_1.ml ¤òÆÉ¤ß¹ş¤ó¤Ç¤ª¤¯É¬Í×¤¢¤ê *) 
#use "ex12_1.ml" 
 
(* ±Ø¥ê¥¹¥È¤ÎÎã *) 
 
let eki_list = [ 
{namae="ÃÓÂŞ"; saitan_kyori = infinity; temae_list = []}; 
{namae="¿·ÂçÄÍ"; saitan_kyori = infinity; temae_list = []}; 
{namae="è¬²ÙÃ«"; saitan_kyori = infinity; temae_list = []}; 
{namae="¸å³Ú±à"; saitan_kyori = infinity; temae_list = []}; 
{namae="ËÜ¶¿»°ÃúÌÜ"; saitan_kyori = infinity; temae_list = []}; 
{namae="¸æÃã¥Î¿å"; saitan_kyori = infinity; temae_list = []} 
] 
 
(* ÌÜÅª¡§ekimei list ¤«¤é eki list ¤òºî¤ë *) 
(* shokika : eki_t list -> string -> eki_t list *) 
let rec shokika eki_list kiten = match eki_list with 
    [] -> [] 
  | ({namae = k; saitan_kyori = s; temae_list = t} as first) :: rest -> 
      if k = kiten 
      then {namae = k; saitan_kyori = 0.; temae_list = [k]} 
	   :: shokika rest kiten 
      else first :: shokika rest kiten 
 
(* ¥Æ¥¹¥È *) 
let test1 = shokika [] "è¬²ÙÃ«" = [] 
let test2 = shokika eki_list "è¬²ÙÃ«" = [ 
{namae="ÃÓÂŞ"; saitan_kyori = infinity; temae_list = []}; 
{namae="¿·ÂçÄÍ"; saitan_kyori = infinity; temae_list = []}; 
{namae="è¬²ÙÃ«"; saitan_kyori = 0.; temae_list = ["è¬²ÙÃ«"]}; 
{namae="¸å³Ú±à"; saitan_kyori = infinity; temae_list = []}; 
{namae="ËÜ¶¿»°ÃúÌÜ"; saitan_kyori = infinity; temae_list = []}; 
{namae="¸æÃã¥Î¿å"; saitan_kyori = infinity; temae_list = []} 
] 
