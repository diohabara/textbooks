(* ÌÜÅª¡§¥ê¥¹¥ÈÃæ¤ÎÊ¸»úÎó¤ò¤Ä¤Ê¤²¤¿Ê¸»úÎó¤òÊÖ¤¹ *) 
(* concat : string list -> string *) 
let concat lst = List.fold_right (^) lst "" 
 
(* ¥Æ¥¹¥È *) 
let test1 = concat [] = "" 
let test2 = concat ["½Õ"] = "½Õ" 
let test3 = concat ["½Õ"; "²Æ"; "½©"; "Åß"] = "½Õ²Æ½©Åß" 
