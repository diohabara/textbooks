#use "../chap9/9_9.ml";;
#use "../chap18/18_6.ml";;

(* obj : make romaji name kanji *)
(* romaji_to_kanji : string -> ekimei list -> string *)
let rec romaji_to_kanji r0 ekimei_lsit = match ekimei_list with
    [] -> raise (No_such_station (r0))
  | {kanji=k;kana=a;romaji=r;shozoku=s} :: rest ->
     if r0 = r then k else romaji_to_kanji r0 rest;;

(* テスト *) 
let test1 = romaji_to_kanji "myogadani" global_ekimei_list = "茗荷谷" 
let test2 = romaji_to_kanji "shibuya" global_ekimei_list = "渋谷" 
let test3 = romaji_to_kanji "otemachi" global_ekimei_list = "大手町" 
(* let test4 = romaji_to_kanji "osaka" global_ekimei_list *) 
   (* No_such_station "osaka" を起こす *) 
