#use "17_14.ml";;
#use "../chap15/15_5.ml";;
#use "../chap9/9_9.ml";;

(* objective : make eki list from ekimei list *)
(* make_initial_eki_list : ekimei_t list -> string -> eki_t list *) 
let make_initial_eki_list ekimei_list kiten = 
  List.map (fun ekimei -> match ekimei with 
	                    {kanji = k; kana = a; romaji = r; shozoku = s} -> 
	                    if k = kiten 
	                    then {namae = k; saitan_kyori = 0.; temae_list = [k]} 
	                    else {namae = k; saitan_kyori = infinity; temae_list = []}) 
    ekimei_list;; 
 
(* objective : return renewed v, uncetain eki_t list *)
(* koushin : eki_t -> eki_t list -> ekikan_tree_t -> eki_t list *) 
let koushin p v ekikan_tree = match p with 
    {namae = pn; saitan_kyori = ps; temae_list = pt} -> 
    List.map (fun q -> match q with 
	                 {namae = qn; saitan_kyori = qs; temae_list = qt} -> 
		         let kyori = get_ekikan_kyori pn qn ekikan_tree in 
		         if kyori = infinity 
		         then q 
		         else if ps +. kyori < qs 
		         then {namae = qn; saitan_kyori = ps +. kyori; 
			       temae_list = qn :: pt} 
		         else q) 
      v;;
 
(* objective : from uncertain list and ekikan_t list, return the shortest distnace *)
(* dijkstra_main : eki_t list -> ekikan_tree_t -> eki_t list *) 
let rec dijkstra_main eki_list ekikan_tree = match eki_list with 
    [] -> [] 
  | first :: rest -> 
      let (saitan, nokori) = saitan_wo_bunri (first :: rest) in 
      let eki_list2 = koushin saitan nokori ekikan_tree in 
      saitan :: dijkstra_main eki_list2 ekikan_tree
 
(* objective : find shuten's record from given eki_list *)
(* find : string -> eki_t list -> eki_t *) 
let rec find shuten eki_list = match eki_list with 
    [] -> {namae = ""; saitan_kyori = infinity; temae_list = []} 
  | ({namae = n; saitan_kyori = s; temae_list = t} as first) :: rest -> 
      if n = shuten then first else find shuten rest 
 
(* objective : given shiten and shuten, get shortest distance nad return shtuen record *)
(* dijkstra : string -> string -> eki_t *) 
let dijkstra romaji_kiten romaji_shuten = 
  let kiten = romaji_to_kanji romaji_kiten global_ekimei_list in 
  let shuten = romaji_to_kanji romaji_shuten global_ekimei_list in 
  let eki_list = make_initial_eki_list global_ekimei_list kiten in 
  let global_ekikan_tree = inserts_ekikan Empty global_ekikan_list in 
  let eki_list2 = dijkstra_main eki_list global_ekikan_tree in 
  find shuten eki_list2 
 
(* test *)
let test1 = dijkstra "shibuya" "gokokuji" = 
  {namae = "護国寺"; saitan_kyori = 9.8; 
   temae_list = 
     ["護国寺"; "江戸川橋"; "飯田橋"; "市ヶ谷"; "麹町"; "永田町"; 
      "青山一丁目"; "表参道"; "渋谷"]} 
let test2 = dijkstra "myogadani" "meguro" = 
  {namae = "目黒"; saitan_kyori = 12.7000000000000028; 
   temae_list = 
     ["目黒"; "白金台"; "白金高輪"; "麻布十番"; "六本木一丁目"; "溜池山王"; 
      "永田町"; "麹町"; "市ヶ谷"; "飯田橋"; "後楽園"; "茗荷谷"]} 
