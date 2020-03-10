#use "../chap9/09_10.ml";; (* ekikan_t, global_ekikan_list の定義 *) 
#use "../chap12/12_1.ml";; (* eki_t の定義 *) 
#use "../chap1717_16.ml";; (* saitan_wo_bunri の定義 *) 
#use "../chap1818_7.ml";; (* romaji_to_kanji の定義 *) 
 
open RedBlack 
(* open Tree *) (* Tree モジュールを使う場合はこちらを使用する *) 
 
(* 目的：受け取った kiten, shuten, kyori を ekikan_tree に挿入した木を返す *) 
(* insert1 : (string * (string * float) list) t -> 
	      string -> string -> float -> 
	     (string * (string * float) list) t *) 
let rec insert1 ekikan_tree kiten shuten kyori = 
  let lst = try 
	      search ekikan_tree kiten 
	    with Not_found -> [] 
  in insert ekikan_tree kiten ((shuten, kyori) :: lst) 
 
(* 目的：受け取った ekikan 情報を ekikan_tree に挿入した木を返す *) 
(* insert_ekikan : (string * (string * float) list) t -> 
		    ekikan_t -> 
		   (string * (string * float) list) t *) 
let insert_ekikan ekikan_tree ekikan = match ekikan with 
  {kiten = k; shuten = s; keiyu = y; kyori = r; jikan = j} -> 
    insert1 (insert1 ekikan_tree s k r) k s r 
 
(* 目的：受け取った ekikan のリストを ekikan_tree に挿入した木を返す *) 
(* inserts_ekikan : (string * (string * float) list) t -> 
		     ekikan_t list -> 
		    (string * (string * float) list) t *) 
let inserts_ekikan ekikan_tree ekikan_list = 
  List.fold_left insert_ekikan ekikan_tree ekikan_list 
 
(* 目的：ふたつの駅の間の距離を求める *) 
(* 見つからなかったら例外 Not_found を起こす *) 
(* get_ekikan_kyori : string -> string -> 
		     (string * (string * float) list) t -> float *) 
let rec get_ekikan_kyori eki1 eki2 tree = 
  List.assoc eki2 (search tree eki1) 
 
(* 目的：ekimei list から eki list を作る *) 
(* make_initial_eki_list : ekimei_t list -> string -> eki_t list *) 
let make_initial_eki_list ekimei_list kiten = 
  List.map (fun ekimei -> match ekimei with 
	     {kanji = k; kana = a; romaji = r; shozoku = s} -> 
	       if k = kiten 
	       then {namae = k; saitan_kyori = 0.; temae_list = [k]} 
	       else {namae = k; saitan_kyori = infinity; temae_list = []}) 
	   ekimei_list 
 
(* 目的：未確定の駅のリスト v を必要に応じて更新したリストを返す *) 
(* koushin : eki_t -> eki_t list -> ekikan_tree_t -> eki_t list *) 
let koushin p v ekikan_tree = match p with 
  {namae = pn; saitan_kyori = ps; temae_list = pt} -> 
    List.map (fun q -> match q with 
	       {namae = qn; saitan_kyori = qs; temae_list = qt} -> 
		 try 
		   let kyori = get_ekikan_kyori pn qn ekikan_tree in 
		   if ps +. kyori < qs 
		   then {namae = qn; saitan_kyori = ps +. kyori; 
				     temae_list = qn :: pt} 
		   else q 
		 with Not_found -> q) 
	     v 
 
(* 目的：未確定駅のリストと駅間リストから、各駅への最短路を求める *) 
(* dijkstra_main : eki_t list -> ekikan_tree_t -> eki_t list *) 
let rec dijkstra_main eki_list ekikan_tree = match eki_list with 
    [] -> [] 
  | first :: rest -> 
      let (saitan, nokori) = saitan_wo_bunri first rest in 
      let eki_list2 = koushin saitan nokori ekikan_tree in 
      saitan :: dijkstra_main eki_list2 ekikan_tree 
 
(* 目的：受け取った eki_list から shuten のレコードを探し出す *) 
(* find : string -> eki_t list -> eki_t *) 
let rec find shuten eki_list = match eki_list with 
    [] -> {namae = ""; saitan_kyori = infinity; temae_list = []} 
  | ({namae = n; saitan_kyori = s; temae_list = t} as first) :: rest -> 
      if n = shuten then first else find shuten rest 
 
(* 目的：始点と終点を受け取ったら、最短路を求め、終点のレコードを返す *) 
(* dijkstra : string -> string -> eki_t *) 
let dijkstra romaji_kiten romaji_shuten = 
  let kiten = romaji_to_kanji romaji_kiten global_ekimei_list in 
  let shuten = romaji_to_kanji romaji_shuten global_ekimei_list in 
  let eki_list = make_initial_eki_list global_ekimei_list kiten in 
  let global_ekikan_tree = inserts_ekikan empty global_ekikan_list in 
  let eki_list2 = dijkstra_main eki_list global_ekikan_tree in 
  find shuten eki_list2 
 
(* 目的：eki_t 型のレコードをきれいに表示する *) 
(* print_eki : eki_t -> unit *) 
let print_eki eki = match eki with 
  {namae = n; saitan_kyori = s; temae_list = lst} -> match lst with 
      [] -> assert false (* この場合は起こりえない *) 
    | [a] -> print_string (a ^ "（" ^ string_of_float s ^ "km）"); 
	     print_newline () 
    | a :: rest -> List.fold_right (fun b () -> print_string (b ^ "、")) 
				   rest (); 
		   print_string (a ^ "（" ^ string_of_float s ^ "km）"); 
		   print_newline () 
 
(* メイン関数 *) 
(* main : string -> string -> unit *) 
let main romaji_kiten romaji_shuten = 
  let eki = dijkstra romaji_kiten romaji_shuten in 
  print_eki eki 
 
(* テスト *) 
let test1 = main "shibuya" "gokokuji" = () 
(* 渋谷、表参道、青山一丁目、永田町、麹町、市ヶ谷、飯田橋、江戸川橋、 
   護国寺（9.8km）と表示される *) 
let test2 = main "myogadani" "meguro" = () 
(* 茗荷谷、後楽園、飯田橋、市ヶ谷、麹町、永田町、溜池山王、六本木一丁目、 
   麻布十番、白金高輪、白金台、目黒（12.7km）と表示される *) 
