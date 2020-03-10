(* ekimei_t型のリストを受け取ったら，それをひらがなの順に整列し，さらに駅名の重複を取り除いたekimei_t型のリストを返す *)
(* seiretsu : ekeimei_t -> ekimei_t *)
let rec seiretsu lst = match lst with
    [] -> []
  | first :: rest -> ekimei_insert (seiretsu rest) first;;

(* 目的 : ローマ字の駅名と駅名リストを受け取り，その駅の漢字表記を返す *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji romaji lst = match lst with
    [] -> ""
  | ({kanji = k; kana = ka; romaji = r; shozoku = s} as first) :: rest
    -> if r = romaji then k
       else romaji_to_kanji romaji rest;;

(* 目的 : ekimei_t型のリストと，起点を受け取ったら，saitan_kyoriが0.0，temae_listが始点のeki_t型リストを返す *)
(* make_initial_eki_list : ekimei_t list -> string -> eki_t list *)
let make_initial_eki_list ekimei_list kiten =
  List.map (fun ekimei -> match ekimei with 
                            {kanji = k; kana = kana; romaji = r; shozoku = s} ->
                            if k = kiten 
                            then {namae = kiten; saitan_kyori = 0.0; temae_list = [k]}
                            else {namae = k; saitan_kyori = infinity; temae_list = []})
    ekimei_list;;

(* 目的 : 漢字の駅名を2つと駅間リスト受け取り，駅間リストの中から2駅間の距離を返す *)
(* get_ekikan_kyori : string -> string -> ekikan_t list -> float *)
let rec get_ekikan_kyori eki1 eki2 lst = match lst with
    [] -> infinity
  | {kiten = k; shuten = s; keiyu = sen; kyori = km; jikan = t} :: rest ->
     if (k = eki1 && s = eki2) || (k = eki2 && s = eki1)
     then km
     else get_ekikan_kyori eki1 eki2 rest;;

(* 目的 : 直前に確定した駅pと未確定の駅のリストv駅間のリスト(ekikan_t list)を受け取ったら，必要な更新処理後に未確定の駅のリストを返す *)
(* koushin : eki_t -> eki_t list -> -> ekikan_t list -> eki_t list *)
let koushin p v ekikan_list = match p with 
  {namae = pn; saitan_kyori = ps; temae_list = pt} -> 
    List.map (fun q -> match q with 
	       {namae = qn; saitan_kyori = qs; temae_list = qt} -> 
		 let kyori = get_ekikan_kyori pn qn ekikan_list in 
		 if kyori = infinity 
		 then q 
		 else if ps +. kyori < qs 
		 then {namae = qn; saitan_kyori = ps +. kyori; 
				   temae_list = qn :: pt} 
		 else q) 
	     v ;;

(* 目的　: eki_t list型のリストを受け取ったら、最短距離最小の駅と最短
   距離最小の駅以外からなるリストの組を返す *)
(* saitan_wo_bunri : eki_t list -> eki_t * eki_t list *)
let saitan_wo_bunri eki_list = match eki_list with 
    [] -> ({namae = ""; saitan_kyori = infinity; temae_list = []}, []) 
  | first :: rest -> 
     List.fold_right (fun first (p, v) -> 
	 match (first, p) with 
	   ({namae = fn; saitan_kyori = fs; temae_list = ft}, 
	    {namae = sn; saitan_kyori = ss; temae_list = st}) -> 
	   if fs < ss then (first, p :: v) 
	   else (p, first :: v)) 
       rest 
       (first, []) 

(* eki_t list型の駅のリストとekikan_t list型の駅間のリストを受け取ったら，ダイクストラのアルゴリズムに従って，各駅について最短距離の最短経路が正しく入ったリスト(eki_t list型))を返す *)
(* dijkstra_main : eki_t list -> ekikan_t list -> eki_t list *)
let rec dijkstra_main eki_lst ekikan_lst = match eki_lst with
    [] -> []
  | first ::rest ->
     let (saitan, nokori) = saitan_wo_bunri (first :: rest) in
     let eki_lst2 = koushin saitan nokori ekikan_lst in
     saitan :: dijkstra_main eki_lst2 ekikan_lst;;

(* 目的：受け取った eki_list から shuten のレコードを探し出す *) 
(* find : string -> eki_t list -> eki_t *) 
let rec find shuten eki_list = match eki_list with 
    [] -> {namae = ""; saitan_kyori = infinity; temae_list = []} 
  | ({namae = n; saitan_kyori = s; temae_list = t} as first) :: rest -> 
      if n = shuten then first else find shuten rest 

(* 始点の駅名（ローマ字の文字列）と終点の駅名（ローマ字の文字列)を受け取り，終点の駅のレコードを返す *)
let dijkstra shiten_roman shuten_roman =
  let ekimei_list = seiretsu global_ekimei_list in
  let shiten = romaji_to_kanji shiten_roman ekimei_list in
  let shuten = romaji_to_kanji shuten_roman ekimei_list in
  let eki_list1 = make_initial_eki_list ekimei_list shiten in
  let eki_list2 = dijksra_main eki_list1 globa_ekikan_list in
  find shuten eki_list2;;
