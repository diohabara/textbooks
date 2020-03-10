type ekimei_t = { 
    kanji   : string; (* 駅名 *) 
    kana    : string; (* 読み *) 
    romaji  : string; (* ローマ字 *) 
    shozoku : string; (* 所属線名 *) 
  } 

type eki_t =  {
    namae : string; (* 駅名 *)
    saitan_kyori : float; (* 最短距離 *)
    temae_list: string list; (* 駅名のリスト *)
  };;

(* 目的 : ekimei_t型のリストと，起点を受け取ったら，saitan_kyoriが0.0，temae_listが始点のeki_t型リストを返す *)
(* make_initial_eki_list : ekimei_t list -> string -> eki_t list *)
let make_initial_eki_list ekimei_list kiten =
  List.map (fun ekimei -> match ekimei with 
                            {kanji = k; kana = kana; romaji = r; shozoku = s} ->
                            if k = kiten 
                            then {namae = kiten; saitan_kyori = 0.0; temae_list = [k]}
                            else {namae = k; saitan_kyori = infinity; temae_list = []}) ekimei_list;;

(* テスト *)
let lst_eki = [{namae="代々木上原"; saitan_kyori = 0.0; temae_list = ["代々木上原"]}; 
               {namae="代々木公園"; saitan_kyori = infinity; temae_list = []}] ;;
let lst_ekimei = [{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"};
                  {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}];;
let test1 = make_initial_eki_list lst_ekimei "代々木上原" = lst_eki;;
