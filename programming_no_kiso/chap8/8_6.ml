(* 駅の情報(漢字の駅名，ひらがなの駅名，ローマ字の駅名，路線名) *)
type ekimei_t = {
    kanji : string; (* 漢字の駅名 *)
    kana : string; (* ひらがなの駅名 *)
    romaji : string; (* ローマ字の駅名 *)
    shozoku : string; (* その駅が所属する路線名 *)
  };;
                  
(* 目的 : ekimei_t型のデータを受け取ったら路線名，駅名(かな)の形式の文字列を返す *)
(* hyoji : ekimei_t -> string *)
let hyoji ekimei_t = match ekimei_t with
    {kanji = name; kana = subname; romaji = roman; shozoku = rosen} ->
    rosen ^ "，" ^ name ^ "(" ^ subname ^ ")" ;;

(* テスト *)
let test1 = hyoji {kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku="丸ノ内線"}
            = "丸ノ内線，茗荷谷(みょうがだに)";;
let test2 = hyoji {kanji="御茶ノ水"; kana="おちゃのみず"; romaji="ochanomizu"; shozoku="総武線"} =
              "総武線，御茶ノ水(おちゃのみず)";;
