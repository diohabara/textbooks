type eki_t =  {
    namae : string; (* 駅名 *)
    saitan_kyori : float; (* 最短距離 *)
    temae_list: string list; (* 駅名のリスト *)
  };;
(* 目的　: eki_t lsit型のリストを受け取ったら、最短距離最小の駅と最短距離最小の駅以外からなるリストをの組を返す *)
(* saitan_wo_bunri : eki_t list -> eki_t * eki_t list *)
let rec saitan_wo_bunri lst = match lst with
    [] -> ({namae = "";saitan_kyori = infinity; temae_list = []}, [])
  | first :: rest ->
     let (p, v) = saitan_wo_bunri rest in
     match(first, p) with
       ({namae = fn; saitan_kyori = fs; temae_list = ft},
        {namae = sn; saitan_kyori = ss; temae_list = st}) ->
       if sn = "" then (first, v)
       else if fs < ss then (first, p :: v)
       else (p, first ::v);;
