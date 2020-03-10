#use "../chap10/10_10.ml";;
(* 目的　: eki_t list型のリストを受け取ったら、最短距離最小の駅と最短距離最小の駅以外からなるリストの組を返す *)
(* saitan_wo_bunri : eki_t list -> eki_t * eki_t list *)
let rec saitan_wo_bunri lst =
  List.fold_right (fun first (p, v) ->
      match (first, p) with
        ({namae=fn;saitan_kyori=fs;temae_list=ft},
         {namae=sn;saitan_kyori=ss;temae_list=st})->
        if sn = "" then (first, v)
        else if fs < ss then (first, p :: v)
        else (p, first :: v))
    lst
    ({namae="";saitan_kyori=infinity;temae_list=[]}, []);;
