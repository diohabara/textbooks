type person_t = {
    name : string ; (* 名前 *)
    chi : string ; (* 血液型 *)
  };;

(* 目的 : person_t型のデータを受け取ったらnamaeさんの血液型はketsueki型ですという文字列を返す *)
(* ketsueki_hyoji : person_t -> string *)
let ketsueki_hyoji person_t = match person_t with
    {name = n; chi = c;} ->
    n ^ "さんの血液型は" ^ c ^ "型です" ;;

(* テスト *)
let test1 = ketsueki_hyoji {name="kadoi"; chi="B"}
              = "kadoiさんの血液型はB型です"
let test2 = ketsueki_hyoji {name="tantan"; chi="O"}
            = "tantanさんの血液型はO型です"
let test3 = ketsueki_hyoji {name="中村"; chi="AB"}
            = "中村さんの血液型はAB型です"
               
