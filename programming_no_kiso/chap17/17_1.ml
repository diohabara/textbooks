(* 年号を表す型 *)
type nengou_t = Meiji of int (* 明治 *)
              | Taisho of int (* 大正 *)
              | Showa of int (* 昭和 *) 
              | Heisei of int (* 平成 *);;

(* 目的:年号を受け取ったら対応する西暦年を返す *)
(* to_seireki : nengou_t -> int *)
let to_seireki nengou = match nengou with
    Meiji (n) -> n + 1867
  | Taisho (n) -> n + 1911
  | Showa (n) -> n + 1925
  | Heisei (n) -> n + 1988;;

(* 目的 : 誕生日と現在の年をnengou_tで受け取ったら年利絵を返す *)
(* nenrei : nengou_t nengou_t -> int *)
let nenrei umare ima = 
  let toshi1 = to_seireki umare in
  let toshi2 = to_seireki ima in
  toshi2 - toshi1 ;;

(* テスト *)
let test1 = nenrei (Heisei (9)) (Heisei (31)) = 22;;
let test2 = nenrei (Showa (55)) (Heisei (31)) = 39;;
