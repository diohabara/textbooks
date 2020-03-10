(* 12 星座を表す型 *) 
type seiza_t = Capricorus	(* 山羊座 *) 
             | Aquarius		(* 水瓶座 *) 
             | Pisces		(* 魚座 *) 
             | Aries		(* 牡羊座 *) 
             | Taurus		(* 牡牛座 *) 
             | Gemini		(* 双子座 *) 
             | Cancer		(* 蟹座 *) 
             | Leo		(* 獅子座 *) 
             | Virgo		(* 乙女座 *) 
             | Libra		(* 天秤座 *) 
             | Scorpius		(* 蠍座 *) 
             | Sagittarius	(* 射手座 *) 
type year_t = January of int 
            | February of int
            | March of int 
            | April of int 
            | May of int 
            | June of int 
            | July of int 
            | August of int
            | September of int 
            | October of int 
            | November of int
            | December of int ;;



(* 目的 : year_t型の値を受け取ったらseiza_t型の星座を返す *)
let seiza year = match year with 
    January (hi) -> if hi <= 19 then Capricorus else Aquarius 
  | February (hi) -> if hi <= 18 then Aquarius else Pisces 
  | March (hi) -> if hi <= 20 then Pisces else Aries 
  | April (hi) -> if hi <= 19 then Aries else Taurus 
  | May (hi) -> if hi <= 20 then Taurus else Gemini 
  | June (hi) -> if hi <= 21 then Gemini else Cancer 
  | July (hi) -> if hi <= 22 then Cancer else Leo 
  | August (hi) -> if hi <= 22 then Leo else Virgo 
  | September (hi) -> if hi <= 22 then Virgo else Libra 
  | October (hi) -> if hi <= 23 then Libra else Scorpius 
  | November (hi) -> if hi <= 21 then Scorpius else Sagittarius 
  | December (hi) -> if hi <= 21 then Sagittarius else Capricorus 

(* テスト *)
let test1 = seiza (September(2)) = Virgo;;
