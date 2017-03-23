{-# LANGUAGE ScopedTypeVariables #-}

module NLam where
import Lam
import Data.Complex
import Data.List
import Data.Maybe
import Data.Char

--Contexto de nomes
--type NContext = [(Char, Int)]
-- Contexto de nomes -- ([Tipos Sem EC], [Tipos Com EC])
type NContext = ([Char],[Char])
type NContext2 = [Char]

--Contexto padrão
contexto = ("","")
contexto2 = ""
--Candidatos a variável ligada
letras = "abcdefghijklmnopqrstuvywz"

--Função que retorna uma varável que não está no contexto
varDisp :: NContext -> [Char] -> Char
varDisp (gamma,delta) c =
   if not (elem (head c) gamma) && not (elem (head c) delta) then
      head c
   else
      varDisp (gamma,delta) (tail c)

--Função removeNames (troca as letras to TLam por índices)
removeNames :: NContext -> TLam -> NLam
removeNames (ctx1,ctx2) (Var c) =
   if elem c (ctx1) then
      NVar (fromJust (elemIndex c (reverse ctx1)))
   else
      if elem c (ctx2) then
        NVar (fromJust (elemIndex c (reverse ctx2)))
  else 
    error "Varíável fora do contexto"

removeNames (ctx1,ctx2) (Abs x _ t) = NAbs (removeNames ((ctx1 ++ [x]),ctx2) t)
removeNames ctx (App t1 t2) = NApp (removeNames ctx t1) (removeNames ctx t2)
removeNames ctx TTrue = NTrue
removeNames ctx TFalse = NFalse
removeNames ctx (TIf t1 t2 t3) = NIf (removeNames ctx t1) (removeNames ctx t2) (removeNames ctx t3)
removeNames ctx TZero = NZero
removeNames ctx (TSucc t1) = NSucc (removeNames ctx t1)
removeNames ctx (TPred t1) = NPred (removeNames ctx t1)
removeNames ctx (TIsZero t1) = NIsZero (removeNames ctx t1)
removeNames ctx TUnit = NUnit
removeNames ctx (TSeq t1 t2) = let t1' = removeNames ctx t1
                                   t2' = removeNames ctx t2 
                                in NSeq t1' t2' 
removeNames (ctx1,ctx2) (TLet c t1 t2) = let ctx = ((ctx1 ++ [c]),ctx2)
                                             t1' = removeNames ctx t1
                                             t2' = removeNames ctx t2
                                  in NLet 0 t1' t2'    
removeNames ctx (TTuple (t1, t2)) = let t1' = removeNames ctx t1
                                        t2' = removeNames ctx t2
                                    in NTuple (t1', t2')
removeNames ctx (TProjTuple (TTuple (t1,t2)) index) = let t1' = removeNames ctx t1
                                                          t2' = removeNames ctx t2
                                                        in NProjTuple (NTuple (t1', t2')) index                                              

removeNames ctx (TProjTuple t index) = let t' = removeNames ctx t 
                                        in NProjTuple t' index 

--- MONADAS
removeNames ctx (TMon t1) = NMon (removeNames ctx t1)
removeNames (ctx1,ctx2) (TLetM c t1 t2) = let ctx = ((ctx1 ++ [c]),ctx2)
                                              t1' = removeNames ctx t1
                                              t2' = removeNames ctx t2
                                          in NLetM 0 t1' t2'
removeNames ctx  TVZero = NVZero
removeNames ctx  (TMPlus t1 t2) = NMPlus (removeNames ctx t1) (removeNames ctx t2)
removeNames ctx  (TMMinus t1 t2) = NMMinus (removeNames ctx t1) (removeNames ctx t2)
removeNames ctx  (TMScalar t1 t2) = NMScalar t1 (removeNames ctx t2)

--- ARROWS
removeNames ctx  (TArrow t1) = NArrow (removeNames ctx t1)
removeNames ctx  TAZero = NAZero
removeNames ctx  (TAPlus t1 t2) = NAPlus (removeNames ctx t1) (removeNames ctx t2)
removeNames ctx  (TAMinus t1 t2) = NAMinus (removeNames ctx t1) (removeNames ctx t2)
removeNames (ctx1,ctx2)  (TAAbs x _ t) = NAAbs (removeNames (ctx1, (ctx2 ++ [x])) t)
removeNames ctx  (TAApp t1 t2) = NAApp (removeNames ctx t1) (removeNames ctx t2)
removeNames (ctx1,ctx2)  (TALet c t1 t2) = let ctx = (ctx1, (ctx2 ++ [c]))
                                               t1' = removeNames ctx t1
                                               t2' = removeNames ctx t2
                                           in NALet 0 t1' t2'
 -- TERMOS MEDIDA
removeNames ctx TMeas = NMeas
removeNames ctx TTrace = NTrace

removeNames2 :: NContext2 -> TLam -> NLam
removeNames2 ctx (Var c) =
   if elem c ctx then
      NVar (fromJust (elemIndex c (reverse ctx)))
   else 
    error "Varíável fora do contexto"

removeNames2 ctx (Abs x _ t) = NAbs (removeNames2 (ctx ++ [x]) t)
removeNames2 ctx (App t1 t2) = NApp (removeNames2 ctx t1) (removeNames2 ctx t2)
removeNames2 ctx TTrue = NTrue
removeNames2 ctx TFalse = NFalse
removeNames2 ctx (TIf t1 t2 t3) = NIf (removeNames2 ctx t1) (removeNames2 ctx t2) (removeNames2 ctx t3)
removeNames2 ctx TZero = NZero
removeNames2 ctx (TSucc t1) = NSucc (removeNames2 ctx t1)
removeNames2 ctx (TPred t1) = NPred (removeNames2 ctx t1)
removeNames2 ctx (TIsZero t1) = NIsZero (removeNames2 ctx t1)
removeNames2 ctx TUnit = NUnit
removeNames2 ctx (TSeq t1 t2) = let t1' = removeNames2 ctx t1
                                    t2' = removeNames2 ctx t2 
                                in NSeq t1' t2' 
removeNames2 ctx (TLet c t1 t2) = let ctx' = (ctx ++ [c])
                                      t1' = removeNames2 ctx' t1
                                      t2' = removeNames2 ctx' t2
                                  in NLet 0 t1' t2'    
removeNames2 ctx (TTuple (t1, t2)) = let t1' = removeNames2 ctx t1
                                         t2' = removeNames2 ctx t2
                                     in NTuple (t1', t2')
removeNames2 ctx (TProjTuple (TTuple (t1,t2)) index) = let t1' = removeNames2 ctx t1
                                                           t2' = removeNames2 ctx t2
                                                       in NProjTuple (NTuple (t1', t2')) index
removeNames2 ctx (TProjTuple t index) = let t' = removeNames2 ctx t 
                                        in NProjTuple t' index                                                                                                     

--- MONADAS
removeNames2 ctx (TMon t1) = NMon (removeNames2 ctx t1)
removeNames2 (ctx) (TLetM c t1 t2) = let ctx' = (ctx ++ [c])
                                         t1' = removeNames2 ctx' t1
                                         t2' = removeNames2 ctx' t2
                                      in NLetM 0 t1' t2'
removeNames2 ctx  TVZero = NVZero
removeNames2 ctx  (TMPlus t1 t2) = NMPlus (removeNames2 ctx t1) (removeNames2 ctx t2)
removeNames2 ctx  (TMMinus t1 t2) = NMMinus (removeNames2 ctx t1) (removeNames2 ctx t2)
removeNames2 ctx  (TMScalar t1 t2) = NMScalar t1 (removeNames2 ctx t2)

--- ARROWS
removeNames2 ctx  (TArrow t1) = NArrow (removeNames2 ctx t1)
removeNames2 ctx  TAZero = NAZero
removeNames2 ctx  (TAPlus t1 t2) = NAPlus (removeNames2 ctx t1) (removeNames2 ctx t2)
removeNames2 ctx  (TAMinus t1 t2) = NAMinus (removeNames2 ctx t1) (removeNames2 ctx t2)
removeNames2 ctx  (TAAbs x _ t) = NAAbs (removeNames2 (ctx ++ [x]) t)
removeNames2 ctx  (TAApp t1 t2) = NAApp (removeNames2 ctx t1) (removeNames2 ctx t2)
removeNames2 ctx  (TALet c t1 t2) = let ctx' = (ctx ++ [c])
                                        t1' = removeNames2 ctx' t1
                                        t2' = removeNames2 ctx' t2
                                    in NALet 0 t1' t2'

 -- TERMOS MEDIDA
removeNames2 ctx TMeas = NMeas
removeNames2 ctx TTrace = NTrace

--Função restoreNames (troca os números por nomes)
restoreNames :: NContext -> NLam -> TLam 
restoreNames (ctx1,ctx2) (NVar n) =
   if (length ctx1) > (n) then
      Var ((reverse ctx1)!!n)
   else
      if (length ctx2) > (n) then
        Var ((reverse ctx2)!!n)
      else
        error "Variável fora do contexto"

restoreNames (ctx1,ctx2)  (NAbs t) =
   let cont = ctx1 ++ [(varDisp (ctx1,ctx2) letras)]
   in Abs ((reverse cont)!!0) TypeBool (restoreNames (cont,ctx2) t) --Ver como recuperar o tipo da abstração
restoreNames ctx  (NApp t1 t2) = App (restoreNames ctx t1) (restoreNames ctx t2)
restoreNames ctx  NTrue = TTrue
restoreNames ctx  NFalse = TFalse
restoreNames ctx  (NIf t1 t2 t3) = TIf (restoreNames ctx t1) (restoreNames ctx t2) (restoreNames ctx t3)
restoreNames ctx  NZero = TZero
restoreNames ctx  (NSucc t1) = TSucc (restoreNames ctx t1)
restoreNames ctx  (NPred t1) = TPred (restoreNames ctx t1)
restoreNames ctx  (NIsZero t1) = TIsZero (restoreNames ctx t1)
restoreNames ctx  NUnit = TUnit
restoreNames ctx  (NSeq t1 t2) = TSeq (restoreNames ctx t1) (restoreNames ctx t2)
restoreNames (ctx1,ctx2)  (NLet c t1 t2) = let letra = varDisp (ctx1,ctx2) letras
                                               ctx1 = (ctx1 ++ [letra]) 
                                               t1' = restoreNames (ctx1,ctx2) t1 
                                               t2' = restoreNames (ctx1,ctx2) t2
                                           in TLet letra t1' t2'   
restoreNames ctx  (NTuple (t1, t2)) = let t1' = restoreNames ctx t1
                                          t2' = restoreNames ctx t2
                                      in TTuple (t1', t2')

--- MONADAS
restoreNames ctx  (NMon t1) = TMon (restoreNames ctx t1)
restoreNames (ctx1,ctx2) (NLetM c t1 t2) = let letra = varDisp (ctx1,ctx2) letras
                                               ctx1 = (ctx1 ++ [letra]) 
                                               t1' = restoreNames (ctx1,ctx2) t1 
                                               t2' = restoreNames (ctx1,ctx2) t2
                                           in TLetM letra t1' t2'                                                                             
restoreNames ctx  NVZero = TVZero
restoreNames ctx  (NMPlus t1 t2) = TMPlus (restoreNames ctx t1) (restoreNames ctx t2)
restoreNames ctx  (NMMinus t1 t2) = TMMinus (restoreNames ctx t1) (restoreNames ctx t2)
restoreNames ctx  (NMScalar t1 t2) = TMScalar t1 (restoreNames ctx t2)

--- ARROWS

restoreNames ctx  (NArrow t1) = TArrow (restoreNames ctx t1)
restoreNames ctx  NAZero = TAZero
restoreNames ctx  (NAPlus t1 t2) = TAPlus (restoreNames ctx t1) (restoreNames ctx t2)
restoreNames ctx  (NAMinus t1 t2) = TAMinus (restoreNames ctx t1) (restoreNames ctx t2)
restoreNames (ctx1,ctx2)  (NAAbs t) = let cont = ctx2 ++ [(varDisp (ctx1,ctx2) letras)]
                                      in TAAbs ((reverse cont)!!0) TypeBool (restoreNames (ctx1,cont) t) --Ver como recuperar o tipo da abstração
restoreNames ctx  (NAApp t1 t2) = TAApp (restoreNames ctx t1) (restoreNames ctx t2)
restoreNames (ctx1,ctx2) (NALet c t1 t2) = let letra = varDisp (ctx1,ctx2) letras
                                               ctx2 = (ctx2 ++ [letra]) 
                                               t1' = restoreNames (ctx1,ctx2) t1 
                                               t2' = restoreNames (ctx1,ctx2) t2
                                           in TALet letra t1' t2'

 -- TERMOS MEDIDA
restoreNames ctx NMeas = TMeas
restoreNames ctx NTrace = TTrace

--Função shifting
shifting :: (Int, Int) -> NLam -> NLam
shifting (d, c) (NVar k) =
   if k >= c then
      NVar (k + d)
   else
      NVar k
shifting (d, c) (NAbs t) = NAbs (shifting (d, c+1) t)

shifting (d, c) (NMon t) = NMon (shifting (d, c) t)
shifting (d, c) (NArrow t) = NArrow (shifting (d, c) t)

shifting (d, c) (NApp t1 t2) = NApp (shifting (d, c) t1) (shifting (d, c) t2)
shifting (d, c) (NProjTuple t i) = NProjTuple (shifting (d, c) t) i
shifting (d, c) (NIf t1 t2 t3) = NIf (shifting (d, c) t1) (shifting (d, c) t2) (shifting (d, c) t3)
shifting (d, c) (NTuple (t1,t2)) = NTuple ((shifting (d, c) t1),(shifting (d, c) t2))
shifting (d, c) (NAAbs t) = NAAbs (shifting (d, c+1) t)
shifting (d, c) (NAApp t1 t2) = NAApp (shifting (d, c) t1) (shifting (d, c) t2)
shifting (d, c) t = t


--Função substituição
--         j     s       k
--j = Índice a ser substituído
--s = Termo que substitui o índice (j)
--k = Termo onde será aplicada a substituição   subsNL (0, (shifting (1, 0) v2)) (t)
subsNL :: (Int, NLam) -> NLam -> NLam
subsNL (j, s) (NVar k) =
   if k == j then
      s
   else
		   NVar k       
subsNL (j, s) (NIf t1 t2 t3) = NIf (subsNL (j, s) t1) (subsNL (j, s) t2) (subsNL (j, s) t3)
subsNL (j, s) (NLet t t1 t2) = NLet t (subsNL (j+1, (shifting (1, 0) s)) t1) (subsNL (j+1, (shifting (1, 0) s)) t2)
subsNL (j, s) (NLetM t t1 t2) = NLetM t (subsNL (j+1, (shifting (1, 0) s)) t1) (subsNL (j+1, (shifting (1, 0) s)) t2)
subsNL (j, s) (NALet t t1 t2) = NALet t (subsNL (j+1, (shifting (1, 0) s)) t1) (subsNL (j+1, (shifting (1, 0) s)) t2)

subsNL (j, s) (NMon t) = NMon (subsNL ((j), s) t)
subsNL (j, s) (NArrow t) = NArrow (subsNL ((j), s) t)

subsNL (j, s) (NAAbs t) = NAAbs (subsNL ((j+1), (shifting (1, 0) s)) t)
subsNL (j, s) (NAApp t1 t2) = NAApp (subsNL (j, s) t1) (subsNL (j, s) t2)
subsNL (j, s) (NAbs t) = NAbs (subsNL ((j+1), (shifting (1, 0) s)) t)
subsNL (j, s) (NApp t1 t2) = NApp (subsNL (j, s) t1) (subsNL (j, s) t2)
subsNL (j, s) (NTuple (t1,t2)) = NTuple (subsNL (j, s) t1,subsNL (j, s) t2)
subsNL (j, s) (NProjTuple t i) = NProjTuple (subsNL (j, s) t) i
subsNL (j, s) (NMScalar v t) = NMScalar v (subsNL ((j), s) t)
subsNL (j, s) t = t

--Função que retorna se uma expressão é um valor
--Valor: um termo em sua forma normal (primitivo)
isValNL :: NLam -> Bool
isValNL (NVar k) = True
isValNL (NAbs t) = True
isValNL NTrue  = True
isValNL (NMon NTrue) = True
isValNL NFalse = True
isValNL (NMon NFalse) = True
isValNL NUnit = True
isValNL (NMon (NTuple (t1,t2))) = if isValNL t1 then
                             isValNL t2
                           else
                             False
isValNL (NTuple (t1,t2)) = if isValNL t1 then
                             isValNL t2
                           else
                             False
isValNL (NMPlus t1 t2) = if isValNL t1 then
                             isValNL t2
                           else
                             False                                                          
isValNL (NMMinus t1 t2) = if isValNL t1 then
                             isValNL t2
                           else
                             False
isValNL (NMScalar x t2) = if isValNL t2 then
                             True
                           else
                             False
isValNL t = isNumber2 t 

--Função que retorna se uma expressão é uma mônada
isMonNL :: NLam -> Bool
isMonNL (NMon t) = True
isMonNL (NMPlus t1 t2) = True
isMonNL (NMScalar t1 t2) = True
isMonNL _ = False

--Função que verificar se um NLam é um tipo True ou False
--isBool :: NLam -> Bool
--isBool NTrue  = True
--isBool NFalse = True
--isBool t12 = False

--Função para saber se valor é um número
isNumber2 :: NLam -> Bool
isNumber2 NZero = True
isNumber2 (NSucc NZero) = True
isNumber2 (NSucc t) = isNumber2 t
isNumber2 _ = False

-- Função que verifica se o termo é um arrow
isArrowNL :: NLam -> Bool
isArrowNL (NArrow a) = True
isArrowNL (NAAbs a) = True
isArrowNL (NAApp a b) = True
isArrowNL _ = False


--Função que retorna o NLam do índice da projeção da tupla
findProjTupleByIndex :: NLam -> NLam 
findProjTupleByIndex (NProjTuple (NTuple (t1,t2)) index) = if index == 1 then
                                                             t1
                                                           else 
                                                             findProjTupleByIndex (NProjTuple t2 (index-1))
findProjTupleByIndex (NProjTuple t index) = if index == 1 then
                                              t
                                            else
                                              error "O índice da projeção não indica nenhum elemento da tupla!"  
findProjTupleByIndex _ = error "O parâmetro não é uma projeção de NTuple"                

--Função que chama a função de avaliação recursivamente
interpretNLam :: NLam -> NLam
interpretNLam t = let t' = evalCBVNL t
                  in if t' == t then t'
                     else interpretNLam t'    

--Função de avaliação - Call By Value
evalCBVNL :: NLam -> NLam
evalCBVNL (NVar k) = NVar k
evalCBVNL (NAbs t) = NAbs t
evalCBVNL NTrue = NTrue
evalCBVNL NFalse = NFalse
evalCBVNL NZero = NZero
evalCBVNL NUnit = NUnit
evalCBVNL (NApp (NAbs t) v2) =
   if isValNL v2 then
      shifting (-1, 0) (subsNL (0, (shifting (1, 0) v2)) (t)) --EAPPABS
   else --Caso o v2 não seja um valor, então ele é uma Aplicação
      let t2 = evalCBVNL v2
      in NApp (NAbs t) t2

evalCBVNL (NApp t1 t2) =
   if (not (isValNL t1)) then
      let t1' = evalCBVNL t1 --EAPP1
      in (NApp t1' t2)
   else
      let t2' = (evalCBVNL t2) --EAPP2 (Igual ao else da EAPPABS)
      in (NApp t1 t2')

evalCBVNL (NIf (NMPlus a b) t2 t3) =
   if (not (isValNL a) || not (isValNL b)) then
      let t1' = evalCBVNL (NMPlus a b)
      in NIf t1' t2 t3
   else NMPlus (timesSFalse (NMPlus a b) t2) (timesSTrue (NMPlus a b) t3)
--   else NMPlus (timesSFalse (NMPlus a b) t2) (timesSTrue (NMPlus a b) t3)

evalCBVNL (NIf t1 t2 t3) =
   if (t1 == NTrue || t1 == NMon NTrue) then
      t2 --E-IFTRUE
   else if (t1 == NFalse || t1 == NMon NTrue) then
      t3 --E-IFFALSE
   else --E-IF
      let t1' = evalCBVNL t1
      in (NIf t1' t2 t3)

evalCBVNL (NPred NZero) = NZero -- E-PREDZERO
evalCBVNL (NPred (NSucc t1)) =
   if (isNumber2 t1) then
      t1 --E-PREDSUCC
   else (NPred (NSucc (evalCBVNL t1))) 

evalCBVNL (NPred t1) = NPred (evalCBVNL t1) --E-PRED
evalCBVNL (NSucc t1) = NSucc (evalCBVNL t1)  --E-SUCC
evalCBVNL (NIsZero NZero) = NTrue --E-ISZEROT
evalCBVNL (NIsZero (NSucc nv1)) = NFalse --E-ISZEROSUCC
evalCBVNL (NIsZero t1) = (NIsZero (evalCBVNL t1)) --E-ISZERO

evalCBVNL (NSeq t1 t2) = let t1' = evalCBVNL t1
                         in if t1' == NUnit then
                           evalCBVNL t2 --E-SEQNEXT
                         else
                           (NSeq t1' t2) --E-SEQ 
                           
evalCBVNL (NLet n t1 t2) = if isValNL t1 then
                             subsNL (n, t1) t2  -- E-LETV
                           else
                             NLet n (evalCBVNL(t1)) t2 -- E-LET

evalCBVNL (NTuple (t1,t2)) = if isValNL t1 then
                               NTuple(t1, evalCBVNL t2)
                             else
                               let t1' = evalCBVNL t1
                               in NTuple(t1', t2)                             
evalCBVNL (NProjTuple (NTuple (t1,t2)) index) = if isValNL (NTuple (t1,t2)) then
                                                   findProjTupleByIndex (NProjTuple (NTuple (t1,t2)) index) --E-PROJTUPLE
                                                else --E-PROJ e E-TUPLE
                                                  let tupleEvaluated = evalCBVNL (NTuple (t1,t2))
                                                  in (NProjTuple tupleEvaluated index)
-- AVALIAÇÃO DE MÔNADAS
evalCBVNL (NMon a) = NMon (evalCBVNL a)

evalCBVNL (NLetM n (NMon t1) t2) = if isMonNL t1 then evalCBVNL (NLetM n t1 t2)
                                   else if isValNL t1 then subsNL (n, t1) t2 else (NLetM n (NMon (evalCBVNL t1)) t2) -- LEFTM
evalCBVNL (NLetM x NVZero y) = NVZero -- LEFTVZERO
evalCBVNL (NLetM y (NLetM x l n) t) = NLetM x l (NLetM y n t)-- ASSOCm
evalCBVNL (NLetM x (NMPlus a b) t) = NMPlus (NLetM x a t) (NLetM x b t)-- LEFT+M
evalCBVNL (NLetM x (NMMinus a b) t) = NMMinus (NLetM x a t) (NLetM x b t)-- LEFT-M
evalCBVNL (NLetM x (NMScalar a m) t) = NMScalar a (NLetM x m t) -- LEFT*M
evalCBVNL (NLetM x t1 (NMon y)) = if isValNL t1 then t1 else NLetM x (evalCBVNL t1) (NMon y)-- LEFTM t1 -- RIGHTM
evalCBVNL (NLetM n t1 t2) = if isValNL(t1) then 
                                subsNL (n, t1) t2 
                            else NLetM n (evalCBVNL t1) (t2)-- LEFTM

evalCBVNL NVZero = NVZero -- VZERO


-- MEDIDA ARROW
evalCBVNL (NMPlus (NArrow a) (NArrow b)) = NAPlus (NArrow a) (NArrow b)

-- Monadas
evalCBVNL (NMPlus (NVZero) (a)) = a -- VZERO+
evalCBVNL (NMPlus (a) (NVZero)) = a -- VZERO+

--evalCBVNL (NMPlus (NMPlus a b) (NMPlus x y)) = (simplificationP (evalCBVNL (NMPlus a b)) (evalCBVNL (NMPlus x y))) 
--evalCBVNL (NMPlus (NMPlus a b) (NMMinus x y)) = (simplificationP (evalCBVNL (NMPlus a b)) (evalCBVNL (NMMinus x y))) 
evalCBVNL (NMPlus a (NMPlus b c)) = (NMPlus (NMPlus a b) c)  -- VZERO+  
{-}
--Simplicação
evalCBVNL (NMPlus (NMScalar a b) (NMScalar c d)) = if a == (0)
                                                    then NMScalar c (evalCBVNL d)
                                                    else if c == (0) 
                                                          then NMScalar a (evalCBVNL b)
                                                          else NMPlus (evalCBVNL b) (evalCBVNL d) -- VZERO+

-}
evalCBVNL (NMPlus a b) = NMPlus (evalCBVNL a) (evalCBVNL b) -- VZERO+

{-}
-- MEDIDA ARROW
evalCBVNL (NMMinus (NArrow a) (NArrow b)) = NAMinus (NArrow a) (NArrow b)

--evalCBVNL (NMMinus (NVZero) (a)) = (-a) -- VZERO-
evalCBVNL (NMMinus (a) (NVZero)) = a -- VZERO-

evalCBVNL (NMMinus (NMPlus a b) (NMMinus x y)) = (simplificationM (evalCBVNL (NMPlus a b)) (evalCBVNL (NMMinus x y))) 
evalCBVNL (NMMinus a b) = NMMinus (evalCBVNL a) (evalCBVNL b) -- VZERO+
 -}                         
{-evalCBVNL (NMScalar a (NMPlus b c)) = NMPlus (multiplicaScalarPlus a b) 
                                             (multiplicaScalarPlus a c)
evalCBVNL (NMScalar a (NMMinus b c)) = NMMinus (multiplicaScalarPlus a b) 
                                               (multiplicaScalarMinus a c)
-}

-- ARROW SCALAR
evalCBVNL (NMScalar a (NAApp NMeas c)) = if isValNL c 
                                          then (NMScalar a (NArrow (NMon c))) 
                                           else NMScalar a (NAApp NMeas (evalCBVNL c))


--MONADA SCALAR
evalCBVNL (NMScalar a m) = NMScalar a (evalCBVNL m)-- LEFT*M


---- -- AVALIAÇÃO DE ARROWS

--Caso for um scalar
evalCBVNL (NArrow (NMScalar v a)) = if v == 1
                                    then (NArrow (evalCBVNL a))
                                    else (NMScalar v (NArrow (evalCBVNL a)))
evalCBVNL (NArrow a) = NArrow (evalCBVNL a)

evalCBVNL (NAAbs t) = NAAbs t

evalCBVNL NAZero = NAZero -- VZERO

evalCBVNL (NALet n (NArrow t1) t2) = NLetM n t1 t2 -- LEFT
evalCBVNL (NALet x t1 (NArrow t2)) = t1 -- RIGHTM
evalCBVNL (NALet x NAZero y) = NAZero -- LEFTVZERO
evalCBVNL (NALet y (NALet x l n) t) = NALet x l (NALet y n t)-- ASSOCm


evalCBVNL (NALet x (NAPlus a b) t) = NAPlus (NALet x a t) (NALet x b t)-- LEFT+M
evalCBVNL (NALet x (NAMinus a b) t) = NAMinus (NALet x a t) (NALet x b t)-- LEFT-M

evalCBVNL (NALet n t1 t2) = if (isValNL(t1)) then 
                                subsNL (n, t1) t2 
                            else NALet n (evalCBVNL t1) (t2)-- LEFTM


evalCBVNL (NAApp (NAAbs t) v2) =
   if (isArrowNL v2 || isValNL v2) then
      shifting (-1, 0) (subsNL (0, (shifting (1, 0) v2)) (t)) --BETA
   else --Caso o v2 não seja um valor, então ele é uma Aplicação
      let t2 = evalCBVNL v2
      in NAApp (NAAbs t) t2

evalCBVNL (NAApp NMeas (NMPlus t1 t2)) =
      let t1' = (evalCBVNL t1) 
          t2' = (evalCBVNL t2) 
      in (NArrow (NAPlus (t1') (t2')))

evalCBVNL (NAApp NMeas t2) =
      let t2' = (evalCBVNL t2) --EAPP2 (Igual ao else da EAPPABS)
      in (NArrow t2')

evalCBVNL (NAApp NTrace (NTuple (t1,t2))) =
      let t1' = (evalCBVNL t1) 
          t2' = (evalCBVNL t2) 
      in (NArrow (t2'))

evalCBVNL (NAApp NTrace t2) =
      let t2' = (evalCBVNL t2) --EAPP2 (Igual ao else da EAPPABS)
      in (NArrow t2')


evalCBVNL (NAApp t1 t2) =
   if (not (isValNL t1)) then
      let t1' = evalCBVNL t1 --EAPP1
      in (NAApp t1' t2)
   else
      let t2' = (evalCBVNL t2) --EAPP2 (Igual ao else da EAPPABS)
      in (NAApp t1 t2')


--- ARROWS PLUS

evalCBVNL (NAPlus (NAZero) (a)) = a -- VZERO+
evalCBVNL (NAPlus (a) (NAZero)) = a -- VZERO+
evalCBVNL (NAPlus a (NAPlus b c)) = (NAPlus (NAPlus a b) c)  -- VZERO+  

evalCBVNL (NAPlus a b) = NAPlus (evalCBVNL a) (evalCBVNL b) -- VZERO+

evalCBVNL (NAMinus (a) (NAZero)) = a -- VZERO-
evalCBVNL (NAMinus (NAZero) (a)) = a -- VZERO+
evalCBVNL (NAMinus a (NAMinus b c)) = (NAMinus (NAMinus a b) c)  -- VZERO+  

evalCBVNL (NAMinus a b) = NAMinus (evalCBVNL a) (evalCBVNL b) -- VZERO+



simplificationP :: NLam -> NLam
simplificationP (NMPlus (NMScalar 0 b) (NMScalar x y)) = NMScalar x y
simplificationP (NMPlus (NMScalar a b) (NMScalar 0 y)) = NMScalar a b 
simplificationP (NMPlus (NMScalar a b) (NMScalar x y)) = if (b == y) 
                                                         then NMScalar (a + x) b 
                                                         else error "aaaa"
--simplificationP (NMPlus (NMPlus a b) (NMPlus x y)) = NMPlus (simplificationP a x) (simplificationP b y)
--simplificationP (NMPlus a (NMPlus b c)) (NMPlus x (NMPlus y z)) = NMPlus (simplificationP (NMPlus b c)) (simplificationP (NMPlus y z))
simplificationP a = a

{-
simplificationM :: NLam -> NLam -> NLam
simplificationM (NMScalar a b) (NMScalar x y) = if (b == y) 
                                                then NMScalar (a - x) b 
                                                else error "bb"
simplificationM (NMPlus a b) (NMMinus x y) = NMPlus (simplificationM a x) (simplificationM b y)
simplificationM a b = NMMinus a b
-}

multiplicaScalarPlus :: Complex Double -> NLam -> NLam
multiplicaScalarPlus t (NMScalar a b) = NMScalar (t * a) b
--multiplicaScalarPlus t (NMPlus a b) = (multiplicaScalarPlus t a) + (multiplicaScalarPlus t b)
--multiplicaScalarPlus t (NMMinus a b) = (multiplicaScalarPlus t a) - (multiplicaScalarMinus t b)

multiplicaScalarMinus :: Complex Double -> NLam -> NLam
multiplicaScalarMinus t (NMScalar a b) = NMScalar (t * (-a)) b
--multiplicaScalarMinus t (NMPlus a b) = (multiplicaScalarPlus t a) - (multiplicaScalarMinus t b)
--multiplicaScalarMinus t (NMMinus a b) = (multiplicaScalarMinus t a) + (multiplicaScalarMinus t b)

timesSTrue :: NLam -> NLam -> NLam
timesSTrue (NMScalar a b) (NMPlus c d) = NMScalar ((timesS a c) + (timesS a d)) b
timesSTrue (NMPlus a b) (c) = timesSTrue a c 

timesSFalse :: NLam -> NLam -> NLam
timesSFalse (NMScalar a b) (NMPlus c d) = NMScalar ((timesS a c) + (timesS a d)) b
timesSFalse (NMPlus a b) (c) = timesSFalse b c 


timesS :: Complex Double -> NLam -> Complex Double
timesS (a) (NMScalar c d) = a * c

--testeEval1 = TLetM 'x' (TMon TFalse) (Var 'x')
--testeEval2 = TMPlus (TVZero) (TMon TFalse)
--testeNVZero = TLetM 'x' (TVZero) (Var 'x')
--testeEval21 = NMPlus NVZero (NMon NFalse)

--t007 = TLetM 'x' (TMon (TFalse)) (NApp (hadamard2 (Var 'x'))

--testeEval3 = TMPlus (TMon (TIf TTrue TFalse TFalse)) (TMon (TIf TTrue TFalse TFalse))
--testeEval4 = TLetM 'x' (TMScalar 2 (TMon (TIf TTrue TFalse TFalse))) (TMon (Var 'x'))

