module LamType where
import Data.Char
import Lam
import NLam
import Operations
import TesteLam
import Debug.Trace

--Definição de um tipo para o contexto usado no type checker
--type TContext = [(Char,Type)]
-- Definição do contexto de tipos como uma tupla de duas listas de tuplas
type TContext = ([(Char,Type)],[(Char,Type)])
--type TArrowContext = [(Char,Type)]

contextoTipo = ([('j', (TypeTuple (TypeBool,TypeBool))), ('a', (TypeTuple (TypeBool,TypeBool))), ('y', TypeBool), ('b', TypeBool), ('c', TypeBool), ('d', TypeVec TypeBool)],
                [('b', TypeBool), ('j', TypeBool), ('d', TypeVec TypeBool)])

teste1 = TSeq (TUnit) (TIf TTrue TFalse TFalse)
teste2 = TSeq (Abs 'x' TypeUnit (TIf TTrue TFalse TFalse)) (TIf TTrue TFalse TFalse)
teste3 = App (TSeq TUnit (Abs 'a' TypeBool (Var 'a'))) (TSeq TUnit (Var 'b'))
teste4 = TMPlus (TMon (Var 'b')) (TMon (Var 'c'))

--Testa se nao é erro
isNotError :: Type -> Bool
isNotError (TypeErr str) = False
isNotError _             = True

-- Adiciona ao contexto de tipos valores monádicos
addType :: TContext -> Char -> Type -> TContext
addType (gamma,delta) char type1 = (gamma ++ [(char,type1)],delta)

-- Adiciona ao contexto valores com arrows types - efeito colateral
addArrowType :: TContext -> Char -> Type -> TContext
addArrowType (gamma,delta) char type1 = (gamma, delta ++ [(char,type1)])

--Encontra o tipo da variavel no contexto
findType :: Char -> TContext -> Type
findType x ([],[]) = TypeErr "Variavel fora do contexto"
findType x ([],head:tail) = if x == fst (head)
                            then snd (head)
                            else findType x ([],tail)
findType x (head:tail, arrow) = if x == fst (head)
                                then snd (head)
                                else findType x (tail,arrow)

isFun :: Type -> Bool
isFun (TypeFunc a b) = True
isFun _              = False 

isTuple :: TLam -> Bool
isTuple (TTuple _) = True
isTuple _ = False

isTyTuple :: Type -> Bool
isTyTuple (TypeTuple _) = True
isTyTuple _ = False

isMonad :: Type -> Bool
isMonad (TypeVec _) = True
isMonad (TypeFunc _ (TypeVec _)) = True
isMonad (TypeFunc _ (TypeFunc a b)) = isMonad (TypeFunc a b)
isMonad _ = False

removeFromMonad :: Type -> Type
removeFromMonad (TypeVec a) = a
removeFromMonad (TypeFunc _ (TypeFunc a b)) = removeFromMonad (TypeFunc a b)
removeFromMonad (TypeFunc _ (TypeVec b)) = b
removeFromMonad (TypeTuple (a,b)) = removeFromMonad a
removeFromMonad _ = TypeErr "O primeiro termo deve ser do Tipo Vec"

isSuper :: Type -> Bool
isSuper (TypeSuper _ _ ) = True
isSuper _ = False

getTy2Super :: Type -> Type
getTy2Super (TypeSuper ty1 ty2) = ty2

isCommand :: Type -> Bool
isCommand (TypeComm _) = True
isCommand _ = False


removeFromArrow :: Type -> Type
removeFromArrow (TypeComm (TypeDens a)) = a
removeFromArrow (TypeSuper _ (TypeFunc a b)) = removeFromMonad (TypeFunc a b)
removeFromArrow (TypeSuper _ (TypeComm (TypeDens a))) = a
removeFromArrow (TypeTuple (a,b)) = removeFromArrow a
removeFromArrow _ = TypeErr "O primeiro termo deve ser do tipo Command"

isTVZero :: Type -> Type -> Bool
isTVZero (TypeVec TypeNat) b = True
isTVZero a (TypeVec TypeNat) = True
isTVZero _ _ = False

isntTVZero :: Type -> Type -> Type
isntTVZero (TypeVec TypeNat) b = b
isntTVZero a (TypeVec TypeNat) = a
isntTVZero _ _ = TypeErr "Erro de tipo"

isTAZero :: Type -> Type -> Bool
isTAZero (TypeComm (TypeDens TypeNat)) b = True
isTAZero a (TypeComm (TypeDens TypeNat)) = True
isTAZero _ _ = False

isntTAZero :: Type -> Type -> Type
isntTAZero (TypeComm (TypeDens TypeNat)) b = b
isntTAZero a (TypeComm (TypeDens TypeNat)) = a
isntTAZero _ _ = TypeErr "Erro de tipo"

typeMatch :: Type -> Type -> Bool
typeMatch (TypeSuper a b) (TypeSuper d e) = if b == e then True else False
typeMatch (TypeSuper a b) (e) = if b == e then True else False
typeMatch (b) (TypeSuper d e) = if b == e then True else False
typeMatch (TypeFunc a b) (TypeFunc d e) = if b == e then True else False
typeMatch (TypeFunc a b) (e) = if b == e then True else False
typeMatch (b) (TypeFunc d e) = if b == e then True else False
typeMatch (a) (e) = typeMatch a e


--Valida o tipo do argumento e retorna o tipo de retorno da função 
validaArgumento :: Type -> Type -> Type
validaArgumento (TypeFunc tyT1 tyT1') tyT2 = if (tyT1 == tyT2) 
                                               then tyT1' 
                                            else TypeErr "Argumento recebido UAUASUAHSIA nao eh do tipo esperado pela funcao"
                           
--Função que verifica se um termo possui um tipo diferente de erro                            
isWellTyped :: Type -> Bool
isWellTyped (TypeErr str) = error str
isWellTyped (TypeTuple (TypeErr str,b)) = error str
isWellTyped (TypeTuple (a,TypeErr str)) = error str
isWellTyped (TypeTuple (a,b)) = isWellTyped b
isWellTyped _             = True

findTypeProj :: Type -> Int -> Type
findTypeProj (TypeTuple (a,b)) index
    | index == 0 = TypeErr "O índice deve ser maior que 0"
    | index == 1 = a
    | index == 2 = b
    | index > 2  = findTypeProj b (index-1)
    | otherwise  = TypeErr "O índice deve ser maior que 0"
 
{-typeMatch :: Type -> Type -> Bool
typeMatch (a) (b)
    | a == b-}

--Função que retorna o tipo de um termo
typeOf :: TContext -> TLam -> Type
typeOf ctx TTrue = TypeBool
typeOf ctx TFalse = TypeBool
typeOf ctx TZero = TypeNat 
typeOf ctx TUnit = TypeUnit
typeOf ctx (TIf t1 t2 t3) = if (((typeOf ctx t1) == TypeBool) || (isTyTuple (typeOf ctx t1))) 
                               then let tyT2 = typeOf ctx t2
                                        tyT3 = typeOf ctx t3
                                    in if ((isNotError tyT2) && (isNotError tyT3)) then tyT2
                                     else TypeErr "Os possveis retornos possuem tipos diferentes ---"
                            else TypeErr "A guarda nao eh do tipo TypeBool"
typeOf ctx (TSucc t1) = if ((typeOf ctx t1) == TypeNat)
                           then TypeNat
                        else TypeErr "O argumento de TSucc nao eh do tipo TypeNat"                                   
typeOf ctx (TPred t1) = if ((typeOf ctx t1) == TypeNat)
                           then TypeNat
                        else TypeErr "O argumento de TPred nao eh do tipo TypeNat"                                  
typeOf ctx (TIsZero t1) = if ((typeOf ctx t1) == TypeNat)
                           then TypeBool
                        else TypeErr "O argumento de TIsZero nao eh do tipo TypeNat"                                   
typeOf ctx (TSeq t1 t2) = let tyT1 = typeOf ctx t1
                              tyT2 = typeOf ctx t2
                          in
                             if ( tyT1 == TypeUnit)
                                then tyT2
                             else
                                TypeErr "O primeiro argumento nao eh do tipo Unit"
typeOf ctx (TLet x t1 t2) = let tyT1 = typeOf ctx t1
                                ctx' = addType ctx x tyT1
                                tyT2  = typeOf ctx' t2
                            in tyT2 
typeOf ctx (Var char) = findType char ctx 
typeOf ctx (Abs char tyT1 t2) = let ctx' = addType ctx char tyT1 
                                    tyT2 = typeOf ctx' t2 
                                in if (isNotError tyT2) 
                                      then TypeFunc tyT1 tyT2
                                   else tyT2 --Retorna o erro 
typeOf ctx (App t1 t2) = let tyT1 = typeOf ctx t1
                             tyT2 = typeOf ctx t2
                         in if (isNotError tyT1) 
                               then if (isFun tyT1)
                                       then if (isNotError tyT2)
                                               then tyT1                               
                                            else tyT2 --Retorna o erro
                                    else TypeErr "O primeiro termo da aplicacao nao eh uma funcao/abstracao"          
                            else tyT1 --Retorna o erro
typeOf ctx (TTuple (t1, t2)) = let tyT1 = typeOf ctx t1
                               in TypeTuple (tyT1, typeOf ctx t2)

typeOf ctx (TProjTuple (Var char) index) = if isTyTuple (findType char ctx)  then
                                                findTypeProj (findType char ctx) index
                                           else TypeErr "O argumento da projecao deve ser uma tupla"

typeOf ctx (TProjTuple (TTuple (t1,t2)) index) = if index <= 0 then
                                                   TypeErr "O índice da projecao deve ser maior que 0" 
                                                 else if index == 1 then
												 					                  typeOf ctx t1
												                         else if index == 2 then
												                            if isTuple t2 then
												                              typeOf ctx (TProjTuple t2 (index-1))
												                            else
												                              typeOf ctx t2  
												                            else
												                              if isTuple t2 then
												               	                typeOf ctx (TProjTuple t2 (index-1))
												                              else
												               	                TypeErr "O indice da projecao e maior que o numero de elementos"       


-- MÔNADAS                                                                    
typeOf ctx (TMon t1) = TypeVec (typeOf ctx t1)

typeOf ctx TVZero = TypeVec TypeNat -- QUAL O RETORNO DO TYPEVEC PRO TVZERO

typeOf ctx (TLetM x t1 t2) 
        = let tyT1 = typeOf ctx t1
              tyT1' = removeFromMonad (tyT1)
              ctx' = addType ctx x tyT1'
              tyT2  = typeOf ctx' t2
          in if (isNotError tyT1')
             then if (isNotError tyT2)
                  then if (isMonad tyT2) 
                       then tyT2 
                       else TypeErr "O segundo termo da aplicacao deve ser do Tipo Monad"                           
                  else tyT2 --Retorna o erro
          else tyT1' --Retorna o erro

typeOf ctx (TMPlus t1 t2) = 
        let a = typeOf ctx t1
            b = typeOf ctx t2
        in if ((isMonad a) && (isMonad b)) 
           then if (a == b) 
                 then a 
                 else if (isTVZero a b)
                      then (isntTVZero a b) 
                      else TypeErr "Os--- argumentos nao sao do mesmo tipo"
           else TypeErr "Os argumentos de TMPlus nao são do tipo TypeVec"

typeOf ctx (TMMinus t1 t2) = let a = typeOf ctx t1
                                 b = typeOf ctx t2
                             in if ((isMonad a) && (isMonad b))
                                then if (a == b) 
                                     then a 
                                     else if (isTVZero a b)
                                          then (isntTVZero a b) 
                                          else TypeErr "Os--- argumentos nao sao do mesmo tipo"
                                else TypeErr "Os argumentos de TMPlus nao são do tipo TypeVec"

typeOf ctx (TMScalar alfa t1) = 
        let b = typeOf ctx t1
        in if (isMonad b) 
           then b 
           else TypeErr "The snd term ins't monadic type"

---- ARROW TYPES
typeOf ctx (TArrow t1) = let tyT1 = typeOf ctx t1
                         in if (isMonad tyT1)
                            then TypeComm (TypeDens tyT1)
                            else TypeErr "The Arrow type doesn't have a Vec type"

{-typeOf ctx (TArrow t1) = let tyT1 = typeOf ctx t1
                         in if (isMonad tyT1)
                            then TypeComm (TypeDens tyT1)
                            else TypeErr "O tipo Arrow deve conter um tipo TypeVec"                            -}

typeOf ctx TAZero = TypeComm (TypeDens TypeNat) -- QUAL O RETORNO DO TYPEVEC PRO TVZERO
typeOf ctx (TAAbs char tyT1 t2) = 
    let ctx' = addArrowType ctx char tyT1 
        tyT2 = typeOf ctx' t2 
    in if (isNotError tyT2)
       then if (isCommand tyT2)
            then TypeSuper tyT1 tyT2
            else TypeErr "The second term of abstraction should be a Command type"
       else tyT2 

typeOf ctx (TAApp (TMeas) t) = 
    let tyT = typeOf ctx t
    in if (isNotError tyT)
       then TypeComm (TypeDens (tyT))                               
       else tyT --Retorna o erro          
                                --else TypeComm (TypeDens (tyT2)) --Retorna o erro

typeOf ctx (TAApp TTrace (TTuple(t1,t2))) = let tyT2 = typeOf ctx t2
                                           in if (isNotError tyT2)
                                              then TypeComm (TypeDens (tyT2))
                                              else tyT2 --Retorna o erro          
                                --else TypeComm (TypeDens (tyT2)) --Retorna o erro                                

typeOf ctx (TAApp t1 t2) = 
      let tyT1 = typeOf ctx t1
          tyT2 = typeOf ctx t2
      in if (isNotError tyT1) 
         then if (isSuper tyT1)
              then if (isNotError tyT2)
                   then (getTy2Super tyT1)                               
                   else tyT2 --Retorna o erro
              else TypeErr "O first term doesn't a Super type"          
      else tyT1

typeOf ctx (TALet x t1 t2) = 
     let tyT1 = typeOf ctx t1
         tyT1' = removeFromArrow (tyT1)
         ctx' = addArrowType ctx x tyT1'
         tyT2 = typeOf ctx' t2
     in if (isNotError tyT1')
        then if (isNotError tyT2)
             then if (isCommand tyT2) 
                  then tyT2 
                  else TypeErr "The second term sgould be a Command type"                              
             else tyT2
        else tyT1' 

typeOf ctx (TAPlus t1 t2) = 
        let a = typeOf ctx t1
            b = typeOf ctx t2
        in if ((isCommand a) && (isCommand b)) 
           then if (a == b) 
                then a 
                else if (isTAZero a b)
                     then (isntTAZero a b) 
                     else TypeErr "The terms aren't of the same type"
           else TypeErr "The terms should be a Command type"

typeOf ctx (TAMinus t1 t2) = let a = typeOf ctx t1
                                 b = typeOf ctx t2
                             in if ((isCommand a) && (isCommand b)) 
                                then if (a == b) 
                                     then a 
                                     else if (isTAZero a b)
                                          then (isntTAZero a b) 
                                          else TypeErr "The terms aren't of the same type"
                                else TypeErr "The terms should be a Command type"      


--Função interpret () com tipos
--Função que chama a função de avaliação recursivamente
interpret :: TLam -> TLam
interpret t = if (isWellTyped (typeOf contextoTipo t))
				      then let tN = removeNames2 contexto2 t
                  in restoreNames contexto (interpretNLam tN)
              else error "Erro na validacao de tipos" 