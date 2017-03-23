module Operations where
import Lam
import NLam

hadamardFalse = TMPlus (TMScalar (1/sqrt(2)) (TMon TFalse)) (TMScalar (1/sqrt(2)) (TMon TTrue))
hadamardTrue = TMPlus (TMScalar (1/sqrt(2)) (TMon TFalse)) (TMScalar ((-1)/sqrt(2)) (TMon TTrue))

pauliYFalse = TMPlus (TMScalar (sqrt(-1)) (TMon TFalse)) (TMScalar (0) (TMon TTrue))
pauliYTrue = TMPlus (TMScalar (0) (TMon TFalse)) (TMScalar (-sqrt(-1)) (TMon TTrue)) 

pauliZFalse = TMPlus (TMScalar (0) (TMon TFalse)) (TMScalar (1) (TMon TTrue))
pauliZTrue = TMPlus (TMScalar (-1) (TMon TFalse)) (TMScalar (0) (TMon TTrue)) 

notFalse = TTrue
notTrue = TFalse 

notOP :: TLam
notOP = Abs 'y' TypeBool (TIf (Var 'y') notTrue notFalse)

pauliYOP :: TLam 
pauliYOP = Abs 'y' TypeBool (TIf (Var 'y') pauliYTrue pauliYFalse)

pauliZOP :: TLam 
pauliZOP = Abs 'y' TypeBool (TIf (Var 'y') pauliZTrue pauliZFalse)

cnotOP :: TLam
cnotOP = Abs 'x' (TypeTuple (TypeBool,TypeBool)) (TIf (TProjTuple (Var 'x') 1) 
                                                    (TMon (TTuple (TTrue, (App notOP (TProjTuple (Var 'x') 2)))))
                                                    (TMon (TTuple (TFalse, (TProjTuple (Var 'x') 2)))))

czOP :: TLam
czOP = Abs 'x' (TypeTuple (TypeBool,TypeBool)) (TIf (TProjTuple (Var 'x') 1)
												  (TIf (TProjTuple (Var 'x') 2)
                                                    (TMScalar (-1) (TMon (TTuple (TTrue,TTrue)))) 
                                                    (TMScalar (1) (TMon (TTuple (TTrue,TFalse)))))
												  (TMScalar (1) (TMon (TTuple (TFalse,(TProjTuple (Var 'x') 2))))))


hadamardOP :: TLam 
hadamardOP = Abs 'x' TypeBool (TIf (Var 'x') 
			 				  (TMPlus (TMScalar (1/sqrt(2)) (TMon TFalse)) 
			 				  		  (TMScalar (1/sqrt(2)) (TMon TTrue))) 
							  (TMPlus (TMScalar (1/sqrt(2)) (TMon TFalse)) 
							  		  (TMScalar ((-1)/sqrt(2)) (TMon TTrue))))

hadamard2OP :: TLam
hadamard2OP = Abs 'x' TypeBool (TLetM 'y' (App hadamardOP (Var 'x')) (App hadamardOP (Var 'y')))

hadamard2OP2 :: TLam
hadamard2OP2 = TLetM 'o' (TMPlus (TMScalar (0.7071067811865475) (TMon TFalse)) 
								 (TMScalar (0.7071067811865475) (TMon TTrue))) 
						 (App hadamard2OP (Var 'o'))

toffoliOP :: TLam
toffoliOP = Abs 'x' (TypeTuple (TypeBool,(TypeTuple (TypeBool,TypeBool))))
						(TIf (TProjTuple (Var 'x') 1)
							(TIf (TProjTuple (Var 'x') 2)
								(TMon (TTuple (TTrue,(TTuple (TTrue, (App notOP (TProjTuple (Var 'x') 3)))))))
								(TMon (TTuple (TTrue,(TTuple (TFalse, (TProjTuple (Var 'x') 3)))))))
						    (TMon (TTuple (TFalse,(TTuple ((TProjTuple (Var 'x') 2), (TProjTuple (Var 'x') 3)))))))

{-cnotOPSuper :: TLam
cnotOPSuper = TAAbs 'x' (TypeTuple (TypeBool,TypeBool)) (TIf (TProjTuple (Var 'x') 1) 
                                                        (TArrow (TMon (TTuple (TTrue, (App notOP (TProjTuple (Var 'x') 2))))))
                                                        (TArrow (TMon (TTuple (TFalse, (TProjTuple (Var 'x') 2))))))
-}
cnotSuper :: TLam
cnotSuper = TAAbs 'j' (TypeTuple (TypeBool,TypeBool)) (TArrow (App cnotOP (Var 'j')))

czSuper :: TLam
czSuper = TAAbs 'j' (TypeTuple (TypeBool,TypeBool)) (TArrow (App czOP (Var 'j')))

hadamardOPSuper :: TLam
hadamardOPSuper = TAAbs 'p' (TypeTuple (TypeBool,TypeBool)) (TArrow (TMon (App hadamardOP (TProjTuple (Var 'p') 1))))
--hadamardOPSuper = TAAbs 'p' (TypeTuple (TypeBool,TypeBool)) (TArrow (TMon (TTuple ((App hadamardOP (TProjTuple (Var 'p') 1)),(TProjTuple (Var 'p') 2)))))

epr :: TLam
epr = TMPlus (TMScalar (1/sqrt(2)) (TMon (TTuple (TFalse,TFalse)))) 
			 (TMScalar (1/sqrt(2)) (TMon (TTuple (TTrue,TTrue))))

epr3 :: TLam
epr3 = TMPlus (TMScalar (1/sqrt(2)) (TMon (TTuple (TFalse,(TTuple (TFalse,TFalse)))))) 
			 (TMScalar (1/sqrt(2)) (TMon (TTuple (TTrue,(TTuple (TTrue,TTrue))))))			 

tupla :: TLam
tupla = TMon (TTuple (TFalse,TTrue))

tuplaTrue :: TLam
tuplaTrue = TMon (TTuple (TTrue,TTrue))

tuplaFalse :: TLam
tuplaFalse = (TTuple (TTrue,TTrue))

triplaFalse :: TLam
triplaFalse = TMon (TTuple (TFalse,(TTuple (TFalse,TFalse))))

triplaTrue :: TLam
triplaTrue = TMon (TTuple (TTrue,(TTuple (TTrue,TTrue))))

triplaEPR :: TLam
triplaEPR =  (TTuple (TFalse,(TTuple (TFalse,TFalse))))

cnotAPP :: TLam
cnotAPP = TLetM 'y' (epr) (App cnotOP (Var 'y'))

czAPP :: TLam
czAPP = TLetM 'y' (tuplaTrue) (App czOP (Var 'y'))

cnotEPRMeas :: TLam
cnotEPRMeas = TALet 'x' (TArrow (epr)) 
					(TALet 'y' (TAApp cnotSuper (Var 'x'))
						(TAApp TMeas (Var 'y')))

hadamardEPRMeas :: TLam
hadamardEPRMeas = TALet 'x' (TArrow (epr)) 
					(TALet 'y' (TAApp hadamardOPSuper (Var 'x'))
						(TAApp TMeas (Var 'y')))					

alice :: TLam
alice = TALet 'x' (TArrow (tuplaTrue)) 
					(TALet 'y' (TAApp cnotSuper (Var 'x'))
						(TALet 'a' (TAApp hadamardOPSuper (Var 'y'))
						   (TAApp TMeas (TTuple((Var 'a'),(TProjTuple (Var 'y') 2))))))

aliceOP :: TLam
aliceOP = TAAbs 'x' (TypeTuple (TypeBool,TypeBool)) 
					(TALet 'y' (TAApp cnotSuper (Var 'x'))
						(TALet 'a' (TAApp hadamardOPSuper (Var 'y'))
						  (TAApp TMeas (TTuple((Var 'a'),(TProjTuple (Var 'y') 2))))))


bob :: TLam
bob = TALet 'x' (TArrow (epr3)) 
					(TALet 'j' (TAApp cnotSuper (TTuple((TProjTuple (Var 'x') 3),(TProjTuple (Var 'x') 1))))
						(TALet 'a' (TAApp cnotSuper (TTuple((TProjTuple (Var 'x') 2),(TProjTuple (Var 'j') 2))))
							(TAApp TTrace (TTuple((TTuple((TProjTuple (Var 'a') 1),(TProjTuple (Var 'j') 1))),
													(TProjTuple (Var 'a') 2))))))				

bobOP :: TLam
bobOP = TAAbs 'x' (TypeTuple (TypeBool,(TypeTuple (TypeBool,TypeBool)))) 
					(TALet 'j' (TAApp cnotSuper (TTuple((TProjTuple (Var 'x') 3),(TProjTuple (Var 'x') 1))))
						(TALet 'a' (TAApp cnotSuper (TTuple((TProjTuple (Var 'x') 2),(TProjTuple (Var 'j') 2))))
							(TAApp TTrace (TTuple((TTuple((TProjTuple (Var 'a') 1),(TProjTuple (Var 'j') 1))),
													(TProjTuple (Var 'a') 2))))))


teleport :: TLam
teleport = TALet 'c' (TArrow (triplaFalse))
					  (TALet 'd' (TAApp aliceOP (TTuple((TProjTuple (Var 'c') 1),(TProjTuple (Var 'c') 3))))
						 --(TArrow (TMon (TTuple ((TProjTuple (Var 'y') 2),(TProjTuple (Var 'y') 2))))))
						 (TAApp bobOP (TTuple((TProjTuple (Var 'c') 2),TTuple((TProjTuple (Var 'd') 1),(TProjTuple (Var 'd') 2))))))

teleport2 :: TLam
teleport2 = TALet 'c' (TArrow (triplaFalse))
					   (TALet 'd' (TAApp aliceOP (TTuple((TProjTuple (Var 'c') 1),(TProjTuple (Var 'c') 3))))
						 --(TArrow (TMon (TTuple ((TProjTuple (Var 'd') 2),(TProjTuple (Var 'd') 2))))))
						  (TAApp bobOP (TTuple((TProjTuple (Var 'c') 2),TTuple((TProjTuple (Var 'd') 1),(TProjTuple (Var 'd') 2))))))
						
						--(TArrow (TMon (Var 'x')))
--						(TArrow (TMon (TTuple((TProjTuple (Var 'x') 1),(TProjTuple (Var 'x') 3))))))


--(TAApp (TAAbs 'z' (TypeComm (TypeDens (TypeVec (TypeTuple (TypeBool,TypeBool))))) (TAApp cnotEPR (TArrow (TArrow (Var 'z')))))) eprSuper)
--(TALet 'y' (TAApp (TAAbs 'z' (TypeComm (TypeDens (TypeVec (TypeTuple (TypeBool,TypeBool))))) (TAApp cnotOPSuper (Var 'z'))) (TArrow (epr))) (TAApp TMeas (Var 'y')))