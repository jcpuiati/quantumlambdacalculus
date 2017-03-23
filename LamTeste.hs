module TesteLam where
              

m1 = TMon TTrue

m2 = TMScalar 2 (TMon (TTrue))

m3 = TMPlus (TMScalar 2 (TMon TFalse)) (TMScalar 2 (TMon TTrue))

m4 = TLetM 'x' (TMon TFalse) (Var 'x')

m5 = TMPlus (TVZero) (TMon TFalse)

m6 = TLetM 'x' (TVZero) (Var 'x')

m7 = NMPlus NVZero (NMon NFalse)

m8 = TLetM 'x' (TMon (TFalse)) (NApp (hadamard2 (Var 'x'))

m9 = TMPlus (TMon (TIf TTrue TFalse TFalse)) (TMon (TIf TTrue TFalse TFalse))

m10 = TLetM 'x' (TMScalar 2 (TMon (TIf TTrue TFalse TFalse))) (TMon (Var 'x'))

ex4 = TArrow TTrue

ex5 = TAAbs 'o' TypeBool (TArrow TTrue)

ex6 = TAApp (TAAbs 'o' TypeBool (TArrow TTrue)) TTrue

ex7 = TALet 'p' (TArrow TFalse) (TArrow (Var 'p'))