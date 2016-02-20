
-- expression grammar
type Identifier = String
type ArgList = [Identifier]
type ParamList = [Expr]
data Expr = 
    Let Identifier Expr Expr
    | Fun ArgList Expr
    | SimpleExpr
data SimpleExpr = 
    Expr 
    | Identifier 
    | FuncCall SimpleExpr ParamList

-- type grammar
type TyList = [Ty]
data Ty = 
    FNoArgs Ty
    | FUncurry TyList Ty
    | FGeneric ArgList Ty
    | FNormal SimplTy Ty
    | BareTy SimplTy
data SimplTy = 
      TTy Ty
    | TIdent Identifier
    | TList SimplTy TyList

main = do
    print "not implemented"
