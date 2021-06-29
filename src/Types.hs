module Types where

import Control.Arrow (first)

import Data.Map qualified as Map
import Data.Set qualified as Set

import Name
import Pretty

data Type
  = Var    Name
  | App    Type Type
  | Const  Name Kind
  | Forall Name Type
  -- | Exists Name
  -- | Rigid  Name
  deriving Show via PP Type

data Kind
  = Star
  | Arrow Polarity Kind Kind
  deriving Show via PP Kind

data Polarity
  = Invariant
  | In
  | Out
  | Phantom
  deriving Show via PP Polarity

instance Pretty Type where
  prettyAtPrec prec = \case
    App (App (Const "->" _) dom) cod -> parensIf (prec <= 7) $ prettyAtPrec 7 dom <+> cKw "->" <+> prettyAtPrec 8 cod
    App f x -> parensIf (prec <= 5) $ prettyAtPrec 5 f <+> prettyAtPrec 6 x
    Forall name ty -> parensIf (prec <= 8) $ cKw "/\\" <.> cRigid (pretty name) <.> cKw "." <.> prettyAtPrec 8 ty
    Const  name kind -> parensIf (prec <= 9) $ cConst (pretty name) <+> cKw ":" <+> pretty kind
    Var    name      -> cVar   (pretty name)
    -- Exists name      -> cExist (pretty name)
    -- Rigid  name      -> cRigid (pretty name)

instance Pretty Kind where
  prettyAtPrec prec = \case
    Arrow pol dom cod -> parensIf (prec <= 0) $ prettyAtPrec 0 dom <+> cKw (pretty pol) <+> prettyAtPrec 1 cod
    Star              -> cConst "*"

instance Pretty Polarity where
  pretty = \case
    Invariant -> "=>"
    In        -> "->"
    Out       -> "+>"
    Phantom   -> "~>"

parensIf :: Bool -> Doc -> Doc
parensIf = makeParensIf (cPunct "(", cPunct ")")

cRigid, cExist, cVar, cConst, cPunct, cArrow, cKw :: Doc -> Doc
cRigid = color magenta
cExist = color cyan
cVar   = color red
cConst = color green
cKw    = color (faint green)
cPunct = color black
cArrow = color (bright green)

arrow :: Type -> Type -> Type
arrow dom cod = App (Const "->" (Arrow In Star (Arrow Out Star Star))) dom `App` cod

class Substitutable n v c | c -> n v where
  apply :: Subst n v -> c -> c
  vars  :: c -> Set.Set n

data Subst n v = Subst { getSubst :: Map.Map n v }
  deriving stock Functor
  deriving Show via PP (Subst n v)

remove :: Ord n => n -> Subst n v -> Subst n v
remove n (Subst s1) = Subst $ Map.delete n s1

add :: (Ord n, Substitutable n v v) => n -> v -> Subst n v -> Subst n v
add n v = apply $ Subst $ Map.singleton n v

instance Pretty n => Pretty1 (Subst n) where
  pretty1 (Subst substs) = braces $ train "," (map (first pretty) (Map.toList substs))

instance (Ord n, Substitutable n v v) => Substitutable n v (Subst n v) where
  apply s1 (Subst s2) = Subst $ Map.map (apply s1) s2 `Map.union` s2
  vars (Subst s1) = foldMap vars s1

instance (Ord n, Substitutable n v a) => Substitutable n v [a] where
  apply = fmap . apply
  vars = foldMap vars

instance Substitutable Name Type Type where
  apply s1 = \case
    Var n -> maybe (Var n) id (Map.lookup n $ getSubst s1)
    Const n k -> Const n k
    -- Exists n -> Exists n
    -- Rigid n -> Rigid n
    App f x -> App (apply s1 f) (apply s1 x)
    Forall n ty -> Forall n (apply (remove n s1) ty)

  vars = \case
    Var n -> Set.singleton n
    App f x -> vars f <> vars x
    Forall n ty -> vars ty `Set.difference` Set.singleton n
    _ -> Set.empty

occurs :: (Substitutable n v a, Ord n) => n -> a -> Bool
occurs n a = n `Set.member` vars a