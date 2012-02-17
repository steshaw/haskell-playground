{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		Type						     *
*	Purpose:	MiniTriangle type representation		     *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									     *
******************************************************************************
-}

-- | MiniTriangle type representation and related definitions.

module Type where

-- | Type for representing MiniTriangle types. Type representations can
-- be compared for equality. Types are only equal if they have the same
-- representation.
data Type = SomeType		-- ^ The smallest type
          | Unit		-- ^ The unit type (return type of procedures)
          | Boolean		-- ^ The Boolean type
          | Integer		-- ^ The Integer type
          | Source Type		-- ^ Read-only variable reference
          | Sink Type		-- ^ Write-only variable reference
          | Ref Type		-- ^ Variable reference
          | Arrow [Type] Type	-- ^ Type of procedures and functions. The
				-- fields represent the argument types the
                                -- and result type. The latter is 'Unit' for
				-- procedures.
          | AllTypes		-- ^ The largest type
          deriving Eq

-- | MiniTriangle Subtyping relation. Predicate that decides whether the
-- type represented by the first argument is a subtype of the type
-- represented by the second. Note: forall T . Sink T <: Sink SomeType
-- and forall T . Source T <: Source AllTypes.
(<:) :: Type -> Type -> Bool
SomeType    <: _           = True
(Ref t1)    <: (Ref t2)    = t1 <: t2 && t2 <: t1
(Ref t1)    <: (Source t2) = (Source t1) <: (Source t2)
(Ref t1)    <: (Sink t2)   = (Sink t1)   <: (Sink t2)
(Source t1) <: (Source t2) = t1          <: t2
(Sink t1)   <: (Sink t2)   = t2          <: t1
_           <: AllTypes    = True
t1          <: t2          = t1 == t2


-- | Predicate that decides if the type is a reference type.
-- isRefType T == T <: Sink SomeType || T <: Source AllTypes
isRefType :: Type -> Bool
isRefType (Ref _)    = True
isRefType (Source _) = True
isRefType (Sink _)   = True
isRefType _          = False


-- | Strips off one layer of referencing.
derefType :: Type -> Type
derefType (Ref t)    = t
derefType (Source t) = t
derefType (Sink t)   = t
derefType _          = SomeType


instance Show Type where
    showsPrec _ SomeType     = showString "SomeType"
    showsPrec _ Unit         = showString "Unit"
    showsPrec _ Boolean      = showString "Boolean"
    showsPrec _ Integer      = showString "Integer"
    showsPrec d (Source t)   = showParen (d >=10)
                                         (showString "Source "
                                          . showsPrec 10 t)
    showsPrec d (Sink t)     = showParen (d >=10)
                                         (showString "Sink " . showsPrec 10 t)
    showsPrec d (Ref t)      = showParen (d >=10)
                                         (showString "Ref " . showsPrec 10 t)
    showsPrec d (Arrow ts t) = showParen (d >= 9)
			     	         (showsTypes ts
                                          . showString " -> "
                                          . showsPrec 9 t)


showsTypes :: [Type] -> ShowS
showsTypes []     = showString "()"
showsTypes (t:ts) = showChar '(' . shows t . stAux ts
    where
        stAux []     = showChar ')'
        stAux (t:ts) = showString ", " . shows t . stAux ts
