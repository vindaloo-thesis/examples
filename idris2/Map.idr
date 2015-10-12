module Map

-- Yes, Idris has SortedMap, but was a nice excercise to build this anyway ^_^

data Map : Type -> Type -> Type where
  Empty : Map k v
  M : Eq k => List (k, v) -> Map k v

get : k -> Map k v -> Maybe v
get k Empty = Nothing
get k (M m) = get' m where
  get' (y::ys) =
    case y of
         (k, val) => Just val
         otherwise => get' ys

put : Eq k => k -> v -> Map k v -> Map k v
put k v Empty = M [(k, v)]
put k v (M m) = M (map (\(k', v') => if k == k' then (k, v) else (k', v')) m)

instance (Show k, Show v) => Show (Map k v) where
  show Empty = ""
  show (M m) = "{"++ show' m ++"}" where
    show' [] = ""
    show' ((k, v) :: Nil) = show k ++ ": " ++ show v
    show' ((k, v) :: xs) = show k ++ ": " ++ show v ++ ", "

