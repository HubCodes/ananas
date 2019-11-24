module Type

type Type =
  | Primitive of Primitive
  | Function1 of Function1
  | TypeVar of string

and Primitive =
  | Int
  | Bool
  | String

and Function1 = Type * Type
