module Type

type Type =
  | Int
  | Bool
  | String
  | Function1 of Type * Type
  | TypeVar of string
