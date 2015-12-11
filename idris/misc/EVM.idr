module EVM

data EVM_Types : Type -> Type where
  EVM_Str : EVM_Types String

FFI_EVM : FFI
FFI_EVM = MkFFI EVM_Types String String

EVM : Type -> Type
EVM = IO' FFI_EVM
