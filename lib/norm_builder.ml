open Norm

let pany ty = PAny ty
let punit = PUnit
let pvar ty name = PVar (ty, name)
let ptuple pl = PTuple pl
let econst c = EConst c
let evar id = EVar id
let eunop op e = EUnOp (op, e)
let ebinop op e1 e2 = EBinOp (op, e1, e2)
let eeither e1 e2 = EEither (e1, e2)
let etuple es = ETuple es
let eif c t e = EIf (c, t, e)
let eapp f args = EApp (f, args)
let efby e1 e2 = EFby (e1, e2)
let enone = ENone
let esome e = ESome e
let elet p e1 e2 = ELet (p, e1, e2)
let step name inp e = Step (name, inp, e)
let package name items = Package (name, items)
