exception InvalidInheritance of string
exception RecursiveInheritance of string

(* Modifiers *)
exception DuplicateModifier of string
exception MultiAccessModifiers of string
exception InvalidModifier of string
exception IllegalModifierCombination of string

exception DuplicateClass of string