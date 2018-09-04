(* ::Package:: *)

Begin["System`Convert`PackageDump`"]


ImportExport`RegisterImport[
 "Package",
 {
  "Get" -> ImportLoad,
  "Comments"-> ImportComments,
  ImportPackage
 },
 {
  "ExpressionList" :> GetExpressions,
  {"HeldExpressions", i_Integer?Positive} :> GetHeldExpression[i],
  {"ExpressionList", i_Integer?Positive} :> GetExpression[i],
  {"InactivatedExpressions", i_} :> GetInactivatedExpression[i],
  {"HeldExpressions"|"InactivatedExpressions"|"ExpressionList", "Elements"} :> CountExpressions,
  "InactivatedExpressions" :> GetInactivatedExpressions
 },
 "FunctionChannels" -> {"FileNames"},
 "AvailableElements" -> {"Comments", "ExpressionList", "Get", "HeldExpressions", "InactivatedExpressions"},
 "DefaultElement" -> "Get",
 "Options" -> {"Comments"}
]


End[]
