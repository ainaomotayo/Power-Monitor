(* ::Package:: *)

Begin["System`Convert`ExcelDump`"]


ImportExport`RegisterImport[
 "XLS",
{
 "Data" :> ImportExcel["XLS"][False],
 {"Data", "Elements"} :> getDataElements["XLS"],
 {"Data", name_String|name_Integer} :> GetDataSheet["XLS"][name],
 "FormattedData":>ImportFormattedData["XLS"],
 "Formulas" :> ImportExcel["XLS"][True],
 "Images" :> GetImages["XLS"],
 "Sheets" :> JustSheetNames["XLS"],
 {"Sheets", "Elements"} :> SheetNames["XLS"]["Sheets"],
 {"Sheets", name_String|name_Integer} :> GetSheet["XLS"][name],
 "TableView" -> ImportExcelTableView["XLS", All],
 {"TableView", "Elements"} :> ImportExcelTableView["XLS", "Elements"],
 {"TableView", name_String|name_Integer} :> ImportExcelTableView["XLS", name],
 "Elements" ->
   (("Elements"->{"Data", "FormattedData", "Formulas", "Images", "Sheets"})&)
 },
 "Sources" -> Join[{"JLink`"}, ImportExport`DefaultSources["Excel"]],
 "AvailableElements" -> {"Data", "FormattedData",  "Formulas", "Images", "Sheets"},
 "DefaultElement" -> "Data",
 "BinaryFormat" -> True
]


End[]

