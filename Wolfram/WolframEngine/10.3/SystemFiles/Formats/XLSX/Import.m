Begin["System`Convert`ExcelDump`"]

ImportExport`RegisterImport[
  "XLSX",
  {
    "Data" :> System`Convert`ExcelDump`ImportExcel["XLSX"][False],
    {"Data", "Elements"} :> System`Convert`ExcelDump`getDataElements["XLSX"],
    {"Data", name_String|name_Integer} :> System`Convert`ExcelDump`GetDataSheet["XLSX"][name],
    "FormattedData":>ImportFormattedData["XLSX"],
    "Formulas" :> System`Convert`ExcelDump`ImportExcel["XLSX"][True],
    "Sheets" :> System`Convert`ExcelDump`JustSheetNames["XLSX"],
    "Images" :> System`Convert`ExcelDump`GetImages["XLSX"],
    {"Sheets", "Elements"} :> System`Convert`ExcelDump`SheetNames["XLSX"]["Sheets"],
    {"Sheets", name_String|name_Integer} :> System`Convert`ExcelDump`GetSheet["XLSX"][name],
    "TableView" -> ImportExcelTableView["XLSX", All],
    {"TableView", "Elements"} :> ImportExcelTableView["XLSX", "Elements"],
    {"TableView", name_String|name_Integer} :> ImportExcelTableView["XLSX", name],
 
    "Elements" ->
     (("Elements"->{"Data", "FormattedData", "Formulas", "Images", "Sheets"})&)
   },
   "Sources" -> Join[{"JLink`"}, ImportExport`DefaultSources["Excel"]],
   "DefaultElement" -> "Data",
   "AvailableElements" -> {"Data", "FormattedData",  "Formulas", "Images", "Sheets"},
   "BinaryFormat" -> True
]

End[]