(* ::Package:: *)

Begin["System`Convert`TeXImportDump`"]


ImportExport`RegisterImport[
  "LaTeX",
  ImportTeXElements,
  {
	"Notebook" :> ElementsToNotebook,
	"NotebookObject" :> ElementsToNotebookObject
  },
  "Sources" -> ImportExport`DefaultSources["TeXImport"],
  "AvailableElements" -> {"Notebook", "NotebookObject"},
  "DefaultElement" -> "Notebook"
]


End[]
