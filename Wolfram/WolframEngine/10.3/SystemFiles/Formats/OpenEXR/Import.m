(* ::Package:: *)

Begin["System`Convert`OpenEXRDump`"]

ImportExport`RegisterImport[
 "OpenEXR",
 {
  (********** elements **************)
  "Elements" :> ImportOpenEXRElements,

  (********** attribute elements *********)
  "Attributes"      :> OpenEXRImportAttribute[All],
  "Author"          :> OpenEXRImportAttribute["Author"],
  "ColorSpace"      :> OpenEXRImportAttribute["ColorSpace"],
  "Comments"        :> OpenEXRImportAttribute["Comments"],
  "Compression"     :> OpenEXRImportAttribute["Compression"],
  "CopyrightNotice" :> OpenEXRImportAttribute["CopyrightNotice"],
  "DataWindow"      :> OpenEXRImportAttribute["DataWindow"],
  "DisplayWindow"   :> OpenEXRImportAttribute["DisplayWindow"],
  "ImageChannels"   :> OpenEXRImportAttribute["ImageChannels"],
  "Version"         :> OpenEXRImportAttribute["Version"],
  "ImageSize"       :> OpenEXRImportAttribute["ImageSize"],

  (********** data elements *************)
  "Data"                    :> (ImportOpenEXRData[All][##]&),
  {"Data", channel_String } :> (ImportOpenEXRData[channel][##]&),
  "RGBColorArray"           :> ImportOpenEXRRGBColorArray,

  (****** visualization elements *******)
  "Graphics" :> (ImportOpenEXRSingleItem["Graphics", Automatic][##]&),
  "Image"    :> (ImportOpenEXRSingleItem["Image",    Automatic][##]&),
  {"Graphics", channel_String } :> (ImportOpenEXRSingleItem["Graphics", channel][##]&),
  {"Image",    channel_String } :> (ImportOpenEXRSingleItem["Image",    channel][##]&),
  "GraphicsList" :> (ImportOpenEXRListItems["GraphicsList"][##]&),
  "ImageList"    :> (ImportOpenEXRListItems["ImageList"   ][##]&),

  OpenEXRDefault
 },
 "Sources" -> ImportExport`DefaultSources[{"OpenEXR","DataCommon"}],
 "AvailableElements" -> {"Attributes","Author","ColorSpace","Comments","Compression","CopyrightNotice","Data","DataWindow","DisplayWindow","Graphics","GraphicsList","Image","ImageChannels","ImageList","ImageSize","RGBColorArray","Version" },
 "DefaultElement" -> "Image",
 "Options" -> {"Background", "ColorSpace", "ImageSize" },
 "BinaryFormat" -> True,
 "AlphaChannel" -> True
]

End[]

		
