(* ::Package:: *)

Begin["System`Convert`FITSDump`"]


ImportExport`RegisterImport[
 "FITS",
 {
  (* Raw Importers *)
  "Data":>ImportFITSProcessedData,
  "Graphics":> ImportFITSGraphics,
  "Image" :> ImportFITSImage,
  "Elements" :> ImportFITSElements,
  "RawData"|"Range"|"TableData"|"Plaintext" :> (ImportFITS[#1,##2]&),
  "Metadata" :> ImportFITSMetadata["Metadata"],
  ImportFITSMetadata[All]
 },
 "AvailableElements" -> {
		"Airmass", "Author", "BitDepth", "ColorSpace", "Comments", "Data",
		"DataType", "Declination", "Device", "Equinox", "ExposureTime",
		"Graphics", "History", "HourAngle", "Image", "ImageSize", "Metadata",
		"Object", "ObservationDate", "Observer", "Organization", "Plaintext",
		"Range", "RawData", "Reference", "RightAscension", "SiderealTime",
		"TableData", "TableHeaders", "TableUnits", "Telescope"},
 "DefaultElement" -> "Image",
 (*"Sources" -> ImportExport`DefaultSources[{"Table", "FITS"}],*)
 "BinaryFormat" -> True
]


End[]
