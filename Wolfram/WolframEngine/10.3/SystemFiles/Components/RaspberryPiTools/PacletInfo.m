(* ::Package:: *)

(* Paclet Info File *)

(* created 2015/06/05*)

Paclet[
    Name -> "RaspberryPiTools",
    Version -> "1.0.0",
    MathematicaVersion -> "10+",
    Creator -> "Brett Haines <bhaines@wolfram.com>",
    Extensions -> {
	{"LibraryLink",SystemID->"Linux-ARM"},
        {"Kernel", Root->"Kernel", Context->{"RaspberryPiTools`"} },
        {"Documentation", MainPage->"ReferencePages/Devices/AstroPi", Language->"English"},
        {"Resource", Root->"Resources", Resources->{{"TextImage", "Bitmap/astro_pi_text.png"}} }
    }
]
