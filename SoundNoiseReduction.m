(* ::Package:: *)

BeginPackage["SoundNoiseReduction`"];

GetSoundData::usage = "GetSoundData[sound] returns number representation of the sound"

GetSoundRate::usage = "GetSoundRate[sound] returns sample rate of the sound"

PlotSound::usage = "PlotSound[sound] plots the data of the sound"

AddWhiteNoise::usage = "AddWhiteNoise[sound] add some noise to the sound "

Begin["`Private`"]


GetSoundData[sound_Sound] := sound[[ 1, 1, 1 ]] 

GetSoundRate[sound_Sound] := sound[[ 1, 2 ]]

PlotSound[sound_Sound]:= ListPlot[ GetSoundData[sound] ]

AddWhiteNoise[sound_Sound]:= 
Manipulate[
Module[{data = GetSoundData[sound], r = GetSoundRate[sound], 
	lenth, max, res},
	lenth = Length[data];
	max = amp * Max[ Abs /@ data ];
    res = (# + RandomReal[{-max, max}] )& /@ data;
	(*return*)
	ListPlay[res, SampleRate->r] 	
 ],{{amp, 0.1, "Noise amplitude"}, 0.01, 1 }
]

End[]

EndPackage[]
