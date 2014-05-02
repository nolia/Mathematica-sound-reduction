(* ::Package:: *)

BeginPackage["SoundNoiseReduction`"];

GetSoundData::usage = "GetSoundData[sound] returns number representation of the sound"

GetSoundRate::usage = "GetSoundRate[sound] returns sample rate of the sound"

PlotSound::usage = "PlotSound[sound] plots the data of the sound"

AddWhiteNoise::usage = "AddWhiteNoise[sound] add some noise to the sound "

Begin["`Private`"]
GetSoundData[sound_Sound] := Module[ 
	{sdata, tmp},
	 If[ (sound[[ 1, 1, 1 ]] // Head) === List, 
		sdata = sound[[1,1,1]],
		(*reimport*)
		Export["temp.wav",sound ];
		tmp= Import["temp.wav", "Data"];
		sdata = tmp; 
	 DeleteFile["temp.wav"];
	];
	Return[sdata]
]

GetSoundRate[sound_Sound] := sound[[ 1, 2 ]]

PlotSound[sound_Sound]:= ListPlot[ GetSoundData[sound] ]

Options[AddWhiteNoise] = {WithNoise -> False};

AddWhiteNoise[sound_Sound, otps : OptionsPattern[] ]:= 
Manipulate[
Module[{data = GetSoundData[sound], r = GetSoundRate[sound], 
	lenth, max, res, noise,i,
	(*opts*)
	withNoise = OptionValue[WithNoise]
	}, 
	lenth = Length[data];
	max = amp * Max[ Abs /@ data ];
	noise = Table[RandomReal[{-max, max}],{i, 1, lenth}];
    res = data + noise;
	(*return*)
	Sound[
		SampledSoundList[ 
			If[withNoise, {res, noise}, res], 
		r ] 
	]
 ],{{amp, 0.1, "Noise amplitude"}, 0.01, 1 }
]

End[]

EndPackage[]
