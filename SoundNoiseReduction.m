(* ::Package:: *)

BeginPackage["SoundNoiseReduction`"];

GetSoundData::usage = "GetSoundData[sound] returns number representation of the sound"
GetSoundRate::usage = "GetSoundRate[sound] returns sample rate of the sound"
PlotSound::usage = "PlotSound[sound] plots the data of the sound"
GetSoundDiff::usage = "GetSoundDiff[sound1, sound2] get numerical abs for difference between\
sound1 and sound2."
ShowSoundDiff::usage = "ShowSoundDiff[sound1, sound2] shows diff between sound1\
and sound2 as bar chart. "

AddNoise::usage = "AddNoise[sound] adds chosen noise to the sound"

AddWhiteNoise::usage = "AddWhiteNoise[sound] adds random some noise to the sound "
AddBandNoise::usage = "AddBandNoise[sound] adds band-limited noise to sound. "
AddPulseNoise::usage = "AddPulseNoise[sound] adds pulse noise to sound " 

addWhiteNoise::usage = "addWhiteNoise[sound] adds random some noise to the sound "
addBandNoise::usage = "addBandNoise[sound] adds band-limited noise to sound. "
addPulseNoise::usage = "addPulseNoise[sound] adds pulse noise to sound " 

CancelNoiseInSound::usage = "CancelNoiseInSound[sound, sigChannel, noiseChannel] cancels noise \
by substracting noise channel from signal channel"

SoundWienerFilter::usage = "SoundWienerFilter[sound, r] clears noise from sound \
by applying Wienner Filter with dimension r to sound data. "
SoundBandFilter::usage = "SoundBandFilter[sound, low, high] clears noise from sound \
by appluing Bandpass Filter to sound data. "

Begin["`Private`"]
GetSoundData[sound_Sound] := GetSoundData[sound, 1];

GetSoundData[sound_Sound, chan_Integer] := Module[ 
	{sdata, tmp},
	 If[ (sound[[ 1, 1, chan ]] // Head) === List, 
		sdata = sound[[1, 1, chan]],
		(*reimport*)
		Export["temp.wav",sound ];
		tmp = Import["temp.wav", "Data"] // Flatten;
		sdata = tmp;
	 DeleteFile["temp.wav"];
	];
	Return[sdata]
]

GetSoundRate[sound_Sound] := sound[[ 1, 2 ]]

PlotSound[sound_Sound]:= ListPlot[ GetSoundData[sound] ]

GetSoundDiff[s1_Sound, s2_Sound] :=
	Module[{data1 = GetSoundData[s1], data2 = GetSoundData[s2]},
	Abs[ data1 - data2 ] // {Max[#],Mean[#],Min[#] }&
]

ShowSoundDiff[s1_Sound, s2_Sound] :=
  Module[{diff = GetSoundDiff[s1,s2]},
	BarChart[diff,
    ChartLegends->{"Max" ,"Average","Min"} , 
    ChartStyle->"DarkRainbow"
   ]
]


(*noise creation & manipulations *)

Options[addWhiteNoise] = {WithNoise -> False};

addWhiteNoise[sound_Sound, 
	amp1_Real, 
	opts : OptionsPattern[]] := 
Module[{data = GetSoundData[sound], r = GetSoundRate[sound], 
	lenth, max, res, noise,i,amp,
(*res*)
	s,
(*opts*)
	withNoise = OptionValue[WithNoise]
	}, 
	amp = If[amp1 < 0.01, 0.01, If[amp1 > 3, 3, amp1]];
  
	lenth = Length[data];
	max = amp * Max[ Abs /@ data ];
	noise = Table[RandomReal[{-max, max}],{i, 1, lenth}];
    res = data + noise;
	s = Sound[
		SampledSoundList[ 
			If[withNoise, {res, noise}, res], 
		r ] 
	]
 ]


Options[AddWhiteNoise] = {WithNoise -> False};

AddWhiteNoise[sound_Sound, otps : OptionsPattern[] ]:= 
Manipulate[
	addWhiteNoise[sound, amp, WithNoise -> OptionValue[WithNoise] ]
,{{amp, 0.1, "Noise amplitude"}, 0.01, 2 }
]

Options[addBandNoise] = {WithNoise -> False };

addBandNoise[sound_Sound, 
	amp_, bandLow_, bandHigh_,
	otps : OptionsPattern[] ] := 
Module[{data = GetSoundData[sound], r = GetSoundRate[sound], 
	lenth, max, res, noise,i,
(*opts*)
	withNoise = OptionValue[WithNoise]
	}, 
	lenth = Length[data];
	max = amp * Max[ Abs /@ data ];
	noise = BandpassFilter[ Table[RandomReal[{-max, max}],{i, 1, lenth}] ,
			Sort[{N[bandLow*Pi]//Abs, N[bandHigh*Pi]//Abs}], 
			SampleRate->r
			];
    res = data + noise;
(*return*)
	Sound[
		SampledSoundList[ 
			If[withNoise, {res, noise}, res], 
		r ] 
	]
 ]

Options[AddBandNoise] = {WithNoise -> False };

AddBandNoise[sound_Sound, otps : OptionsPattern[] ]:= 
Manipulate[
	addBandNoise[
		sound, amp, bandLow, bandHigh,
		 WithNoise -> OptionValue[WithNoise]
	]
  ,{{amp, 0.1, "Noise amplitude"}, 0.01, 2 },
   {{bandLow , 1, "Low band frequency"}, 20, GetSoundRate[sound] } , 
   {{bandHigh, 10, "High band frequency "}, 10, GetSoundRate[sound] }  
]

Options[addPulseNoise] = {WithNoise -> False};
addPulseNoise[sound_Sound, 
amp_,  nImpl_, dur_,
otps : OptionsPattern[] ] :=
Module[{plen, data = GetSoundData[sound],r=GetSoundRate[sound], length, noise,i , max , padLength,pulseTable,
	res, interLength , 
	withNoise = OptionValue[WithNoise]
	},
(*init*)
	length = Length[data];
    max = amp * Max[ Abs /@ data ];
	plen = Round[ dur*r ];
	interLength = Round[ length / nImpl ];
	padLength = Round[ (interLength - plen) / 2];
(*creates pulses*)
	pulseTable = Table[ max Sin[ (2Pi/plen) t],{t,1,plen}];
	noise = Flatten[
	Table[ ArrayPad[pulseTable, padLength],{nImpl}]];
	If[ Length[noise] >= length, 
	noise = Take[noise, length],
	noise = PadRight[noise, length] 
	];
(*adds pulses*)
	res = data + noise;
(*return*)
	Sound[
		SampledSoundList[ 
			If[withNoise, {res, noise}, res], 
		r ] 
	]
]


Options[AddPulseNoise] = {WithNoise -> False};

AddPulseNoise[sound_Sound, otps : OptionsPattern[] ] :=
Manipulate[
	addPulseNoise[sound, amp, nImpl, dur]
,{{amp, 1, "Noise amplitude"}, 1, 10} , 
 {{nImpl, 1,"Number of pulses"} , 1, 10 },
 {{dur, 0.01, "Pulse duration"}, 0.01, 0.1,0.01}
]

(*Main function for adding noise*)
AddNoise[sound_Sound] :=
Manipulate[ fun[sound, WithNoise-> wn],
{{fun, AddWhiteNoise, "Noice type:"}, {AddWhiteNoise, AddPulseNoise, AddBandNoise} }, 
  {{wn, False, "With noice channel"},{ True, False}, Checkbox }
	
];

(*noise reducing functions*)
CancelNoiseInSound[sound_Sound, dataChan_Integer, noiseChan_Integer] :=
 Module[{res, s, noise},If[TrueQ[ ( (GetSoundData[sound, dataChan] // Head) ==  List)
	&& ( Head[GetSoundData[sound, noiseChan] ] == List) 
	],
	s = GetSoundData[sound,dataChan];
	noise =GetSoundData[sound,noiseChan];
	res = Plus[ s,Minus[noise] ];
	Sound[ SampledSoundList[ res, GetSoundRate[sound] ] 
 ] 
,
(*error*)
	Print["Can't access noise channel !"]
	]
];

SoundWienerFilter[sound_Sound, r_Integer]:= 
	Module[{res,data = GetSoundData[sound], rate = GetSoundRate[sound]},
	res = WienerFilter[data,r];
	Sound[SampledSoundList[res, rate]]
];

SoundBandFilter[sound_Sound, low_, high_] :=
Module[{data = GetSoundData[sound], r = GetSoundRate[sound], res , l, h},
	{l,h} = Sort[{low, high}];
	res = BandpassFilter[data, 
		{ If[l<0, 0, l ] // N, If[ h > r, r, h] // N} *Pi, 
		SampleRate->r];
	Sound[SampledSoundList[res,r]]
]


End[]

EndPackage[]
