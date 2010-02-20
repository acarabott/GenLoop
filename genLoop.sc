GenLoop {

	var <>s;				//Server
	var <clockBus;			//Control bus for the clock synth
	var <recStartBus;		//Control bus to signal start of recording
	var <onsetStartBus;		//Control bus to signal start of onset detection
	var <analysisClockBus;	//Control bus for sending analysis clock data
	var <duffAudioBus;		//Audio bus for sending signals not intended to be recieved by any Synth.
	var <alwaysOnBus;		//Control bus that provides a continuous 'on' signal
	var <bufferTime;		//Time (in seconds) for recording buffer
	var <recBuf;			//Recording buffer
	var <loopBufs;			//List of buffers for looping
	var <duffBuf;			//Blank buffer used for analysis synth when not analysing
	var <loopSynths;		//List of looping synths
	var <onSynth;			//Synth that is always 'on'
	var <onsetFrames;		//List of frames for analysis loop onsets
	var <onsetsList;		//List of lists storing all onsets
	var <currentLoopIndex;	//Index of the loop buffer currently 'on the table'
	var <>recSynth;			//Recording Synth
	var <>metroSynth;		//Metronome Synth
	var <>onsetSynth;		//Onset Detection Synth
	var <>oscr;				//OSC Responder for timing
	var <mixoscr;			//OSC Responder for mixer channels
	
	var <oneBeat;			//Number of samples required for 1 beat at this tempo.
	var <beatCounter;		//Storage for total passed beats
	var <currentBeat;		//Current beat of the bar
	var <startRecBeat;		//The buffer beat at which recording started
	var <endRecBeat;		//The buffer beat at which recording stopped
	var <bufferBeat;		//Beat relative to the recording buffer
	var <totalRecBeats;		//Total beats used for recording
	var <totalLoopBeats;	//Total beats used for a loop;
	
	var <nowRecording;		//Boolean, whether or recording is taking place
	var <doneCropping;	
	var <reallyCropping;
	var <nowAnalysing;		//Boolean, whether or not onset frame collection is taking place
	var <cuttingList;		//List of whether a layer should be looping or cutting
		
	var <onsetMeter;		//LED Meter for onset detector
	var <onsetLED;			//LED for onset detector
	var <inMeter;			//LED Meter for input signal
	var <guiMixer;			//The mixer GUI
	var <guiMixerHeight;	//Height of the mixer GUI
	var <guiMixerRect;		//The mixer GUI's size definitions
	var <guiMixerNextX;		//The next X value to build a channel at
	var <guiMixerMeters;	//A collection of meters for channels
	var <guiMixerAmps;		//The amplitude values of channels
	var <guiMixerPans;		//The pan values of channels
	var <channelRoutines;	//The routines for each channel
	var <channelProbs;		//The trigger probabilities for each channel
	
	var <>cropFrame;		//Frame to crop at
	var <>tempo;			//Tempo
	var <>timeSig;			//Time Signature
	
	*new {|aTempo=60, aTimeSig=4, aBufferTime=30|
		var bufferMod;
		
		//For safety, bufferTime must be a multiple of the time signature
		if((bufferMod = (aBufferTime%aTimeSig))!=0) {
			aBufferTime = aBufferTime - bufferMod
		};

		^super.new.genLoopInit(aTempo, aTimeSig, aBufferTime);
	}
	
	genLoopInit{|aTempo, aTimeSig, aBufferTime|
		
		//Store default server 
		s = Server.default;
		clockBus = Bus.audio(s,1);
		recStartBus = Bus.control(s, 1);
		onsetStartBus = Bus.control(s, 1);
		alwaysOnBus = Bus.control(s, 1);
		analysisClockBus = Bus.audio(s,1);
		duffAudioBus = Bus.audio(s,1);
		
		tempo = aTempo;
		timeSig = aTimeSig;		
		bufferTime = aBufferTime;
		oneBeat = (s.sampleRate / (tempo/60)).round;
		recBuf = Buffer.alloc(s, oneBeat*aBufferTime, 1);
		loopBufs = List[];
		duffBuf = Buffer.alloc(s, 1024);
		loopSynths = List[];
		onsetFrames = List[];
		onsetsList = List[];
		currentLoopIndex = 0;
		guiMixerHeight = 320;
		guiMixerMeters = List[];
		guiMixerAmps = List[];
		guiMixerPans = List[];	
		channelRoutines = List[];
				
		nowRecording = false;
		doneCropping = false;
		reallyCropping = false;
		nowAnalysing = false;
		cuttingList = List[];
		channelProbs = List[];
		
		beatCounter = 0;
		totalRecBeats = 0;
		
		{
		//Setup SynthDefs
			this.setSynthDefs; 
			s.sync;
			//Setup OSCResponder
			this.setOSCResponders;
			//Instantiate Synths
			this.setSynths;
		}.fork;
		//Setup GUI
		this.setMainGUI;
		this.setMixerGUI;
	}
	
	//Method to setup GUI
	setMainGUI {
		var win;
		var winW, winH;
		var recBut, analyseBut, loopBut, metroBut;
		var slider;
		var onsetMeterText, onsetText, inMeterText;
		
		win = Window.new("GenLoop", Rect((SCWindow.screenBounds.width/2)-200, (Window.screenBounds.height/2), 400, 400));
		winW = win.bounds.width;
		winH = win.bounds.height;
		recBut = GUI.button.new(win, Rect(62.5, 25, 75, 75));
		recBut.states = [["Record", Color.white, Color.red],["Stop", Color.white, Color.black]];

		recBut.action_(
			Routine {
				inf.do { 
					this.startRecordingAction;
					0.yield;		
					this.stopRecordingAction;
					0.yield;
				};
			};
		);
		
		analyseBut = GUI.button.new(win, Rect(262.5, 25, 75, 75));
		analyseBut.states = [["Analyse", Color.white, Color.grey]];
		
		analyseBut.mouseDownAction_(
			Routine {
				inf.do {
					this.startAnalysis;
					0.yield;
				}
			};
		);

		metroBut = GUI.button.new(win, Rect((winW/2)-50, (winH/10)*9, 100, 25));
		metroBut.states = [["Metronome On", Color.white, Color.blue],["Metronome Off", Color.white, Color.blue]];

		metroBut.action_(
			Routine {
				inf.do { 
					metroSynth.set(\beepVol, 0);
					0.yield;		
					metroSynth.set(\beepVol, 1);
					0.yield;		
				};
			};
		);
		
		slider = EZSlider.new(
						win, 
						Rect((winW/10)*9, winH/2-20, 20, 180), 
						"Onset Threshold  ", 
						ControlSpec.new(0, 1, 'linear', 0.001), 
						{|ez| onsetSynth.set(\val, ez.value)},
						0.5,
						labelWidth:120,
						numberWidth:22,
						
						layout: \line2
			);
		
		inMeter = SCLevelIndicator(win, Rect((winW/10), winH/2, 30, 160));
		inMeter.style = 2;
		inMeter.numSteps = 10;
		inMeter.numTicks = 11;
		inMeter.numMajorTicks = 3;
		inMeterText = StaticText(win, Rect((winW/10), winH/2+160, 20, 20));	
		inMeterText.string = "In";
		
		onsetText = StaticText(win, Rect((winW/10)*9, winH/2+160, 20, 20));
		onsetText.string = "OD";
		onsetLED = SCLevelIndicator(win, Rect((winW/10)*9, winH/2-40, 20, 20));
		onsetLED.style = 2;
		onsetLED.numSteps = 1;
		
		onsetMeter = SCLevelIndicator(win, Rect((winW/5)*4, winH/2, 30, 160));
		onsetMeter.style = 2;
		onsetMeter.numSteps = 10;
		onsetMeter.numTicks = 11;
		onsetMeter.numMajorTicks = 3;
		
		onsetMeterText = StaticText(win, Rect((winW/5)*4, winH/2+160, 20, 20));
		onsetMeterText.string = "VU";
		
		win.front;
		win.onClose = {	this.cleanUp };
		
	}
	
	setMixerGUI {
		guiMixerRect = Rect((Window.screenBounds.width/2)-200, (Window.screenBounds.height/2)-400, 20, guiMixerHeight);
		guiMixer = Window.new("GCMix", guiMixerRect);
		guiMixer.userCanClose_(false);
		guiMixerNextX = 0;
		guiMixer.front;
	}
	
	newGUIChannel {|index|
	var chan, lvl, mute, amp, prob, probSpecs, pan, panSpec, panVal, panText, automate, cutOrLoop;
		
		{
			guiMixerMeters.add(SCLevelIndicator(guiMixer, Rect(guiMixerNextX,0,25,240)));

			mute=Button(guiMixer, Rect(guiMixerNextX, 240, 20, 20));
			mute.states_([["M", Color.white, Color.grey],["M", Color.white, Color.blue]]);

			amp=EZSlider(guiMixer, Rect(guiMixerNextX+25,0,25,260), "Vol", \db.asSpec.step_(0.01), initVal:1, unitWidth:25, numberWidth:25,layout:\vert);
			amp.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey, Color.white, Color.yellow,nil,nil, Color.grey(0.7));
			amp.sliderView.focusColor_(Color.clear); 
			amp.font_(Font("Helvetica",10));

			amp.action_({|ez| loopSynths[index].set(\amp, ez.value.dbamp)});
			guiMixerAmps.add(amp);
			
			prob=EZSlider(guiMixer, Rect(guiMixerNextX+50,0,25,260), "Prob", initVal:0.25, unitWidth:25, numberWidth:25,layout:\vert);
			prob.setColors(Color.grey,Color.white, Color.grey(0.7),Color.grey, Color.white, Color.yellow,nil,nil, Color.grey(0.7));
			prob.sliderView.focusColor_(Color.clear); 
			prob.font_(Font("Helvetica",10));
			probSpecs = [	[0.5, 0.95,\linear, 0.01, 0.95].asSpec, 
							[0.2, 0.7, \linear, 0.01, 0.5].asSpec,
							[0.125, 0.8, \linear, 0.01, 0.125].asSpec,
							[0.0625, 0.7, \linear, 0.01, 0.0625].asSpec];

			prob.action_({|ez| 
				probSpecs.size.do { |i|	channelProbs[index][i]= probSpecs[i].map(ez.value)};
			});
			
			pan = Slider(guiMixer, Rect(guiMixerNextX,260,75,20));
			pan.value_(0.5);
			panSpec = ControlSpec(-1,1,\linear,0.01);
			panVal = StaticText(guiMixer, Rect(guiMixerNextX+35,280,35,20));
			panVal.font_(Font("Helvetica",10));
			pan.action_({
				panVal.string_(panSpec.map(pan.value).asString);
				loopSynths[index].set(\pan, panSpec.map(pan.value))
			});
			panText = StaticText(guiMixer, Rect(guiMixerNextX,280, 35,20));
			panText.string = "Pan: ";
			panText.font_(Font("Helvetica",10));
			guiMixerPans.add(pan);

			channelRoutines.add(this.setChannelAutomation(index));
			automate = Button(guiMixer, Rect(guiMixerNextX, 280, 75, 20));
			automate.states_([["Manual", Color.white, Color.grey],["Automated", Color.white, Color.red]]);
			
			automate.action_(
				Routine { inf.do {
					this.channelRoutines[index].reset;
					this.channelRoutines[index].play;
					0.yield;
					this.channelRoutines[index].stop;
					0.yield
				}}
			);

			cutOrLoop = Button(guiMixer, Rect(guiMixerNextX, 300, 75, 20));
			cutOrLoop.states_([["Looping", Color.black, Color.white],["Cutting", Color.white, Color.black]]);
			cutOrLoop.action_(
				Routine { inf.do {
					"hi".postln;
					this.cutButFunc(index);
					0.yield;
					"bye".postln;
					this.loopButFunc(index);
					0.yield
				}}
			);
						
			guiMixerNextX = guiMixerNextX + 100;
			guiMixerRect = guiMixer.bounds;
			guiMixerRect.width_(guiMixerRect.width+100);
			guiMixer.bounds = guiMixerRect;
		}.defer;
	}
	//Method to send Synth definitions to the server
	setSynthDefs {
	
		//Time Keeper SynthDef
		SynthDef(\clickTrack) { |buf, tempo, oneBeat, timeSig, outClock, recTrigger=0, outStartRecBus, onsetTrigger=0, outStartOnsetBus, beepVol=1|
			var phase;
			var crotchetTrig, beatOneTrig, quaverTrig, semiquaverTrig;
			var crotchetSTrig, beatOneSTrig, quaverSTrig, semiquaverSTrig;
			var beep, beepEnvFunc, env1, env2;
				
			phase = Phasor.ar(0, BufRateScale.kr(buf), 0, inf, 0);
			
			beep = [SinOsc.ar(2000,0,0.1), SinOsc.ar(1000,0,0.1)]*beepVol;
		
			beepEnvFunc = {|trig| EnvGen.ar(Env.asr(0.01,1,0.01, 'sine'), trig)};			
			
			beatOneTrig = Trig.ar(phase+1 % (oneBeat*timeSig));
			crotchetTrig = Trig.ar(phase+1 % oneBeat);
			quaverTrig = Trig.ar(phase+1 % (oneBeat*0.5));			
			semiquaverTrig = Trig.ar(phase+1 % (oneBeat*0.25));

			beatOneSTrig = SendTrig.kr(A2K.kr(beatOneTrig), 1, timeSig);
			crotchetSTrig = SendTrig.kr(A2K.kr(crotchetTrig), 2, 1);
			quaverSTrig = SendTrig.kr(A2K.kr(quaverTrig), 3, 1);
			semiquaverSTrig = SendTrig.kr(A2K.kr(quaverTrig), 3, 1);
			
			//Output clock data
			Out.ar(outClock, phase);
		
			//Output recStart trigger						
			Out.kr(outStartRecBus, Latch.kr(recTrigger, crotchetTrig));
		
			//Output loopStart trigger

			//onsetTrigger.poll;

			Out.kr(outStartOnsetBus, Select.kr(onsetTrigger, [DC.kr(0), A2K.kr(crotchetTrig)]));
		
			//Output metronome Beeps
			Out.ar(0, Pan2.ar(Mix.new([beep[0]*beepEnvFunc.(A2K.kr(beatOneTrig)), beep[1]*beepEnvFunc.(A2K.kr(crotchetTrig))]), 1));
	
		}.send(s);
		
		//Recording SynthDef
		SynthDef(\GCRec) { |sigIn=0, out=0, clockIn, bufnum, trigger, stopRec=0, oneBeat|
			var inSig;
			var clock;
			var phase;
			var phase2;
			var env;
			var sig;
			var trig;
			var trig2;
			var imp, delimp;
			var lag;
			
			inSig = SoundIn.ar(sigIn);
			clock = In.ar(clockIn);
			phase = clock%BufFrames.kr(bufnum);
			phase2 = (clock%oneBeat)+(s.sampleRate/50)%oneBeat;


			trig = In.kr(trigger);
			
			trig2 = (Trig1.kr(phase2, 0.2)-1).abs;

			env = EnvGen.ar(Env.asr(0.01, 1, 0.02, 'welch'), Select.kr(stopRec, [trig, trig2]));

			sig = BufWr.ar(inSig*env, bufnum, phase, 1);

			imp = Impulse.kr(10);
			delimp = Delay1.kr(imp);
			// measure rms and Peak
			SendReply.kr(imp, '/tr', [Amplitude.kr(inSig), K2A.ar(Peak.ar(inSig, delimp).lag(0, 3))], 0);
		
			Out.ar(out, inSig*env)
		}.send(s);
		
		//Loop and Onset detection SynthDef
		SynthDef(\onTrig, {|clockIn, bufnum, val=0.5|
		    var sig, chain, onsets, phase, sendVal;
			var imp, delimp;
			
			phase = In.ar(clockIn);
			
			sig = BufRd.ar(1, bufnum, phase, 1, 4);

			chain = FFT(LocalBuf(1024), sig);
		    onsets = Onsets.kr(chain, val, \rcomplex);
		 	SendTrig.kr(onsets, 4, phase);

			imp = Impulse.kr(10);
			delimp = Delay1.kr(imp);
			// measure rms and Peak
			SendReply.kr(imp, '/tr', [Amplitude.kr(sig), K2A.ar(Peak.ar(sig, delimp).lag(0, 3))], 6);
		    
			SendTrig.kr(onsets, 7, onsets);
		}).send(s);
		
		//Simple looping synth
		SynthDef(\GCPlay) { |bufnum, trigBus, clockOut, amp=1, pan=0, index, ampLag=0, panLag=0|
			var sig;
			var phase;
			var trigIn;
			var select;
			var latch;
			var imp, delimp;
			
			trigIn = In.kr(trigBus);
			latch = Latch.kr(trigIn, trigIn);
			
			phase = Phasor.ar(latch, BufRateScale.kr(bufnum), 0, BufFrames.kr(bufnum), 0);

			sig = BufRd.ar(1, bufnum, phase, 1, 4);

			select = Select.kr(latch, [trigIn, Lag.kr(amp, ampLag)]);

			imp = Impulse.kr(10);
			delimp = Delay1.kr(imp);
			// measure rms and Peak
			SendReply.kr(imp, '/mix', [Amplitude.kr(sig*select), K2A.ar(Peak.ar(sig*select, delimp).lag(0, 3))], index);
			
			//Output clock data
			Out.ar(clockOut, phase);
			//Output playback signal
			Out.ar(0, Pan2.ar(sig*select, Lag.kr(pan, panLag)));
		}.send(s);
				
		//Loop for playing back a specific portion of a buffer
		SynthDef(\GCHit) { |bufnum, startFrame, endFrame, rate=1|
			var sig;
			var phase;
			var dur;
			var env;

			dur = (endFrame-startFrame)/(BufSampleRate.kr(bufnum)*rate);
			phase = Line.ar(startFrame, endFrame, dur);
			sig = BufRd.ar(1, bufnum, phase, 0);
			env = EnvGen.ar(Env.new([0,1,1,0], [0.02, dur-0.05, 0.03 ]), doneAction:2);
			
			Out.ar(0, Pan2.ar(sig*env, 0));
		}.send(s);
		
		SynthDef(\onBus) { |outBus|
			Out.kr(outBus,
				DC.kr(1);
			);
		}.send(s);
		
		SynthDef(\offBus) { |outBus|
			Out.kr(outBus,
				DC.kr(0);
			);
		}.send(s);
	}
	
	//Method to instantiate Synths 
	setSynths {
		s.makeBundle(0.2, {
			//Instantiate metronome synth
			metroSynth = Synth(\clickTrack, [	\buf, recBuf, \outClock, clockBus, \tempo, tempo, \timeSig, timeSig, \oneBeat, oneBeat, 
												\recTrigger, 0, \outStartRecBus, recStartBus, \onsetTrigger, 0, \outStartOnsetBus, onsetStartBus, \beepVol, 1]);
			//Instantiate recording synth
			recSynth = Synth.after(metroSynth, \GCRec, [\bufnum, recBuf, \clockIn, clockBus, \trigger, recStartBus, \oneBeat, oneBeat]);
			
			//Instantiate onset detection synth		
			onsetSynth = Synth.after(metroSynth, \onTrig, [\clockIn, analysisClockBus, \bufnum, duffBuf]);			
			
			onSynth = Synth(\onBus, [\outBus, alwaysOnBus]);
			
		});
	}
	
	setInput {|newVal|
		recSynth.set(\sigIn, newVal);
	}
	
	//Method to setup OSCResponder.
	setOSCResponders {
		
		oscr = OSCresponder(s.addr,'/tr',{ |time,responder,msg|
		//	OSC id lookup cheat sheet
		//	0	Input Levels
		//	1	Beat One
		//	2	Crotchets
		//	3	Quavers
		//	4	Onsets Frames
		//	5	SemiQuavers
		//	6	Onset loop Level
		//	7	Onset LED
		
		switch (msg[2]) 
			{0}	{
					{
						inMeter.value = msg[3].ampdb.linlin(-40, 0, 0, 1);
						inMeter.peakLevel = msg[4].ampdb.linlin(-40, 0, 0, 1);
					}.defer
				}
			{1}	{ this.beatOneOSCAction }
			{2}	{ this.crotchetOSCAction }
			{3}	{ this.quaverOSCAction }
			{4}	{ 
					if(nowAnalysing) {
						onsetFrames.add(msg[3])
					};
				}
			{5}	{ this.semiquaverOSCAction }
			{6}	{
					{
						onsetMeter.value = msg[3].ampdb.linlin(-40, 0, 0, 1);
						onsetMeter.peakLevel = msg[4].ampdb.linlin(-40, 0, 0, 1);
					}.defer
				}
			{7}	{
					{
						{onsetLED.value = msg[3]/msg[3]}.defer;
						0.1.wait;
						{onsetLED.value = 0}.defer ;
					}.fork
				};
			
		}).add;
		
		mixoscr = OSCresponder(s.addr, '/mix', {|time, responder, msg|
			{
				guiMixerMeters[msg[2]].value = msg[3].ampdb.linlin(-40, 0, 0, 1);
				guiMixerMeters[msg[2]].peakLevel = msg[4].ampdb.linlin(-40, 0, 0, 1);
			}.defer
		}).add;
	}
	
	beatOneOSCAction {
		this.allHitFuncs(0);
	}
	
	crotchetOSCAction {	
		
		this.adjustCounters;
		
		this.nowRecordingFunc;
		
		this.nowAnalysingFunc;
		
		this.doneCroppingFunc;
		
		this.allHitFuncs(1);
	}
	
	quaverOSCAction {		
		this.allHitFuncs(2);
	}
	
	semiquaverOSCAction {
		this.allHitsFuncs(3)
	}
	
	adjustCounters {
		currentBeat = (beatCounter%timeSig) + 1;

		bufferBeat = (beatCounter%bufferTime);

		beatCounter = beatCounter + 1;
	}
	
	nowRecordingFunc {
		if(nowRecording,
			{totalRecBeats = totalRecBeats + 1}
		);
	}
	
	nowAnalysingFunc {
		var tempBuf;
		var tempSynths;
		
		if(nowAnalysing) {
			if(totalLoopBeats!=0) {
				totalLoopBeats = totalLoopBeats - 1;
			} {
				nowAnalysing = false;
				onsetsList.add(onsetFrames);
				onsetFrames = List[];
				channelProbs.add([0.95,0.6,0.25,0.125]);
				"finished analysis".postln;
				this.stopOnsetSynth;
			};
		};
	}
	
	doneCroppingFunc {
		if(doneCropping) {

			s.makeBundle(0.2, {
				//prevent synth from being re-triggered
				loopSynths[currentLoopIndex].set(\amp, 1, \trigBus, alwaysOnBus);
				onsetSynth.set(\bufnum, loopBufs[currentLoopIndex]);
				recSynth.set(\stopRec, 0);
			});
			s.makeBundle(0.2, {
				metroSynth.set(\onsetTrigger, 0);				
			});
			doneCropping = false;
			cuttingList.add(false);
		};
	}
	
	startRecordingAction {
		
		this.stopOnsetSynth;
		//Trigger recording
		s.makeBundle(0.2, {
			metroSynth.set(\recTrigger, 1);
			loopSynths.add(Synth.after(metroSynth, \GCPlay, [\bufnum, duffBuf, \trigBus, onsetStartBus, \clockOut, analysisClockBus, \index, currentLoopIndex]));				
			
		});
		//Note the beat in the buffer at which recording started
		
		startRecBeat = bufferBeat+1;

		"recording".postln;					//Change these for GUI stuff
		this.newGUIChannel(loopSynths.size-1);
		nowRecording = true;

	}
	
	stopRecordingAction {
		var syn;
		var startFrame, endFrame;

		nowRecording = false;

		endRecBeat = bufferBeat+1;
		
		startFrame = startRecBeat*oneBeat;

		endFrame = (endRecBeat*oneBeat);

		"stopped recording".postln;
		
		s.makeBundle(nil, {
			//Stop recording at next beat
			metroSynth.set(\recTrigger, 0, \onsetTrigger, 1);
			recSynth.set(\stopRec, 1);
		});
		
		//Crop the buffer at the next beat
		this.cropAndErase(startRecBeat, totalRecBeats);

	}
	//Method to crop a recording to the nearest beat and erase the recording buffer
	cropAndErase {|startBeat, totalBeats|
		var fakeEndVal;
		var firstLot;
		var copyFunc;
		var newBuf;
		var syn;
		
		
		fakeEndVal = startBeat+totalBeats;
		
		firstLot = bufferTime-startBeat;
		
		newBuf = Buffer.alloc(s, totalBeats*oneBeat);
		copyFunc = {|buf|	
			if(fakeEndVal>bufferTime) {
				recBuf.copyData(buf, 0, startBeat*oneBeat, firstLot*oneBeat);
				recBuf.copyData(buf, firstLot*oneBeat, 0, (totalBeats-firstLot)*oneBeat )
			} {
				recBuf.copyData(buf, 0, startBeat*oneBeat, totalBeats*oneBeat);
			};		
		};
		{
			copyFunc.(newBuf);
			loopBufs.add(newBuf);
			currentLoopIndex = (loopBufs.size-1);
			loopSynths.last.set(\bufnum, loopBufs.last);
			(60/tempo).wait;		
			copyFunc.(loopBufs.last);
			recBuf.zero;
			
		}.fork;
		//Erase recording buffer
		totalLoopBeats = totalRecBeats;
		totalRecBeats = 0;
		doneCropping = true;
	}
	
	startAnalysis {
		nowAnalysing = true;
	}
	
	loopButFunc {|index|
		//set analysis synth buffer to duffBuf
		this.stopOnsetSynth;
		//Turn up loop volume
		loopSynths[index].set(\amp, 1);
		//Turn off cutting
		cuttingList[index] = false;
		
		"now looping".postln;
	}
	
	cutButFunc {|index|
		//set analysis synth buffer to duffBuf
		this.stopOnsetSynth;
		//Turn down loop volume
		loopSynths[index].set(\amp, 0);
		//Activate cutting
		cuttingList[index] = true;
	}
	
	//Method to undo a bad loop
	undoAction {
		//Free the loop synth and remove it from storage
		loopSynths[currentLoopIndex].free;
		loopSynths.removeAt(currentLoopIndex);
		//Free the buffer and remove it from storage
		loopBufs[currentLoopIndex].free;
		loopBufs.removeAt(currentLoopIndex);
			
		//If the loop has been analysed, delete the analysis data
		if(onsetsList[currentLoopIndex]!=nil) {
			onsetsList.removeAt(currentLoopIndex)
		};
		
		//remove cutting trigger
		if(cuttingList[currentLoopIndex]!=nil) {
			cuttingList.removeAt(currentLoopIndex);			
		};
	}	
	
	stopOnsetSynth {
		s.makeBundle(0.2, {
			onsetSynth.set(\bufnum, duffBuf);
			loopSynths[currentLoopIndex].set(\clockOut, duffAudioBus);
		});
	}
			
	plotRecBuf {
		recBuf.plot;
	}
	
	hitFunc {|bufnum, startFrame, endFrame, rate=1|
		
		s.makeBundle(nil, {			
			Synth(\GCHit, [\bufnum, bufnum, \startFrame, startFrame, \endFrame, endFrame, \rate, rate]);							
		});
	}
	
	randomHitFunc {|chance=1, bufnum, hitList|
		var hit;
		var startFrame, endFrame;
		var rate;
		
		if(chance.coin) {
			
			hit = hitList.size.rand;
			startFrame = hitList[hit];
			
			if( hit == (hitList.size-1) ) {
				endFrame = bufnum.numFrames; 
				
			} {
				endFrame = hitList[hit+1];
			};
			rate = [-1,0.5,1,2].wchoose([0.15,0.1,0.6,0.15]);

			this.hitFunc(bufnum, startFrame, endFrame, rate)			
		};
	
	}
	
	allHitFuncs {|index|
		cuttingList.size.do { |i|
			if(cuttingList[i]) {
				this.randomHitFunc(channelProbs[i][index], loopBufs[i], onsetsList[i])
			};
		};
	}
	
	setChannelAutomation{|index|
		var rout = Routine {
			var waitTime, ampVal, panVal;
			inf.do {
				waitTime = 5.rand;
				ampVal = 1.0.rand.ampdb;
				panVal = rrand(-1.0, 1.0);
				{
					loopSynths[index].set(\ampLag, waitTime, \panLag, waitTime);
					guiMixerAmps[index].valueAction_(ampVal);
					guiMixerPans[index].valueAction_(panVal);
				}.defer;
				waitTime.wait;
			};
		};
		^rout;
	}
	
	cleanUp {
		s.makeBundle(0.2, {
			oscr.remove;
			mixoscr.remove;
		});
		s.makeBundle(0.2, {
		recSynth.free;
		metroSynth.free;
		onsetSynth.free;
		onSynth.free;
		recBuf.free;
		clockBus.free;	
		recStartBus.free;
		onsetStartBus.free;
		analysisClockBus.free;
		duffAudioBus.free;
		alwaysOnBus.free;
		loopSynths.do { |item, i| item.free };
		loopBufs.do { |item, i| item.free };
		channelRoutines.do { |item, i| item.stop};
		guiMixer.close;
		});
	}
}