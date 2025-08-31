/**
 * plasma.js
 * Copyright (c) 2018 Carl Gorringe 
 * https://github.com/cgorringe/PlasmaJS
 * 1/25/2018
 **/

if (typeof module !== 'undefined') module.exports = PlasmaJS;

function PlasmaJS (self) {

	// public defaults
	if (typeof self === 'undefined') self = {};
	if (!('palette'      in self)) self.palette      = "rainbow";
	if (!('timeInterval' in self)) self.timeInterval = 50;

	// private vars
	var canvas, bgColor, ctx, img, frameCount, timer, fullmode = false, bu = {};
	var plasma1, plasma2;
	var palR = new Uint8ClampedArray(256);
	var palG = new Uint8ClampedArray(256);
	var palB = new Uint8ClampedArray(256);

	var colorGradients = {
		// [ start, end, R1, G1, B1, R2, G2, B2 ]
		"rainbow":[
			[   0,  35, 255,   0, 255,   0,   0, 255 ], // magenta -> blue
			[  36,  71,   0,   0, 255,   0, 255, 255 ], // blue -> cyan
			[  72, 107,   0, 255, 255,   0, 255,   0 ], // cyan -> green
			[ 108, 143,   0, 255,   0, 255, 255,   0 ], // green -> yellow
			[ 144, 179, 255, 255,   0, 255, 127,   0 ], // yellow -> orange
			[ 180, 215, 255, 127,   0, 255,   0,   0 ], // orange -> red
			[ 216, 255, 255,   0,   0, 255,   0, 255 ]  // red -> magenta
		],
		"nebula":[
			[   0,  31,   0,   0,   0,   0,   0, 127 ], // black -> half blue
			[  32,  95,   0,   0, 127, 127,   0, 255 ], // half blue -> blue-violet
			[  96, 159, 127,   0, 255, 255,   0,   0 ], // blue-violet -> red
			[ 160, 191, 255,   0,   0, 255, 255, 255 ], // red -> white
			[ 192, 255, 255, 255, 255,   0,   0,   0 ]  // white -> black
		],
		"fire":[
			[   0,  23,   0,   0,   0,   0,   0, 127 ], // black -> half blue
			[  24,  47,   0,   0, 127, 255,   0,   0 ], // half blue -> red
			[  48,  95, 255,   0,   0, 255, 255,   0 ], // red -> yellow
			[  96, 127, 255, 255,   0, 255, 255, 255 ], // yellow -> white
			[ 128, 159, 255, 255, 255, 255, 255,   0 ], // white -> yellow
			[ 160, 207, 255, 255,   0, 255,   0,   0 ], // yellow -> red
			[ 208, 231, 255,   0,   0,   0,   0, 127 ], // red -> half blue
			[ 232, 255,   0,   0, 127,   0,   0,   0 ]  // half blue -> black
		],
		"bluegreen":[
			[   0,  23,   0,   0,   0,   0,   0, 127 ], // black -> half blue
			[  24,  47,   0,   0, 127,   0, 127, 255 ], // half blue -> teal
			[  48,  95,   0, 127, 255,   0, 255,   0 ], // teal -> green
			[  96, 127,   0, 255,   0, 255, 255, 255 ], // green -> white
			[ 128, 159, 255, 255, 255,   0, 255,   0 ], // white -> green
			[ 160, 207,   0, 255,   0,   0, 127, 255 ], // green -> teal
			[ 208, 231,   0, 127, 255,   0,   0, 127 ], // teal -> half blue
			[ 232, 255,   0,   0, 127,   0,   0,   0 ], // half blue -> black
		],
		"rgb":[
			[   0,  63,   0,   0,   0, 255,   0,   0 ], // black -> red
			[  64, 127,   0,   0,   0,   0, 255,   0 ], // black -> green
			[ 128, 191,   0,   0,   0,   0,   0, 255 ], // black -> blue
			[ 192, 255,   0,   0,   0, 255, 255, 255 ]  // black -> white
		]
	}

	// private methods
	function randInt (min, max) {
		return Math.floor(Math.random() * (max - min + 1)) + min;
	}

	function setPalette(name) {
		if (name && colorGradients[name]) {
			var num, i, j, k;
			for (var g in colorGradients[name]) {
				var grad = colorGradients[name][g];
				// grad[] = [ 0:start, 1:end, 2:R1, 3:G1, 4:B1, 5:R2, 6:G2, 7:B2 ]
				num = grad[1] - grad[0];
				for (i=0; i <= num; i++) {
					k = i / num;
					j = grad[0] + i;
					palR[j] = grad[2] + (grad[5] - grad[2]) * k;
					palG[j] = grad[3] + (grad[6] - grad[3]) * k;
					palB[j] = grad[4] + (grad[7] - grad[4]) * k;
				}
			}
		}
	}

	function setupPlasma () {
		// setup plasma buffers
		const w = ctx.canvas.width;
		const h = ctx.canvas.height;
		img = ctx.createImageData(w, h);
		plasma1 = new Uint8ClampedArray(w * h * 4);
		plasma2 = new Uint8ClampedArray(w * h * 4);

		var dst = 0;
		for (var y=0; y < (h*2); y++) {
			for (var x=0; x < (w*2); x++) {
				plasma1[dst] = (64 + 63 * (Math.sin( Math.sqrt((h-y)*(h-y)+(w-x)*(w-x))/ (w/20) )) );
				plasma2[dst] = (64 + 63 * Math.sin( x/(74+15*Math.cos(y/140)) ) * Math.cos( y/(61 + 11 * Math.sin( x/114 )) ) );
				dst++;
			}
		}
	}

	// public methods
	self.init = function (canvasId) {
		if (canvasId) {
			canvas = document.getElementById(canvasId);
			bgColor = window.getComputedStyle(canvas, null).getPropertyValue('background-color');
			ctx = canvas.getContext('2d');
			self.clear();
			setPalette(self.palette);
			setupPlasma();
			frameCount = 0;
		}
	}

	self.clear = function () {
		if (ctx) {
			ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
		}
	}

	self.reset = function () {
		setupPlasma();
		frameCount = 0;
	}

	self.start = function () {
		if (ctx) {
			if (timer) clearTimeout(timer);
			timer = setInterval(self.draw, self.timeInterval);
		}
		else {
			console.log("ERROR: PlasmaJS: Must set 'canvasId' in constructor, or call init().");
		}
	}

	self.stop = function () {
		if (timer) {
			clearTimeout(timer);
		}
	}

	self.setPalette = function (name) {
		self.palette = name;
		setPalette(name);
	}

	self.draw = function () {
		if (ctx) {
			// position plasma
			var w = ctx.canvas.width;
			var h = ctx.canvas.height;
			var hw = w / 2;
			var hh = h / 2;
			var x1 = Math.floor( hw + ((hw - 1) * Math.cos(frameCount /   97)) );
			var x2 = Math.floor( hw + ((hw - 1) * Math.sin(frameCount / -114)) );
			var x3 = Math.floor( hw + ((hw - 1) * Math.sin(frameCount / -137)) );
			var y1 = Math.floor( hh + ((hh - 1) * Math.sin(frameCount /  123)) );
			var y2 = Math.floor( hh + ((hh - 1) * Math.cos(frameCount /  -75)) );
			var y3 = Math.floor( hh + ((hh - 1) * Math.cos(frameCount / -108)) );
			var src1 = y1 * w * 2 + x1;
			var src2 = y2 * w * 2 + x2;
			var src3 = y3 * w * 2 + x3;

			// draw plasma
			var i, dst = 0;
			for (var y=0; y < h; y++) {
				for (var x=0; x < w; x++) {
					// plot pixel as a sum of plasma functions
					i = (plasma1[src1++] + plasma2[src2++] + plasma2[src3++]) & 0xFF;
					img.data[dst++] = palR[i];
					img.data[dst++] = palG[i];
					img.data[dst++] = palB[i];
					img.data[dst++] = 0xFF;
				}
				src1 += w; src2 += w; src3 += w;
			}
			ctx.putImageData(img, 0, 0);
			frameCount++;
		}
	}

	self.fullscreen = function () {
		if (canvas) {
			if (canvas.requestFullscreen) {
				canvas.requestFullscreen();
				document.addEventListener("fullscreenchange", self.fullscreenchange, false);
			}
			else if (canvas.msRequestFullscreen) {
				canvas.msRequestFullscreen();
				document.addEventListener("msfullscreenchange", self.fullscreenchange, false);
			}
			else if (canvas.mozRequestFullScreen) {
				canvas.mozRequestFullScreen();
				document.addEventListener("mozfullscreenchange", self.fullscreenchange, false);
			}
			else if (canvas.webkitRequestFullscreen) {
				canvas.webkitRequestFullscreen();
				document.addEventListener("webkitfullscreenchange", self.fullscreenchange, false);
			}
		}
	}

	// called when entering and exiting full screen
	self.fullscreenchange = function () {
		self.stop();
		if (fullmode == false) {
			// entering fullscreen
			fullmode = true;
			// backup canvas
			bu.width = ctx.canvas.width;
			bu.height = ctx.canvas.height;
			// resize canvas to fullscreen
			var winW = window.innerWidth, winH = window.innerHeight;
			canvas.style.width = winW + 'px';
			canvas.style.height = winH + 'px';
			ctx.canvas.width = winW;
			ctx.canvas.height = winH;
			self.clear();
			setupPlasma();
		}
		else {
			// exiting fullscreen
			fullmode = false;
			// restore canvas
			canvas.style.width = bu.width + 'px';
			canvas.style.height = bu.height + 'px';
			ctx.canvas.width = bu.width;
			ctx.canvas.height = bu.height;
			self.clear();
			setupPlasma();
		}
		self.start();
	}

	// constructor
	self.init(self.canvasId);
	return self;
}