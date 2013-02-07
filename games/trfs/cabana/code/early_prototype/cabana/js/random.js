/**
 * random.js
 * 
 * Author: Alec Goebel
 * Date: 11/3/2010
 * 
 * Defines new prng objects in the Math library.  These are derived from
 * an explicitly seeded RC4-based algorithm.
 * 
 * Based on 'seedrandom.js' by David Bau.  
 * http://davidbau.com/archives/2010/01/30/random_seeds_coded_hints_and_quintillions.html
 * 
 * LICENSE (BSD):
 * Copyright:
 *     'random.js':      2010 Alec Goebel, all rights reserved.
 *     'seedrandom.js':  2010 David Bau, all rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *      
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *      
 *   3. Neither the name of this module nor the names of its contributors may
 *      be used to endorse or promote products derived from this software
 *      without specific prior written permission.
 *      
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

(function (math) {

/** 
 * Helper functions
 */

	
/** 
 * ARC4
 * 
 * An ARC4 implementation.
 * 
 * The get(n) method returns a pseudorandom integer of n bytes.  The bytes
 * are concatenated meaning the MSB is generated first.  The returned value
 * Its return value is an integer x in the range 0 <= x < 256^count.
 * 
 * @param key   An array of at with a length <= 256 integers.  Each integer should be 0 <= x < 256
 * @returns
 */
function ARC4(key) {
	var t, i, j;	// declare some temp variables we will use
	var S = [];		// all possible bytes
	
	// key must be at least a nullbyte
	if (key == undefined)
		key = [0];
	else if (typeof key == "string") {
		var arr = [];
		for (i=0; i<key.length; i++) 
			arr.push(key.charCodeAt(i));
		key = arr;
	}
	
	var keylen = key.length;
	
	// initialize and shuffle S
	for (i=0; i<256; i++) 
		S[i] = i;
	
	for (i=j=0; i<256; i++) {
		j = j + S[i] + key[i % keylen]; // next index provided by prev + current value + current value at key
		j = j & 255; 				 // only need the LSB
		t = S[i];
		S[i] = S[j];				 // swap
		S[j] = t;
	}
	
	// Main get method, return N random bytes
	var i = 0;
	var j = 0;
	this.get = function get(n) {
		n = n || 1;	// take at least 1 byte
		
		var b, r = 0;
		for(var c=0; c<n; c++) {
			i = (i+1) & 255;	// cycle i between 0 <= x < 256
			j = (j+S[i]) & 255; // set j randomly
			
			// swap
			t = S[i];
			S[i] = S[j];
			S[j] = t;
			
			b = S[ (S[i] + S[j]) & 255 ];	// get a byte based on the current step
			r = (r*256) + b;				// shift a byte left and add next byte
		}
		return r;
	};

	// For robust unpredictability discard an initial batch of values.
	// See http://www.rsa.com/rsalabs/node.asp?id=2009
	this.get(256);
}

/**
 * Random creates a generator which returns a random double in [0, 1) that 
 * contains randomness in every bit of the mantissa of the IEEE 754 value.
 * @param seed
 * @returns
 */
function Random(seed) {
	var chunk = 6;
	var arc4 = new ARC4(seed);
	
	/** 
	 * returns a random double between [0-1)
	 */
	this.rand = function rand() {
		var n = arc4.get(chunk);
		var d = Math.pow(2,chunk*8);
		return n/d;
	}
}

math.Random = Random;
})(Math);