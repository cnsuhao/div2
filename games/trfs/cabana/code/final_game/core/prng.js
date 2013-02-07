node.onCreateEntity = function onCreateEntity() {
    node.observe(node,"load");
}

node.onLoadSignaled = function onLoadSignaled() {
    randomSetup(Math);
    node.properties.loaded.signal();
}

function randomSetup(math) {

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

}