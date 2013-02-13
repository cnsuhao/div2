package com.swordandspade.trfs;

import android.content.Context;
import android.graphics.drawable.Drawable;

public class Level { 
	private final long mSeed;
	
	public Level (Context context, long seed) {
		if ( seed == -1 ) {
			mSeed = System.currentTimeMillis();
		} else {
			mSeed = seed;
		}
		
	}
	
	public void update(double dt) {}
	
	public void draw() {
		
	}

	private Drawable mRock;
}
