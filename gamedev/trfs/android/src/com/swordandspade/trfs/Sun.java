package com.swordandspade.trfs;

public class Sun {

	protected float y = -200f;
	protected float rate = K.TILE * 1.25f;
	
	public void update(double dt) {
		y += (float) (rate * dt);
	}
}