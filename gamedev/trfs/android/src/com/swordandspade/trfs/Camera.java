package com.swordandspade.trfs;

import android.content.Context;
import android.graphics.Canvas;
import android.util.Log;

public class Camera {
	public Camera(TrollThread game, Context context) {
		mGame = game;
	}
	
	public void update(double dt) {
		Player player = mGame.getPlayer();
		
		float px = player.x + player.width / 2.0f;
		float py = player.y + player.height / 2.0f;
		
		x = px;
		y = py;
	}
	
	public void draw(Canvas c) {
		// shift the canvas opposite the the cameras potision
		c.translate(mGame.canvasWidth/2.0f - x, mGame.canvasHeight/2.0f - y);
	}
	
	private final TrollThread mGame;
	
	protected float x, y;
	
	private final String TAG = "TRFS." + Camera.class.getSimpleName();
}
