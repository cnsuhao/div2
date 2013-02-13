package com.swordandspade.trfs;

import android.content.Context;
import android.content.res.Resources;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.drawable.AnimationDrawable;
import android.util.Log;

public class Player {
	// constants
	public static final int IDLE = 0;
	public static final int RUN = 1;
	public static final int JUMP = 2;
	public static final int WALL = 3;
	
	// settings
	private final float RUN_SPEED = 10f;
	private final float RUN_ACC_DIST = 0.5f;
	
	private final float ROLL_DIST = 2f;
	
	private final float JUMP_ANG = 45f;
	
	// variables
	private final float maxVX = K.TILE * RUN_SPEED;
	private final float runAcc = K.TILE * (RUN_SPEED * RUN_SPEED) / (2 * RUN_ACC_DIST);
	private final float rollDist = K.TILE * ROLL_DIST;
	
	
	public Player(TrollThread game, Context context) {
		mGame = game;
		
		// load animations
		Resources res = context.getResources();
		
		mPaint = new Paint();
		mPaint.setARGB(255, 255, 0, 0);
		
		x = 0;
		y = 0;
		height = K.TILE * 1.5f;
		
		state = RUN;
		
		vx = 0.f;
		vy = K.TILE;
	}
	
	public void update(double dt) {
		if (state == RUN) {
			dx = (float) (x + vx * dt + 0.5 * ax * dt *dt);
			dy = y;
		}
		
		x = dx;
		y = dy;
		
		facing = vx / Math.abs(vx);
	}
	
	public void draw(Canvas c) {
		c.save();
		
		// move into position
		c.translate(x, y);
		
		// draw the proper frame
		c.drawRect(0, 0, width, height, mPaint);
		
		c.restore();
	}
	
	private final TrollThread mGame;
	
	protected float x, y, vx, vy, ax, ay, h, state, facing;
	protected float dx, dy, dvx, dvy;
	
	protected float width = K.TILE;
	protected float height = K.TILE;
	
	public Paint mPaint;
	
	private final String TAG = "TRFS." + Player.class.getSimpleName();
}
