package com.swordandspade.trfs;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.BitmapFactory.Options;
import android.graphics.BitmapShader;
import android.graphics.Canvas;
import android.graphics.LinearGradient;
import android.graphics.Matrix;
import android.graphics.Paint;
import android.graphics.PorterDuff;
import android.graphics.PorterDuffXfermode;
import android.graphics.Rect;
import android.graphics.Shader;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.os.Handler;
import android.util.Log;
import android.view.SurfaceHolder;

public class TrollThread extends Thread {
	public static final int STATE_LOSE = 1;
    public static final int STATE_PAUSE = 2;
    public static final int STATE_READY = 3;
    public static final int STATE_RUNNING = 4;
    
	public TrollThread(SurfaceHolder surfaceHolder, Context context) {
		// setup necessary variables
		mSurfaceHolder = surfaceHolder;
		mContext = context;
		
		// load resources
		mPlayer = new Player(this, context);
		mCamera = new Camera(this, context);
		mSun = new Sun();
		
		mLinePaint = new Paint();
        mLinePaint.setAntiAlias(true);
        
        mBackground = mContext.getResources().getDrawable(R.drawable.rock_tile);
        
        tileNightPaint = new Paint();
        tileNightPaint.setXfermode(new PorterDuffXfermode(PorterDuff.Mode.MULTIPLY));
        tileNightPaint.setStyle(Paint.Style.FILL);
        tileNightPaint.setARGB(255, 60, 0, 200);
        tileNightPaint.setShader(null);
        tileNightShader = new LinearGradient(0, 0, 0, K.SUN_WIDTH, 0xffffffff, 0xff3C00C8, Shader.TileMode.CLAMP);
        
        doStart();
	}
	
	/*
	 * Starts the game
	 */
	public void doStart() {
        synchronized (mSurfaceHolder) {
        	mLastTime = System.currentTimeMillis() + 100;
            setState(STATE_RUNNING);
        }
    }

	public void pause() {
        synchronized (mSurfaceHolder) {
            if (mState == STATE_RUNNING) setState(STATE_PAUSE);
        }
    }
	
	public void unpause() {
		synchronized (mSurfaceHolder) {
			mLastTime = System.currentTimeMillis() + 100;
		}
		setState(STATE_RUNNING);
	}
	
	@Override
	public void run() {
		// work around to fix non launch
		if (mState == STATE_READY)
			mState = STATE_RUNNING;
		
		while (mRun) {
			Canvas c = null;
			
			now = System.currentTimeMillis();
			if (now - mLastTime < K.FRAMERATE) continue;
			
			try {
				c = mSurfaceHolder.lockCanvas(null);
				
				synchronized (mSurfaceHolder) {
					if (mState == STATE_RUNNING) {
						update();
					}
					draw(c);
				}
			} finally {
				if (c != null)
					mSurfaceHolder.unlockCanvasAndPost(c);
			}
		}
	}
	
	public void update() {
		// require minimum framerate
		double dt = (now - mLastTime) / 1000.0;
		
		mPlayer.update(dt);
		mSun.update(dt);
		mCamera.update(dt);
		
		mLastTime = now;
	}
	
	public void draw(Canvas c) {
		// screen hasn't resized so skip this draw
		if (mBackgroundImage == null)
			return;
		
		// invert our axis
		c.scale(1f, -1f, 0f, 0f);
		c.translate(0f, -canvasHeight);
		
		// draw level
		c.drawBitmap(mBackgroundImage, -mPlayer.x % K.TILE, -mPlayer.y % K.TILE, null);
		
		float sun = mSun.y - mCamera.y;
		if (sun < -K.SUN_WIDTH) {
			c.drawRect(0, 0, canvasWidth, canvasHeight, tileNightPaint);
		}
		else if (sun < canvasHeight) {
			c.drawRect(0, sun + K.SUN_WIDTH, canvasWidth, canvasHeight, tileNightPaint);
			c.save();
			tileNightPaint.setShader(tileNightShader);
			c.translate(0, sun);
			c.drawRect(0, 0, canvasWidth, K.SUN_WIDTH, tileNightPaint);
			tileNightPaint.setShader(null);
			c.restore();
		}		
		
		// move for camera		
		mCamera.draw(c);

		// draw our fake level
		mLinePaint.setARGB(255, 0, 0, 0);
		c.drawRect(new Rect((int)-K.TILE, (int)-K.TILE, (int)K.TILE*12, (int)K.TILE*12), mLinePaint);
		//c.drawRect(new Rect(0, 0, 300, 300), mLinePaint);
		mPlayer.draw(c);
	}
	
	public void setState(int state) {
		synchronized (mSurfaceHolder) {
			mState = state;
		}
	}
	
	public void setRunning(boolean b) { mRun = b; }
	
	public void setSurfaceSize(int width, int height) {
		canvasWidth = width;
		canvasHeight = height;
		
		int w = (int)(width + K.TILE);
		int h = (int)(height + K.TILE);
		
		mBackground.setBounds(0, 0, w, h);
		mBackgroundImage = Bitmap.createBitmap(w, h, Bitmap.Config.RGB_565);
        Canvas c = new Canvas(mBackgroundImage);
        mBackground.draw(c);
        mBackgroundImage.prepareToDraw();
	}
	
	public Player getPlayer() { return mPlayer; } 
	
	private boolean mRun = true;
	private int mState = 0;
	private long now;
	private long mLastTime;
	
	protected int canvasWidth;
	protected int canvasHeight;
	
	protected Paint tileNightPaint;
	protected LinearGradient tileNightShader;
	
	private final Player mPlayer;
	private final Camera mCamera;
	private final Sun mSun;
	
	private Paint mLinePaint;
	
	private final SurfaceHolder mSurfaceHolder;
	private final Context mContext;
	
	private final String TAG = "TRFS." + TrollThread.class.getSimpleName();
	
	private final Drawable mBackground;
	private Bitmap mBackgroundImage;
	private Bitmap mBackgroundNightImage;
}