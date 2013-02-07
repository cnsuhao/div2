package com.swordandspade.trfs;

import android.content.Context;
import android.util.Log;
import android.view.SurfaceHolder;
import android.view.SurfaceView;

public class TrollView extends SurfaceView implements SurfaceHolder.Callback {

	public TrollView(Context context) {
		super(context);
		
		mContext = context;
		
		SurfaceHolder holder = getHolder();
		holder.addCallback(this);
		
		mThread = new TrollThread(holder, context);
	}

	@Override
    public void onWindowFocusChanged(boolean hasWindowFocus) {
        if (!hasWindowFocus) mThread.pause();
    }
	
	@Override
	public void surfaceChanged(SurfaceHolder holder, int format, int width,	int height) {
		mThread.setSurfaceSize(width, height);
	}

	@Override
	public void surfaceCreated(SurfaceHolder holder) {
		mThread.setRunning(true);
		mThread.start();
	}

	@Override
	public void surfaceDestroyed(SurfaceHolder holder) {
		boolean retry = true;
		mThread.setRunning(false);
		while (retry) {
			try {
				mThread.join();
				retry = false;
			} catch (InterruptedException ex) {}
		}
	}
	
	public TrollThread getThread() { return mThread; }
	
	private Context mContext;
	private TrollThread mThread;
	
	private final String TAG = "TRFS." + TrollView.class.getSimpleName();
}
