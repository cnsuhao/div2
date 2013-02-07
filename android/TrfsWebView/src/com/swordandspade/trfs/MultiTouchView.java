package com.swordandspade.trfs;

import android.content.Context;
import java.util.Formatter;
import java.util.Locale;

import android.util.AttributeSet;
import android.view.MotionEvent;
import android.webkit.WebView;
import android.util.Log;

public class MultiTouchView extends WebView {
	private static final String TAG = "MultiTouchView";
	private final StringBuilder sb = new StringBuilder();
	private final Formatter formatter = new Formatter(sb, Locale.US); 
	
	public float x;
	public float y;
	
	public MultiTouchView(Context context) {
		super(context);
		// TODO Auto-generated constructor stub
	}

	public MultiTouchView(Context context, AttributeSet attrs) {
		super(context, attrs);
		// TODO Auto-generated constructor stub
	}

	public MultiTouchView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		// TODO Auto-generated constructor stub
	}

	@Override
	public boolean onTouchEvent(MotionEvent evt) {
		String s = "act: " + evt.getAction() + "(" + evt.getX() + "," + evt.getY() +")"; 
		Log.v(TAG, s);
		return super.onTouchEvent(evt);
	}
}
