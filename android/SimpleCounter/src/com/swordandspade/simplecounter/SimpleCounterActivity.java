package com.swordandspade.simplecounter;

import android.app.Activity;
import android.os.Bundle;
import android.view.GestureDetector.SimpleOnGestureListener;
import android.view.MotionEvent;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.Chronometer;
import android.widget.RelativeLayout;
import android.widget.TextView;

public class SimpleCounterActivity extends Activity {
	
	// Widgets
	private CounterGestureListener mGestureListener;
	private RelativeLayout mRelativeLayout;
	private Chronometer mTimer;
	private TextView mCounterText;
	private TextView mHelpText;
	
	private Animation fadeIn;
	private Animation fadeOut;
	
	// Local vars
	private boolean running = false;
	private int count = 0;
	
	// Settings
    private String counterFormat = "%03d";
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        
        // load resources
        fadeIn = AnimationUtils.loadAnimation(this, R.anim.fadein);
        fadeOut = AnimationUtils.loadAnimation(this, R.anim.fadeout);
        setContentView(R.layout.main);
        
        // get our member objects
        mRelativeLayout = (RelativeLayout) findViewById(R.id.RelativeLayout);
        mTimer = (Chronometer) findViewById(R.id.Timer);
        mCounterText = (TextView) findViewById(R.id.CounterText);
        mHelpText = (TextView) findViewById(R.id.HelpText);
        
        // screen touch listener
        mGestureListener = new CounterGestureListener();
//        mRelativeLayout.setOnTouchListener( mCounterTouchListener );
        
        drawCounter();
    }
    
    private void inc() {
    	count++;
    	drawCounter();
    }
        
    private void drawCounter() {
    	mCounterText.setText( String.format(counterFormat, count) );
    }
    
    private void invertColors(boolean inv) {
    	int bg = getResources().getColor(R.color.bg);
    	int fg = getResources().getColor(R.color.fg);
    	
    	if(inv) {
    		int tmp = fg;
    		fg = bg;
    		bg = tmp;
    	}
    	
    	mRelativeLayout.setBackgroundColor(bg);
    	mTimer.setTextColor(fg);
    	mCounterText.setTextColor(fg);
    	mHelpText.setTextColor(fg);
    }
    
    private class CounterGestureListener extends SimpleOnGestureListener {
    	private boolean touched = false;
    	private double roughDistance = 0.0;		// calculated by summing dx and dy, not from actual distance 
    	
    	public boolean onTouch(View v, MotionEvent evt) {
    		if(evt.getAction() == MotionEvent.ACTION_DOWN) {
    			invertColors(true);
    			touched = true;
    		}
    		
    		else if(evt.getAction() == MotionEvent.ACTION_UP) {
    			invertColors(false);
    			if(!running) {
    				mTimer.start();
    				count = 0;
    				running = true;
    				mHelpText.startAnimation(fadeOut);
    			}
    			inc();
    		}
    		return true;
    	}
    }
}
