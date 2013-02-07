package com.swordandspade.trfs;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;

public class TrollActivity extends Activity {
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        
        mView = new TrollView(this);
        mView.getThread().setState(TrollThread.STATE_READY);
        
        setContentView(mView);
    }
    
    @Override
    protected void onPause() {
    	super.onPause();
    	mView.getThread().pause();
    }
    
    private TrollView mView;
    
    private static final String TAG = "TRFS." + TrollActivity.class.getSimpleName();
}