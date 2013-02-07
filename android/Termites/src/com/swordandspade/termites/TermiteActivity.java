package com.swordandspade.termites;

import android.os.Bundle;
import android.app.Activity;
import android.util.Log;

public class TermiteActivity extends Activity {

	private TermiteView mView;
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mView = new TermiteView(this);
        setContentView(mView);
    }

    @Override
    protected void onResume() {
        Log.e("rs", "onResume");

        // Ideally a game should implement onResume() and onPause()
        // to take appropriate action when the activity looses focus
        super.onResume();
        mView.resume();
    }

    @Override
    protected void onPause() {
        Log.e("rs", "onPause");

        // Ideally a game should implement onResume() and onPause()
        // to take appropriate action when the activity looses focus
        super.onPause();
        mView.pause();
        //Runtime.getRuntime().exit(0);
    }
    
    /*
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        getMenuInflater().inflate(R.menu.main_layout, menu);
        return true;
    }*/
}
