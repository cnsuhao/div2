package com.swordandspade.hero;

import android.app.TabActivity;
import android.content.Intent;
import android.content.res.Resources;
import android.os.Bundle;
import android.util.Log;
import android.widget.TabHost;

public class CharSelectNewActivity extends TabActivity {	
	@Override
	public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        
        //setContentView(R.layout.sandbox);
        setContentView(R.layout.charselect_new);
        
	    Resources res = getResources(); // Resource object to get Drawables
	    TabHost tabHost = getTabHost();  // The activity TabHost
	    TabHost.TabSpec spec;  // Resusable TabSpec for each tab
	    Intent intent;  // Reusable Intent for each tab

	    // Create an Intent to launch an Activity for the tab (to be reused)
	    intent = new Intent().setClass(this, CharSelectGeneralActivity.class);

	    // Initialize a TabSpec for each tab and add it to the TabHost
	    spec = tabHost.newTabSpec("general").setIndicator("General").setContent(intent);
	    tabHost.addTab(spec);

	    // Do the same for the other tabs
	    intent = new Intent().setClass(this, CharSelectAlignActivity.class);
	    spec = tabHost.newTabSpec("align").setIndicator("Alignment").setContent(intent);
	    
	    tabHost.addTab(spec);

	    tabHost.setCurrentTab(0);
	}
	
	private final static String TAG = "Hero.CharSelect";
}