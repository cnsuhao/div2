package com.swordandspade.hero;

import android.app.Activity;
import android.app.ActivityGroup;
import android.graphics.drawable.TransitionDrawable;
import android.os.Bundle;
import android.util.Log;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.ViewAnimator;

public class CharSelectActivity extends ActivityGroup {
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		
		setContentView(R.layout.charselect);
		
		LinearLayout main = (LinearLayout) findViewById(R.id.main);		
	}
	
	private static final String TAG = "Hero.CharSelect";
}