package com.swordandspade.simplecounter;

import android.preference.PreferenceActivity;

import android.os.Bundle;

public class SimpleCounterPreferenceActivity extends PreferenceActivity {
	
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		this.addPreferencesFromResource(R.xml.prefs);
	}
}
