package com.swordandspade.hero.charselect;

import com.swordandspade.hero.R;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.TableRow;
import android.widget.TextView;

public class LooksSeekBar extends TableRow {

	public LooksSeekBar(Context context, AttributeSet attrs) {
		super(context, attrs);
		
		LayoutInflater inflater = (LayoutInflater)context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		inflater.inflate(R.layout.charselect_looks_seekbar, this, true);
	}
	
	private TextView mMinLabel;
	private TextView mMaxLabel;
}