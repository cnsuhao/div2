// An attempt to create radio buttons in a MxN grid instead of just in a row/col
// The purpose was to use in a D&D alignment selection, but it seemed general enough to abstract
// I attempted to make sure the widget was fully themeable
//
// Note:  probably does not work as is, I found the contents commented out in the src folder

package com.swordandspade.widgets;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.widget.CompoundButton;
import android.widget.RadioButton;
import android.widget.TableLayout;
import android.widget.TableRow;
import android.widget.TextView;

public class RadioGrid extends TableLayout implements CompoundButton.OnCheckedChangeListener  {
	public RadioGrid(Context context, AttributeSet attrs) {
		super(context, attrs);
		// get attributes
		TypedArray a = context.obtainStyledAttributes(attrs, R.styleable.RadioGrid);
		CharSequence[] lblCols = a.getTextArray(R.styleable.RadioGrid_labels_cols);
		CharSequence[] lblRows = a.getTextArray(R.styleable.RadioGrid_labels_rows);
		int rows = a.getInt(R.styleable.RadioGrid_rows, 3);
		int cols = a.getInt(R.styleable.RadioGrid_cols, 3);
		
		// create all the buttons
		mButtons = new RadioButton[rows*cols];
		for (int i=0; i<mButtons.length; i++) {
			mButtons[i] = new RadioButton(context);
			mButtons[i].setId(i);
			mButtons[i].setOnCheckedChangeListener(this);
		}
		
		// insert into radio group
		
		// build column labels
		TableRow row;
		TextView lbl;
		
		if (lblCols != null) {
			row = new TableRow(context);
			row.setLayoutParams(new TableRow.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT));			
			
			for (int i=-1; i<cols; i++) {
				// add empty one at -1
				if (i==-1) {
					lbl = new TextView(context);
					row.addView(lbl);
					continue;
				}
				
				lbl = new TextView(context);
				
				if (i < lblCols.length)
					lbl.setText(lblCols[i].toString());
				else
					lbl.setText("?");
				
				row.addView(lbl);
			}
			
			this.addView(row);
		}
		
		// now add each row of buttons
		for (int i=0; i<rows; i++) {
			row = new TableRow(context);
			row.setLayoutParams(new TableRow.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT));
			
			lbl = new TextView(context);
			if (lblRows != null && i < lblRows.length) 
				lbl.setText(lblRows[i].toString());
			else
				lbl.setText("?");
			row.addView(lbl);
				
			for (int j=0; j<cols; j++) {
				row.addView(mButtons[i*cols+j]);
			}
			
			this.addView(row);
		}
	}

	@Override
	public void onCheckedChanged(CompoundButton button, boolean checked) {
		if (mCurrentButton != -1)
			mButtons[mCurrentButton].setChecked(false);
		
		mCurrentButton = button.getId();
	}
	
	private int mCurrentButton = -1;
	private RadioButton[] mButtons;
}
