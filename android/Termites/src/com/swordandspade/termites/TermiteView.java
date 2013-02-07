package com.swordandspade.termites;

import android.content.Context;
import android.renderscript.RSSurfaceView;
import android.renderscript.RenderScriptGL;
import android.view.SurfaceHolder;

public class TermiteView extends RSSurfaceView {

	private RenderScriptGL mRSGL;
	private TermiteRS mRender;
	
	public TermiteView(Context context) {
		super(context);
	}
	
	@Override
	public void surfaceChanged(SurfaceHolder holder, int format, int w, int h) {
		super.surfaceChanged(holder, format, w, h); 
		
		if (mRSGL == null) {
			RenderScriptGL.SurfaceConfig sc = new RenderScriptGL.SurfaceConfig();
			mRSGL = createRenderScriptGL(sc); 
			mRSGL.setSurface(holder, w, h); 
			mRender = new TermiteRS(w, h);
			mRender.init(mRSGL, getResources()); 
		}
	}

}
