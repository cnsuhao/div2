package com.swordandspade.trfs;

import android.app.Activity;
import android.os.Bundle;
import android.os.Handler;

import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnKeyListener;

import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;

public class MainActivity extends Activity {
    private MultiTouchView mWebView = null;
    private Handler mHandler = new Handler();
    
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        
        mWebView = (MultiTouchView) findViewById(R.id.mWebView);
        WebSettings settings = mWebView.getSettings();
        
        // actually fill the settings
        settings.setJavaScriptEnabled(true);
        settings.setSupportZoom(false);
        
        // clear the cache to force a reload
        mWebView.clearCache(true);
        
        // this is needed for 'alert()'
        mWebView.setWebChromeClient(new MyWebChromeClient());
        
        // add js handlers
        mWebView.addJavascriptInterface(new CoreJSHandler(), "trfs");
        
        mWebView.loadUrl("file:///android_asset/index.html");
    }
    
    private void jsCall(final String cmd) {
    	mHandler.post(new Runnable() {
    		public void run() {
    			mWebView.loadUrl("javascript:"+cmd);
    		}
    	});
    }
    
    final class CoreJSHandler {
    	public boolean msg() {
    		jsCall("setText('Alec was here!')");
    		return true;
    	}
    	public void quit() {
    		finish();
    	}
    }
    
    final class MyWebChromeClient extends WebChromeClient {
    	
    }
}