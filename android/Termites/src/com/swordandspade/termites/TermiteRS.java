package com.swordandspade.termites;

import android.content.res.Resources;
import android.renderscript.Allocation;
import android.renderscript.Element;
import android.renderscript.Mesh;
import android.renderscript.ProgramFragmentFixedFunction;
import android.renderscript.RenderScriptGL;

public class TermiteRS {
	// TODO: most of these variables should be configurable
	public static final int TERMITES = 100;
	public static final int WOOD = 5000;
	public static final int STEPS = 1000;
	
	private int mWidth;
	private int mHeight;
	
	private RenderScriptGL mRS;
	private Resources mResources;
	private ScriptC_termites mScript; 
	
	public TermiteRS(int w, int h) {
		mWidth = w;
		mHeight = h;
	}
	
	public void init(RenderScriptGL rs, Resources res) {
		mRS = rs;
		mResources = res;
		
		createShaders();
		mScript = createScript();
		mRS.bindRootScript(mScript);
	}
	
	public void createShaders() {
		ProgramFragmentFixedFunction.Builder pfb = new ProgramFragmentFixedFunction.Builder(mRS);
        pfb.setVaryingColor(true);
        mRS.bindProgramFragment(pfb.create());
	}
	
	public ScriptC_termites createScript() {
		// calculate preference values
		int termiteCount = TERMITES;
		int woodCount = WOOD;
		
		// create termite mesh
		ScriptField_Termite termites = new ScriptField_Termite(mRS, termiteCount);
		Mesh.AllocationBuilder termMB = new Mesh.AllocationBuilder(mRS);
		termMB.addVertexAllocation(termites.getAllocation());
		termMB.addIndexSetType(Mesh.Primitive.POINT);
		Mesh termMesh = termMB.create();
		
		// create wood mesh
		ScriptField_Wood wood = new ScriptField_Wood(mRS, woodCount);
		Mesh.AllocationBuilder woodMB = new Mesh.AllocationBuilder(mRS);
		woodMB.addVertexAllocation(wood.getAllocation());
		woodMB.addIndexSetType(Mesh.Primitive.POINT);
		Mesh woodMesh = woodMB.create();
		
		
		// setup script with pointers
		ScriptC_termites script = new ScriptC_termites(mRS, mResources, R.raw.termites);
		
		script.set_termiteMesh(termMesh);
		script.bind_termites(termites);
		
		script.set_woodMesh(woodMesh);
		script.bind_wood(wood);
		
		// create the grid
		script.bind_grid(Allocation.createSized(mRS, Element.I32(mRS), mWidth * mHeight));

		// set other variables
		script.set_W(mWidth);
		script.set_H(mHeight);
		script.set_N_STEPS(STEPS);
		
		// run through build
		script.invoke_buildTermites();
		script.invoke_buildWood();

		return script;
	}

}
