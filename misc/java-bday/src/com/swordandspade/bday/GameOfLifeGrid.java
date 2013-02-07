package com.swordandspade.bday;

import java.awt.*;
import java.awt.geom.*;
import java.awt.image.*;

public class GameOfLifeGrid implements BufferedImageOp {
	private final int transparent = new Color(0,0,0,0).getRGB();
	
	private volatile boolean[][] data;
	private volatile boolean[][] next;
	private int xSize;
	private int ySize;
	
	private final int cellSize = 4;
	
	private boolean simulation = false;
	private boolean remove = false;
	
	public GameOfLifeGrid(int xSize, int ySize) {
		this.data = new boolean[xSize/cellSize][ySize/cellSize];
		this.next = new boolean[xSize/cellSize][ySize/cellSize];
		this.xSize = xSize/cellSize;
		this.ySize = ySize/cellSize;
	}
	
	
	public void step() {
		if (simulation) {
			for(int y=0; y<ySize; y++) {
				for(int x=0; x<xSize; x++) {
					int n = getNeighbors(x,y);					
					
					if(data[x][y]) {
						if (n<2 || n>3) next[x][y] = false;
						else next[x][y] = true;
					}
					else {
						if (n==3) next[x][y] = true;
						else next[x][y] = false;
					}
				}
			}
			boolean[][] tmp = data;
			this.data = next;
			this.next = tmp;
		}		
	}
	
	public int getNeighbors(int x,int y) {
		int total = 0;
		for(int i=-1; i<2; i++) {
			for(int j=-1; j<2; j++) {
				if (i==0 && j==0) continue;
				int nx = x+i;
				int ny = y+j;
				
				if (nx<0) nx+=xSize;       if (ny<0) ny+=ySize;
				if (nx>=xSize)  nx-=xSize; if (ny>=ySize) ny-=ySize;
					
				if (data[nx][ny]) {
					total++;
				}
			}
		}
		return total;
	}
	
	public void setMode(int x, int y) {
		remove = data[x/cellSize][y/cellSize];
		addCell(x,y);
	}
	
	public void addCell(int x, int y) {
		x/=cellSize;
		y/=cellSize;
		if (remove) 
			data[x][y]=false;
		else 
			data[x][y]=true;
	}
	
	
	public final BufferedImage filter(BufferedImage src, BufferedImage dst) {
		if (dst==null) dst=createCompatibleDestImage(src,null);
		
		for(int y=0; y<src.getHeight(); y++) {
			for(int x=0; x<src.getWidth(); x++) {
				int srcPixel = src.getRGB(x, y);
				int c;
				if (data[x/cellSize][y/cellSize])
					c = srcPixel;
				else
					c = transparent;
				
				dst.setRGB(x, y, c);
			}
		}
		
		return dst;
	}
	
	public BufferedImage createCompatibleDestImage(BufferedImage src, ColorModel dstCM) {
		BufferedImage image;
		if (dstCM==null) dstCM = src.getColorModel();
		
		int width  = src.getWidth();
		int height = src.getHeight();
		image = new BufferedImage(dstCM,
				dstCM.createCompatibleWritableRaster(width, height),
				dstCM.isAlphaPremultiplied(), null);
		return image;
	}
	
	public final Rectangle2D getBounds2D(BufferedImage src) {
		return src.getRaster().getBounds();
	}
	
	public final Point2D getPoint2D(Point2D srcPt, Point2D dstPt) {
		if (dstPt == null) dstPt = new Point2D.Float();
		dstPt.setLocation(srcPt.getX(), srcPt.getY());
		return dstPt;
	}
	
	public void toggleSimulation() {
		simulation = !simulation;
	}
	
	public final RenderingHints getRenderingHints() { return null; }

	public boolean isRunning() {
		return simulation;
	}
	
	public void placeRandom() {
		for(int i=0; i<400; i++) {
			int x = (int)(Math.random()*xSize);
			int y = (int)(Math.random()*ySize);
			data[x][y]=true;
		}
	}
}
