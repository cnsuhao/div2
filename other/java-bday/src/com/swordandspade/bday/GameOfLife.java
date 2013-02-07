package com.swordandspade.bday;

import java.awt.*;
import java.awt.image.*;
import java.awt.event.*;

import javax.swing.*;

public class GameOfLife extends JPanel implements Runnable, MouseListener, MouseMotionListener, KeyListener {
	private static final long serialVersionUID = 1L;
	private static final Color transparent = new Color(0,0,0,0);
	private static final Color messageColor = new Color(255,255,255,40); // use alpha to determine how many frames it takes to appear
	private static final Color fadeColor = new Color(0,0,0,180);
	private static final Color cellColor = Color.green;
	
	private static final int PWIDTH  = 1000;
	private static final int PHEIGHT = 212;
	
	private Thread animator;
	private volatile boolean running = false;

	private BufferedImage cellColorImg;
	private BufferedImage message;
	
	private BufferedImage unlocked;
	private Graphics2D ug;
	
	private BufferedImage cells;
	private Graphics2D cg;
	
	private BufferedImage dbImage;
	private Graphics2D dbg;
	
	private GameOfLifeGrid grid;
	
	private BufferedImage buff;
	
	private int drawMode=-1;
	/*
	 * minus mouse down...
	 * 
	 * images:
	 * 		message
	 * 		messageUnlocked
	 * 		green
	 * 		background (green+fades)
	 * 
	 * 		mask
	 * 
	 * for update:
	 * 		pull in old array
	 * 		run against life pattern
	 * 		generate mask
	 * 
	 * 
	 */
	
	
	public void init() {
		setBackground(Color.black);
		setPreferredSize(new Dimension(PWIDTH,PHEIGHT));
		grid = new GameOfLifeGrid(PWIDTH,PHEIGHT);
		
		cellColorImg = new BufferedImage(PWIDTH,PHEIGHT,BufferedImage.TYPE_4BYTE_ABGR);
		Graphics2D g = (Graphics2D)cellColorImg.getGraphics();
		g.setColor(cellColor);
		g.fillRect(0, 0, PWIDTH, PHEIGHT);
		
		message = new BufferedImage(PWIDTH,PHEIGHT,BufferedImage.TYPE_4BYTE_ABGR);
		g = (Graphics2D)message.getGraphics();
		g.setColor(transparent);
		g.fillRect(0, 0, PWIDTH, PHEIGHT);
		g.setColor(messageColor);
		Font font = new Font("serif", Font.BOLD, 72);
		Font font2 = new Font("serif", Font.ITALIC, 56);
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		g.setFont(font);
		g.drawString("Happy 18th Birthday!", 90, 80);
		g.setFont(font2);
		g.drawString("-Love Alec", 550, 150);
		
		cells = new BufferedImage(PWIDTH,PHEIGHT,BufferedImage.TYPE_4BYTE_ABGR);
		cg = (Graphics2D)cells.getGraphics();
		cg.setColor(Color.black);
		cg.fillRect(0, 0, PWIDTH, PHEIGHT);
		
		unlocked = new BufferedImage(PWIDTH,PHEIGHT,BufferedImage.TYPE_4BYTE_ABGR);
		ug = (Graphics2D)unlocked.getGraphics();
		
		dbImage = new BufferedImage(PWIDTH,PHEIGHT,BufferedImage.TYPE_4BYTE_ABGR);
		dbg = (Graphics2D)dbImage.getGraphics();
		
		buff = new BufferedImage(PWIDTH,PHEIGHT,BufferedImage.TYPE_4BYTE_ABGR);
		
		addMouseListener(this);
		addMouseMotionListener(this);
		addKeyListener(this);		
	}
	
	public void changeDrawMode() {
		drawMode+=1;
		drawMode%=3;
	}
	
	public void addNotify() {
		super.addNotify();
		startGame();
	}

	public void startGame() {
		if (animator==null || !running) {
			animator = new Thread(this);
			animator.start();
		}
	}
	
	public void stopGame() {
		running = false;
	}
	
	public void gameUpdate() {
		grid.step();
	}
	
	public void gameRender() {	
		// overlay frame on previous cells
		if (drawMode>=0) {
			dbg.setColor(Color.black);
			dbg.fillRect(0, 0, PWIDTH, PHEIGHT);
			if ((drawMode+1)%2==1) {
				cg.setColor(fadeColor);
				cg.fillRect(0, 0, PWIDTH, PHEIGHT);
				BufferedImage living = grid.filter(cellColorImg, buff);
				cg.drawImage(living, 0, 0, null);
				dbg.drawImage(cells, 0, 0, null);
			}
		
			if (grid.isRunning() && drawMode<2) {
				BufferedImage messagePts = grid.filter(message, buff);
				ug.drawImage(messagePts, 0, 0, null);
				dbg.drawImage(unlocked, 0, 0, null);
			}
		}
		else {
			Font font = new Font("serif", Font.BOLD, 24);
			dbg.setColor(Color.white);
			dbg.setFont(font);
			dbg.drawString("Try to find the hidden message!", 20, 30);
			font = new Font("serif", Font.PLAIN, 16);
			dbg.setFont(font);
			dbg.drawString("To turn on a cell, click/drag the mouse", 20, 70);
			dbg.drawString("If you click on an active cell, dragging will turn them off", 20, 90);
			dbg.drawString("The spacebar toggles the simulation on and off", 20, 110);
			dbg.drawString("Backspace cycles through drawing all/just the cells/just the message", 20, 130);
			dbg.drawString("Press R to add 400 random cells", 20, 150);
			dbg.drawString("Click or press any key to begin, Press Escape to Quit", 20, 170);
		}
	}
	
	public void run() {
		running = true;
		
		while(running) {
			gameUpdate();
			gameRender();
			repaint();
			try {
				Thread.sleep(40);
			}
			catch(InterruptedException ex) {}
		}
		System.exit(0);
	}
	
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		Graphics2D g2 = (Graphics2D)g;
		if (dbImage!=null)
			g2.drawImage(dbImage,0,0,null);
		else 
			System.out.println("no db");
	}

	public void mouseDragged(MouseEvent evt) {
		grid.addCell(evt.getX(), evt.getY());
	}
	
	public void mousePressed(MouseEvent evt) {
		if (drawMode==-1) changeDrawMode();
		grid.setMode(evt.getX(), evt.getY());
	}
	
	public void keyPressed(KeyEvent evt) {
		if(evt.getKeyCode()==KeyEvent.VK_ESCAPE)
			running = false;
		if (drawMode==-1) changeDrawMode();
		if(evt.getKeyCode()==KeyEvent.VK_SPACE)
			grid.toggleSimulation();
		else if(evt.getKeyCode()==KeyEvent.VK_BACK_SPACE)
			changeDrawMode();
		else if(evt.getKeyCode()==KeyEvent.VK_R)
			grid.placeRandom();
	}
	
	public void mouseMoved(MouseEvent evt) {}
	public void mouseClicked(MouseEvent evt) {}
	public void mouseEntered(MouseEvent evt) {}
	public void mouseExited(MouseEvent evt) {}
	public void mouseReleased(MouseEvent evt) {}
	public void keyReleased(KeyEvent evt) {}
	public void keyTyped(KeyEvent evt) {}
}
