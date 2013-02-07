package com.swordandspade.bday;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;


public class GameOfLifeFrame extends JFrame {
	private static final long serialVersionUID = 1635990731305587284L;

	private GameOfLife game;
	
	public GameOfLifeFrame() {
		game = new GameOfLife();
		game.init();
		
		setLocation(100,100);
		setTitle("Game of Life");
		
		addWindowListener (new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				game.stopGame();
			}
		});
		
		Container cp = getContentPane();
		cp.add(game);
		
		setSize(game.getPreferredSize());
		
		setVisible(true);
		
		addKeyListener(game);
	}
	
	public void run() {
		game.run();
	}
	
	public static void main(String[] args) {
		new GameOfLifeFrame();
	}
}
