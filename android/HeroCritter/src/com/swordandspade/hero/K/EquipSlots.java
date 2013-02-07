package com.swordandspade.hero.K;

// to reconsider and adjust as needed

public class EquipSlots {	
	// weapons
	public static final int LH = 1<<0;   // left equipment
	public static final int RH = 1<<1;   // right equipment
	public static final int AMMO = 1<<2; // ammo
	
	// hand
	public static final int PLM = 1<<0; // palm
	public static final int THM = 1<<1; // thumb
	public static final int FNG_CNT = 8; // two spots per finger
	public static final int FNGS = ((1<<FNG_CNT) - 1) << 2;
	public static final int HND = PLM|THM|FNGS;
		
	// head
	public static final int HD = 3;  // head
	public static final int NCK = 4;  // necklace
	public static final int FC  = 4;  // face
	public static final int ER = 5; // earing
	public static final int NS = 3;  // nose
	public static final int EYE = 3;  // eyes
	
	// body
	public static final int SHR = 4;  // shirt
	public static final int JKT = 8;  // overshirt
	public static final int CLK = 11; // cloak
	
	// arms
	public static final int UARM = 5;  // upper-arm
	public static final int FARM = 6;  // forearm
	
	// legs
	public static final int THG = 9;  // thighs
	public static final int SHN = 10; // shins
	public static final int BTS = 11; // boots
}