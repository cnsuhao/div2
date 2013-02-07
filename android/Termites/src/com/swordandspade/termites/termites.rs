#pragma version(1)

#pragma rs java_package_name(com.swordandspade.termites)
#pragma stateFragment(parent)

#include "rs_core.rsh"
#include "rs_graphics.rsh"

/********************
 * GLOBALS
 *******************/
uint N_STEPS = 1;
int W;
int H;

uchar facingX[8] = { 0,  1, 1, 1, 0, -1, -1, -1};
uchar facingY[8] = {-1, -1, 0, 1, 1,  1,  0, -1};


/********************
 * STRUCTS
 *******************/
typedef struct __attribute__((packed, aligned(4)))
Termite {
	float2 position;
	uchar facing;
	uchar4 color;
	int carrying;
} Termite_t;

typedef struct __attribute__((packed, aligned(4)))
Wood {
	float2 position;
	uchar4 color;
} Wood_t;

/********************
 * POINTERS
 *******************/
Termite_t *termites;
Wood_t *wood;
rs_mesh termiteMesh;
rs_mesh woodMesh;
int *grid;


/********************
 * HELPERS
 *******************/
static int getCell(int x, int y) { return *(grid + (y * W) + x); }
static int* getCell_p(int x, int y) { return grid + (y * W) + x; }

static int getCellf2(float2 pos) { return getCell(pos.x, pos.y); }
static int* getCellf2_p(float2 pos) { return getCell_p(pos.x, pos.y); }

static int getWCell(int x, int y) {
	x = (x + W) % W;
	y = (y + H) % H;
	return *(grid + (y * W) + x);
}

static int* getWCell_p(int x, int y) {
	x = (x + W) % W;
	y = (y + H) % H;
	return grid + (y * W) + x;
}


static void turn(Termite_t *mite, int d) {
	mite->facing = (mite->facing + d + 8) % 8;
}

static void move(Termite_t *mite) {
	int x = mite->position.x + facingX[mite->facing];
	int y = mite->position.y + facingY[mite->facing];

	mite->position.x = (x + W) % W;
	mite->position.y = (y + H) % H;
}

/********************
 * SETUP
 ********************/
void init() {}

void buildTermites() {
	Termite_t *mite = termites;
	int termiteCount = rsAllocationGetDimX(rsGetAllocation(termites));
	for (int i=0; i<termiteCount; i++) {
		mite->position.x = rsRand(W);
		mite->position.y = rsRand(H);
		mite->color = rsPackColorTo8888(0, 200, 255);
		mite->carrying = -1;
		mite->facing = rsRand(8);
		mite++;
	}
}


void buildWood() {
	// clear grid
	int *cell = grid;
	for (int i=0; i < W*H; i++) {
		*cell = -1;
		cell++;
	}

	int woodCount = rsAllocationGetDimX(rsGetAllocation(wood));
	Wood_t *chip = wood;
	for (int i=0; i<woodCount; i++) {
		int x,y;
		while (true) {
			x = rsRand(W);
			y = rsRand(H);
			if (getCell(x,y) == -1)
				break;
		}

		chip->color = rsPackColorTo8888(255, 255, 0);
		chip->position.x = x;
		chip->position.y = y;

		int *cell = getCell_p(x, y);
		*cell = i;

		chip++;
	}
}



/*****************
 * MAIN
 *****************/

void update() {
	Termite_t *mite = termites;
	int termiteCount = rsAllocationGetDimX(rsGetAllocation(termites));
	for (int i=0; i<termiteCount; i++) {

		if (rsRand(10) == 0)
			turn(mite, rsRand(3) -1);
		move(mite);

		int x = mite->position.x;
		int y = mite->position.y;

		int *cell = getCell_p(x, y);
		int *front = getWCell_p(x + facingX[mite->facing], y + facingY[mite->facing]);
		if (*front != -1) {
			if (mite->carrying == -1) {
				mite->carrying = *front;
				mite->color = rsPackColorTo8888(255, 255, 255);

				Wood_t *chip = wood + *front;
				chip->position.x = -1;
				chip->position.y = -1;

				*front = -1;
			}
			else if (*cell == -1) {
				*cell = mite->carrying;

				Wood_t *chip = wood + *cell;
				chip->position = mite->position;

				mite->carrying = -1;
				mite->color = rsPackColorTo8888(0, 200, 255);
				turn(mite, 4);
			}
		}

		mite++;
	}
}

int root() {
	for (int i=0; i<N_STEPS; i++) {
		update();
	}
	rsDebug("step",0);
	rsgClearColor(0.0f, 0.0f, 0.0f, 1.0f);
	rsgDrawMesh(woodMesh);
	//rsgDrawMesh(termiteMesh);
	return 3;
}
