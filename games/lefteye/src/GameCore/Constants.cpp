#include "C4Constants.h"
#include "Constants.h"

using namespace C4;

// Player Controller constants
float K::playerMovementSpeed	= 1.5e-4f; //walk
float K::playerSprintingMult	= 3.f;		//run speed multiplier

float K::playerJumpSpeed		= 3.e-3f;  //

float K::playerLowJump			= 7.e-3f; //
float K::playerHighJump			= 9.e-3f; //

float K::playerRotationSpeed	= .4f * K::radians;
float K::playerPivotSpeed		= .1f * K::radians;