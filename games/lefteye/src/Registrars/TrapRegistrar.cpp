#include "TrapRegistrar.h"

using namespace C4;

TrapRegistrar *C4::TheTrapReg = nullptr;


TrapRegistrar::TrapRegistrar() : Singleton<TrapRegistrar>(TheTrapReg),
	// all Registration initializations go here
	coinObjectReg(kMethodCollectCoin, "Collect Coin"),
	monkeyObjectReg(kMethodMonkeyPart, "Collect Monkey Part"),

	pivotAReg(kMethodPivotA, "Pivot A"),
	pivotBReg(kMethodPivotB, "Pivot B"),
	gripReg(kMethodGrip, "Grip"),
	ropeReg(kMethodRope, "Rope"),

	killReg(kMethodKill, "Kill Player"),

	killMoverControllerReg(kControllerKillMover, "Kill Mover"),
	rotatorControllerReg(kControllerRotator, "Rotator"),
	spinControllerReg(kControllerSpin, "Spin")
	//coinControllerReg(kControllerCoin, "Coin")
{}

TrapRegistrar::~TrapRegistrar() {}

void TrapRegistrar::RegisterTraps() {
	// any code which is needed to initialize registered traps goes here

}