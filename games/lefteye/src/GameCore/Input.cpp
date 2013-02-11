#include "Input.h"
#include "Player.h"
#include "Game.h"

using namespace C4;

MovementAction::MovementAction(unsigned long type, unsigned long flag) : Action(type) {
	inputFlag = flag;
}

MovementAction::~MovementAction() {}

void MovementAction::Begin(void) {
	PlayerController *controller = TheGame->GetPlayerController();
	if (controller) {
	//	if(inputFlag == kInputLeft) 
	//		controller->SetInputFlags((controller->GetInputFlags() | kInputLeft) & ~kInputRight);
	//	else if (inputFlag == kInputRight) 
	//		controller->SetInputFlags((controller->GetInputFlags() | kInputRight) & ~kInputLeft);
	//	else
			controller->SetInputFlags(controller->GetInputFlags() | inputFlag);
	}
}

void MovementAction::End(void) {
	PlayerController *controller = TheGame->GetPlayerController();
	if (controller) controller->SetInputFlags(controller->GetInputFlags() & ~inputFlag);
}


SwitchAction::SwitchAction(unsigned long type) : Action(type) {}
SwitchAction::~SwitchAction() {}

void SwitchAction::Begin(void) {
	PlayerController* sc = TheGame->GetPlayerController();

	switch (GetActionType()) {
		case kActionRotate:
			sc->SetPlayerAzimuth(sc->GetPlayerAzimuth() + K::pi_over_2);
			break;
		case kActionRotateLeft:
			sc->SetPivotDir(1);
			sc->Pivot();
			break;
		case kActionRotateRight:
			sc->SetPivotDir(-1);
			sc->Pivot();
			break;
		case kActionStopPlayer:
			sc->StopPlayer();
			break;
	}
}