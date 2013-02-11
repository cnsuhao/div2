#include "ActionRegistrar.h"

using namespace C4;

ActionRegistrar *C4::TheActReg = nullptr;

ActionRegistrar::ActionRegistrar() : Singleton<ActionRegistrar>(TheActReg) {}

ActionRegistrar::~ActionRegistrar() {
	delete upAction;
	delete downAction;
	delete leftAction;
	delete rightAction;
	delete jumpAction;
	delete sprintAction;
}

void ActionRegistrar::LoadActions() {
	// create new actions
	upAction	 = new MovementAction(kButtonUp,	 kInputUp);
	downAction	 = new MovementAction(kButtonDown,	 kInputDown);
	leftAction	 = new MovementAction(kButtonLeft,	 kInputLeft);
	rightAction  = new MovementAction(kButtonRight,	 kInputRight);
	jumpAction	 = new MovementAction(kButtonJump,	 kInputJump);
	sprintAction = new MovementAction(kButtonSprint, kInputSprint);

	// add them to the input manager
	TheInputMgr->AddAction(upAction);
	TheInputMgr->AddAction(downAction);
	TheInputMgr->AddAction(leftAction);
	TheInputMgr->AddAction(rightAction);
	TheInputMgr->AddAction(jumpAction);
	TheInputMgr->AddAction(sprintAction);
}