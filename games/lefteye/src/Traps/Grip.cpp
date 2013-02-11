#include "Grip.h"
#include "Game.h"

using namespace C4;



GripMethod::GripMethod() : Method(kMethodGrip) {}
GripMethod::GripMethod(const GripMethod& grip) : Method(grip) {}
GripMethod::~GripMethod() {}

Method *GripMethod::Replicate(void) const{
	return (new GripMethod(*this));
}

void GripMethod::Execute(const ScriptState *state) {
		
	PlayerController* ply = TheGame->GetPlayerController();
	ply->Grip();

	CallCompletionProc();
}