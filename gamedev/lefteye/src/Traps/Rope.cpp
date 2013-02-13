#include "Rope.h"
#include "Game.h"

using namespace C4;


RopeMethod::RopeMethod() : Method(kMethodRope) {}
RopeMethod::RopeMethod(const RopeMethod& RopeMethod) : Method(RopeMethod) {}
RopeMethod::~RopeMethod() {}

Method *RopeMethod::Replicate(void) const{
	return (new RopeMethod(*this));
}

void RopeMethod::Execute(const ScriptState *state) {
	
	PlayerController* ply = TheGame->GetPlayerController();
	ply->GrabRope();

	CallCompletionProc();
}