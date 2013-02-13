#include "KillTrigger.h"
#include "Game.h"

using namespace C4;


KillMethod::KillMethod() : Method(kMethodKill) {}
KillMethod::KillMethod(const KillMethod& killMethod) : Method(killMethod) {}
KillMethod::~KillMethod() {}

Method *KillMethod::Replicate(void) const{
	return (new KillMethod(*this));
}

void KillMethod::Execute(const ScriptState *state) {
		
	PlayerController* ply = TheGame->GetPlayerController();
	ply->KillPlayer();

	CallCompletionProc();
}