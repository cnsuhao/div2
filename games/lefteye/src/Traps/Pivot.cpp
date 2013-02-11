#include "Pivot.h"
#include "Game.h"

using namespace C4;



PivotAMethod::PivotAMethod() : Method(kMethodPivotA) {}
PivotAMethod::PivotAMethod(const PivotAMethod& pivotMethod) : Method(pivotMethod) {}
PivotAMethod::~PivotAMethod() {}

Method *PivotAMethod::Replicate(void) const{
	return (new PivotAMethod(*this));
}

void PivotAMethod::Execute(const ScriptState *state) {
		
	PlayerController* ply = TheGame->GetPlayerController();
	ply->SetPivotDir(-1);
	ply->OnPivot();

	CallCompletionProc();
}



PivotBMethod::PivotBMethod() : Method(kMethodPivotB) {}
PivotBMethod::PivotBMethod(const PivotBMethod& pivotMethod) : Method(pivotMethod) {}
PivotBMethod::~PivotBMethod() {}

Method *PivotBMethod::Replicate(void) const{
	return (new PivotBMethod(*this));
}

void PivotBMethod::Execute(const ScriptState *state) {
	
	PlayerController* ply = TheGame->GetPlayerController();
	ply->SetPivotDir(1);
	ply->OnPivot();

	CallCompletionProc();
}