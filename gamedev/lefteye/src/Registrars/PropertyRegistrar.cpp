#include "PropertyRegistrar.h"

using namespace C4;

PropertyRegistrar *C4::ThePropReg = nullptr;


PropertyRegistrar::PropertyRegistrar() : Singleton<PropertyRegistrar>(ThePropReg),
	// all Registration initializations go here
	moverPropertyReg(kPropertyMover, "Mover Property"),
	killerPropertyReg(kPropertyKiller, "Killer Property"),
	pivotPropertyReg(kPropertyPivot, "Pivot Property")
{}

PropertyRegistrar::~PropertyRegistrar() {}

void PropertyRegistrar::RegisterProperties() {
	// any code which is needed to initialize registered properties goes here

}