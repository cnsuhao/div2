#include "PivotProperty.h"
#include "C4Engine.h"
#include "Player.h"
#include "Game.h"


using namespace C4;

PivotProperty::PivotProperty() : Property(kPropertyPivot)
{
}

PivotProperty::~PivotProperty()
{
}

Property *PivotProperty::Replicate(void) const
{
    return (new PivotProperty(*this));
}

void PivotProperty::Pack(Packer& data, unsigned long packFlags) const
{
    Property::Pack(data, packFlags);
}

void PivotProperty::Unpack(Unpacker& data, unsigned long unpackFlags)
{
    Property::Unpack(data, unpackFlags);
}

