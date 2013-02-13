#include "Mover.h"
#include "C4Engine.h"
#include "Player.h"
#include "Game.h"


using namespace C4;

MoverProperty::MoverProperty() : Property(kPropertyMover)
{
}

MoverProperty::~MoverProperty()
{
}

Property *MoverProperty::Replicate(void) const
{
    return (new MoverProperty(*this));
}

void MoverProperty::Pack(Packer& data, unsigned long packFlags) const
{
    Property::Pack(data, packFlags);
}

void MoverProperty::Unpack(Unpacker& data, unsigned long unpackFlags)
{
    Property::Unpack(data, unpackFlags);
}

