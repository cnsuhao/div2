#include "Kill.h"
#include "C4Engine.h"
#include "Player.h"
#include "Game.h"


using namespace C4;

KillerProperty::KillerProperty() : Property(kPropertyKiller)
{
}

KillerProperty::~KillerProperty()
{
}

Property *KillerProperty::Replicate(void) const
{
    return (new KillerProperty(*this));
}

void KillerProperty::Pack(Packer& data, unsigned long packFlags) const
{
    Property::Pack(data, packFlags);
}

void KillerProperty::Unpack(Unpacker& data, unsigned long unpackFlags)
{
    Property::Unpack(data, unpackFlags);
}

