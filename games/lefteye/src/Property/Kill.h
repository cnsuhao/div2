#ifndef Kill_h
#define Kill_h

#include "C4Configuration.h"
#include "C4Collision.h"
#include "C4Sources.h"
#include "C4Properties.h"


namespace C4
{
	enum
	{
		kPropertyKiller = 'kllr'
	};

	class KillerProperty : public Property
	{
	private:
		Property *Replicate(void) const;

	public:
		KillerProperty();
		~KillerProperty();

		// Serialization functions
		void Pack(Packer& data, unsigned long packFlags) const;
		void Unpack(Unpacker& data, unsigned long unpackFlags);
	};

}
#endif