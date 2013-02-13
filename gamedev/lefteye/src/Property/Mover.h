#ifndef Mover_h
#define Mover_h

#include "C4Configuration.h"
#include "C4Collision.h"
#include "C4Sources.h"
#include "C4Properties.h"


namespace C4
{
	enum
	{
		kPropertyMover = 'movr'
	};

	class MoverProperty : public Property
	{
	private:
		Property *Replicate(void) const;

	public:
		MoverProperty();
		~MoverProperty();

		// Serialization functions
		void Pack(Packer& data, unsigned long packFlags) const;
		void Unpack(Unpacker& data, unsigned long unpackFlags);
	};

}
#endif