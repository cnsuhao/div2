#ifndef PivotProperty_h
#define PivotProperty_h

#include "C4Configuration.h"
#include "C4Collision.h"
#include "C4Sources.h"
#include "C4Properties.h"


namespace C4
{
	enum
	{
		kPropertyPivot = 'pivt'
	};

	class PivotProperty : public Property
	{
	private:
		Property *Replicate(void) const;

	public:
		PivotProperty();
		~PivotProperty();

		// Serialization functions
		void Pack(Packer& data, unsigned long packFlags) const;
		void Unpack(Unpacker& data, unsigned long unpackFlags);
	};

}
#endif