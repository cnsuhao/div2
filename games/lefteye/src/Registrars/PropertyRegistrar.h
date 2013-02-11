#ifndef PropertRegistrar_h
#define PropertRegistrar_h

#include "C4Engine.h"
#include "Mover.h"
#include "Kill.h"
#include "PivotProperty.h"

namespace C4 {

	class PropertyRegistrar : public Singleton<PropertyRegistrar> {
		private:
			// Items which need to be registered here
			// Define the property registration
			PropertyReg<MoverProperty> moverPropertyReg;
			PropertyReg<KillerProperty> killerPropertyReg;
			PropertyReg<PivotProperty> pivotPropertyReg;



		public:
			PropertyRegistrar();
			~PropertyRegistrar();

			// executed on World creation
			void RegisterProperties(void);
	};

	extern PropertyRegistrar *ThePropReg;
}

#endif