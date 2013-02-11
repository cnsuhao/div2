#ifndef MoveObject_h
#define MoveObject_h


#include "C4Controller.h"
#include "C4Triggers.h"
#include "C4Configuration.h"
#include "C4Scripts.h"

#include "Game.h"
/**This is a basic trap that kills the player when stepped on.
*
*/
namespace C4
{
	class Variable;

	/// This is the type for the controller that we use
	enum
	{
		kMethodMoveObject = 'movo'
	};
	
	enum {
		kOriNorth  = 'nort',
		kOriSouth  = 'sout',
		kOriEast = 'east',
		kOriWest = 'west'
	};

	/**This is used to move objects verticle or horizontal
	*/
	class MoveObjectMethod : public Method	{
		private:
			MoveObjectMethod(const MoveObjectMethod& moveObjectMethod);
			Method *Replicate(void) const;
			float	travelDistance;
			int		travelDirection;
			
		
		public:
			MoveObjectMethod();
			~MoveObjectMethod();
			void Execute(const ScriptState *state);

			// Serialization functions
			unsigned long GetPackSize(unsigned long packFlags) const;
			void Pack(Packer& data, unsigned long packFlags) const;
			void Unpack(Unpacker& data, unsigned long unpackFlags);

	        // User interface functions
			long GetSettingCount(void) const;
			Setting *GetSetting(long index) const;
			void SetSetting(const Setting *setting);


	};

}


#endif