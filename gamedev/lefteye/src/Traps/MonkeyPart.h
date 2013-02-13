#ifndef MonkeyPart_h
#define MonkeyPart_h


#include "C4Controller.h"
#include "C4Triggers.h"
#include "C4Properties.h"
#include "C4Configuration.h"
#include "C4Sources.h"
#include "C4Scripts.h"


namespace C4
{
	
	class Variable;

	/// This is the type for the controller that we use
	enum
	{
		kMethodMonkeyPart = 'mprt'
	};
	
	class MonkeyPartMethod : public Method	{
		private:
			short		monkeyPart;
			
			MonkeyPartMethod(const MonkeyPartMethod& monkeyPartMethod);
			Method *Replicate(void) const;
			
			
		
		public:
			MonkeyPartMethod();
			~MonkeyPartMethod();
			void Execute(const ScriptState *state);

			// Serialization functions
			void Pack(Packer& data, unsigned long packFlags) const;
			void Unpack(Unpacker& data, unsigned long unpackFlags);
			bool UnpackChunk(const ChunkHeader *chunkHeader, Unpacker& data, unsigned long unpackFlags);

	        // User interface functions
			long GetSettingCount(void) const;
			Setting *GetSetting(long index) const;
			void SetSetting(const Setting *setting);


	};
	

}


#endif

