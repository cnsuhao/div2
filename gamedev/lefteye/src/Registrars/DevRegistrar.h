#ifndef DevRegistrar_h
#define DevRegistrar_h

#include "C4Engine.h"
#include "C4Scripts.h"

#include "Input.h"
#include "Constants.h"

namespace C4 
{
		enum
		{
			kMethodPlayerRotation	= 'prot',
			kMethodLoadWorld = 'lwld'
		};

		enum
		{
			kMap1 = 'map1',
			kMap2 = 'map2',
			kMap3 = 'map3',
			kMap4 = 'map4',
			kMap5 = 'map5',
			kMap6 = 'map6',
			kMap7 = 'map7'
		};



	// set load world
	class LoadWorldMethod : public Method
	{
		friend class MethodReg<LoadWorldMethod>;
		private:
			long	 mapSelect;
			
			

			LoadWorldMethod();
			LoadWorldMethod(const LoadWorldMethod& loadWorldMethod);
			Method *Replicate(void) const;

			bool UnpackChunk(const ChunkHeader *chunkHeader, Unpacker& data, unsigned long unpackFlags);
		
		public:
			LoadWorldMethod(long mapSelection);
			~LoadWorldMethod();
									
			long GetNextWorld(){ return mapSelect;}
			void SetNextWorld(long mapSelection){mapSelect = mapSelection;}

			unsigned long GetPackSize(unsigned long packFlags) const;
			void Pack(Packer& data, unsigned long packFlags) const;
			void Unpack(Unpacker& data, unsigned long unpackFlags);
			
			long GetSettingCount(void) const;
			Setting *GetSetting(long index) const;
			void SetSetting(const Setting *setting);
			
			void Execute(const ScriptState *state);
	};



	class DevRegistrar : public Singleton<DevRegistrar> {
		private:
			Command			gridMoveToggle;
			Command			cameraDemoCommand;
			Command			startGameCommand;
			Command			menuCommand;
			

			SwitchAction	*stopPlayerAction;

			MethodReg<LoadWorldMethod>		loadWorldReg;

		public:
			DevRegistrar();
			~DevRegistrar();

			void LoadCommands(void);

			// The command functions
			static void gridMoveToggleCmd(const char *text);

			static void CameraDemoCommand(const char *text);
			static void StartGameCommand(const char *text);
			static void MenuCommand(const char *text);
			
	};

	extern DevRegistrar *TheDevReg;

	

}

#endif